package org.bitcoins.core.psbt

import org.bitcoins.core.byteVectorOrdering
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

sealed trait PSBTMap[+RecordType <: PSBTRecord] extends NetworkElement {
  require(elements.map(_.key).groupBy(identity).values.forall(_.length == 1),
          s"All keys must be unique. Got: $elements")

  def elements: Vector[RecordType]

  def bytes: ByteVector =
    elements
      .sortBy(_.key)
      .foldLeft(ByteVector.empty)(_ ++ _.bytes) :+ PSBTMap.separatorByte

  protected def getRecords[KeyIdType <: PSBTKeyId](
      key: KeyIdType,
      keyIdFactory: PSBTKeyIdFactory[KeyIdType]): Vector[key.RecordType] = {
    elements
      .filter(element => keyIdFactory.fromByte(element.key.head) == key)
      .asInstanceOf[Vector[key.RecordType]]
  }

  protected def filterRecords[KeyIdType <: PSBTKeyId](
      key: KeyIdType,
      keyIdFactory: PSBTKeyIdFactory[KeyIdType]): Vector[RecordType] = {
    elements
      .filterNot(element => keyIdFactory.fromByte(element.key.head) == key)
  }

  protected def distinctByKey[rType <: PSBTRecord](
      records: Vector[rType]): Vector[rType] = {
    records.groupBy(_.key).map(_._2.head).toVector
  }
}

object PSBTMap {
  final val separatorByte: Byte = 0x00.byteValue
}

sealed trait PSBTMapFactory[
    RecordType <: PSBTRecord,
    MapType <: PSBTMap[RecordType]]
    extends Factory[MapType] {
  def recordFactory: Factory[RecordType]

  def constructMap(elements: Vector[RecordType]): MapType

  lazy val empty: MapType = constructMap(Vector.empty)

  override def fromBytes(bytes: ByteVector): MapType = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: Vector[RecordType]): Vector[RecordType] = {
      if (remainingBytes.head == PSBTMap.separatorByte) {
        accum
      } else {
        val record = recordFactory.fromBytes(remainingBytes)
        val next = remainingBytes.drop(record.bytes.size)

        loop(next, accum :+ record)
      }
    }

    constructMap(loop(bytes, Vector.empty))
  }
}

case class GlobalPSBTMap(elements: Vector[GlobalPSBTRecord])
    extends SeqWrapper[GlobalPSBTRecord]
    with PSBTMap[GlobalPSBTRecord] {
  import org.bitcoins.core.psbt.GlobalPSBTRecord._
  import org.bitcoins.core.psbt.PSBTGlobalKeyId._
  require(getRecords(UnsignedTransactionKeyId).nonEmpty,
          "A GlobalPSBTMap must have a Unsigned Transaction")
  override val wrapped: Vector[GlobalPSBTRecord] = elements

  def unsignedTransaction: UnsignedTransaction = {
    getRecords(UnsignedTransactionKeyId).head
  }

  def extendedPublicKeys: Vector[XPubKey] = {
    getRecords(XPubKeyKeyId)
  }

  def version: Version = {
    getRecords(VersionKeyId).headOption.getOrElse(Version(UInt32.zero))
  }

  def getRecords(key: PSBTGlobalKeyId): Vector[key.RecordType] = {
    super.getRecords(key, PSBTGlobalKeyId)
  }

  def filterRecords(key: PSBTGlobalKeyId): Vector[GlobalPSBTRecord] = {
    super.filterRecords(key, PSBTGlobalKeyId)
  }

  /**
    * Takes another GlobalPSBTMap and adds all records that are not contained in this GlobalPSBTMap
    * @param other GlobalPSBTMap to be combined with
    * @return A GlobalPSBTMap with the combined data of the two GlobalPSBTMaps
    */
  def combine(other: GlobalPSBTMap): GlobalPSBTMap = {
    require(
      this
        .getRecords(UnsignedTransactionKeyId)
        .head == other.getRecords(UnsignedTransactionKeyId).head,
      "Cannot combine GlobalPSBTMaps with different unsigned transactions"
    )
    // We must keep the highest version number
    // https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#version-numbers
    if (this.version.version > other.version.version) {
      val newElements = distinctByKey(
        this.elements ++ other.filterRecords(VersionKeyId))
      GlobalPSBTMap(newElements)
    } else if (this.version.version < other.version.version) {
      val newElements = distinctByKey(
        this.filterRecords(VersionKeyId) ++ other.elements)
      GlobalPSBTMap(newElements)
    } else {
      val newElements = distinctByKey(this.elements ++ other.elements)
      GlobalPSBTMap(newElements)
    }
  }
}

object GlobalPSBTMap extends PSBTMapFactory[GlobalPSBTRecord, GlobalPSBTMap] {

  override def recordFactory: Factory[GlobalPSBTRecord] = GlobalPSBTRecord

  override def constructMap(elements: Vector[GlobalPSBTRecord]): GlobalPSBTMap =
    GlobalPSBTMap(elements)
}

case class InputPSBTMap(elements: Vector[InputPSBTRecord])
    extends SeqWrapper[InputPSBTRecord]
    with PSBTMap[InputPSBTRecord] {
  override protected val wrapped: Vector[InputPSBTRecord] = elements

  import org.bitcoins.core.psbt.InputPSBTRecord._
  import org.bitcoins.core.psbt.PSBTInputKeyId._

  def nonWitnessOrUnknownUTXOOpt: Option[NonWitnessOrUnknownUTXO] = {
    getRecords(NonWitnessUTXOKeyId).headOption
  }

  def witnessUTXOOpt: Option[WitnessUTXO] = {
    getRecords(WitnessUTXOKeyId).headOption
  }

  def partialSignatures: Vector[PartialSignature] = {
    getRecords(PartialSignatureKeyId)
  }

  def sigHashTypeOpt: Option[SigHashType] = {
    getRecords(SigHashTypeKeyId).headOption
  }

  def redeemScriptOpt: Option[RedeemScript] = {
    getRecords(RedeemScriptKeyId).headOption
  }

  def witnessScriptOpt: Option[WitnessScript] = {
    getRecords(WitnessScriptKeyId).headOption
  }

  def BIP32DerivationPaths: Vector[BIP32DerivationPath] = {
    getRecords(BIP32DerivationPathKeyId)
  }

  def finalizedScriptSigOpt: Option[FinalizedScriptSig] = {
    getRecords(FinalizedScriptSigKeyId).headOption
  }

  def finalizedScriptWitnessOpt: Option[FinalizedScriptWitness] = {
    getRecords(FinalizedScriptWitnessKeyId).headOption
  }

  def proofOfReservesCommitmentOpt: Option[ProofOfReservesCommitment] = {
    getRecords(ProofOfReservesCommitmentKeyId).headOption
  }

  def getRecords(key: PSBTInputKeyId): Vector[key.RecordType] = {
    super.getRecords(key, PSBTInputKeyId)
  }

  def filterRecords(key: PSBTInputKeyId): Vector[InputPSBTRecord] = {
    super.filterRecords(key, PSBTInputKeyId)
  }

  def isFinalized: Boolean =
    getRecords(FinalizedScriptSigKeyId).nonEmpty || getRecords(
      FinalizedScriptWitnessKeyId).nonEmpty

  def isBIP143Vulnerable: Boolean = {
    if (
      !isFinalized && witnessUTXOOpt.isDefined && nonWitnessOrUnknownUTXOOpt.isEmpty
    ) {

      val isNativeV0 = witnessUTXOOpt.get.witnessUTXO.scriptPubKey
        .isInstanceOf[WitnessScriptPubKeyV0]

      val isP2SHV0 =
        redeemScriptOpt.isDefined && redeemScriptOpt.get.redeemScript
          .isInstanceOf[WitnessScriptPubKeyV0]

      // Only unsafe if it doesn't already have signatures
      (isNativeV0 || isP2SHV0) && partialSignatures.isEmpty
    } else false
  }

  /** Finalizes this input if possible, returning a Failure if not */
  def finalize(input: TransactionInput): Try[InputPSBTMap] = {
    if (isFinalized) {
      Success(this)
    } else {
      val scriptPubKeyToSatisfyT: Try[ScriptPubKey] = {
        val witnessUTXOOpt =
          getRecords(WitnessUTXOKeyId).headOption
        val nonWitnessUTXOOpt =
          getRecords(NonWitnessUTXOKeyId).headOption

        (witnessUTXOOpt, nonWitnessUTXOOpt) match {
          case (None, None) =>
            Failure(
              new IllegalStateException(
                s"Cannot finalize without UTXO record: $this"))
          case (Some(witnessUtxo), _) =>
            Success(witnessUtxo.witnessUTXO.scriptPubKey)
          case (_, Some(utxo)) =>
            val outputs = utxo.transactionSpent.outputs
            Try(outputs(input.previousOutput.vout.toInt).scriptPubKey)
        }
      }

      scriptPubKeyToSatisfyT.flatMap(finalize)
    }
  }

  /** Finalizes this input if possible, returning None if not */
  private def finalize(spkToSatisfy: ScriptPubKey): Try[InputPSBTMap] = {

    /** Removes non-utxo and non-unkown records, replacing them with finalized records */
    def wipeAndAdd(
        scriptSig: ScriptSignature,
        witnessOpt: Option[ScriptWitness] = None): InputPSBTMap = {
      val utxos =
        getRecords(WitnessUTXOKeyId) ++ getRecords(NonWitnessUTXOKeyId)
      val unknowns =
        elements.filter(_.isInstanceOf[InputPSBTRecord.Unknown])

      val finalizedScriptSig = FinalizedScriptSig(scriptSig)
      val recordsToAdd = witnessOpt match {
        case None => Vector(finalizedScriptSig)
        case Some(scriptWitness) =>
          Vector(finalizedScriptSig, FinalizedScriptWitness(scriptWitness))
      }

      val records = utxos ++ recordsToAdd ++ unknowns
      InputPSBTMap(records)
    }

    /** Turns the required PartialSignatures into a ScriptSignature and calls wipeAndAdd
      * @return None if the requirement is not met
      */
    def collectSigs(
        required: Int,
        constructScriptSig: Seq[PartialSignature] => ScriptSignature): Try[
      InputPSBTMap] = {
      val sigs = getRecords(PartialSignatureKeyId)
      if (sigs.length != required) {
        Failure(new IllegalArgumentException(
          s"Could not collect $required signatures when only the following were present: $sigs"))
      } else {
        val scriptSig = constructScriptSig(
          sigs.map(sig => PartialSignature(sig.pubKey, sig.signature)))

        val newInputMap = wipeAndAdd(scriptSig)

        Success(newInputMap)
      }
    }

    def toTry[T](opt: Option[T], reason: String): Try[T] = {
      opt match {
        case Some(elem) => Success(elem)
        case None =>
          Failure(
            new IllegalStateException(
              s"Cannot finalize the following input because $reason: $this"))
      }
    }

    spkToSatisfy match {
      case _: P2SHScriptPubKey =>
        val redeemScriptOpt =
          getRecords(RedeemScriptKeyId).headOption
        toTry(redeemScriptOpt, "there is no redeem script record").flatMap {
          case RedeemScript(redeemScript) =>
            finalize(redeemScript).map { inputMap =>
              val nestedScriptSig = inputMap
                .getRecords(FinalizedScriptSigKeyId)
                .head
              val witnessOpt = inputMap
                .getRecords(FinalizedScriptWitnessKeyId)
                .headOption

              val scriptSig =
                P2SHScriptSignature(nestedScriptSig.scriptSig, redeemScript)
              wipeAndAdd(scriptSig, witnessOpt.map(_.scriptWitness))
            }
        }
      case _: P2WSHWitnessSPKV0 =>
        val redeemScriptOpt =
          getRecords(WitnessScriptKeyId).headOption
        toTry(redeemScriptOpt, "there is no witness script record").flatMap {
          case WitnessScript(redeemScript) =>
            finalize(redeemScript).map { inputMap =>
              val nestedScriptSig = inputMap
                .getRecords(FinalizedScriptSigKeyId)
                .head
              val scriptSig = EmptyScriptSignature
              wipeAndAdd(
                scriptSig,
                Some(P2WSHWitnessV0(redeemScript, nestedScriptSig.scriptSig)))
            }
        }
      case _: P2WPKHWitnessSPKV0 =>
        val sigOpt =
          getRecords(PartialSignatureKeyId).headOption
        toTry(sigOpt, "there is no partial signature record").map { sig =>
          val witness = P2WPKHWitnessV0(sig.pubKey, sig.signature)
          val scriptSig = EmptyScriptSignature
          wipeAndAdd(scriptSig, Some(witness))
        }
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        val sigOpt =
          getRecords(PartialSignatureKeyId).headOption
        toTry(sigOpt, "there is no partial signature record").flatMap { sig =>
          if (sig.pubKey == p2pkWithTimeout.pubKey) {
            val scriptSig = P2PKWithTimeoutScriptSignature(beforeTimeout = true,
                                                           signature =
                                                             sig.signature)
            Success(wipeAndAdd(scriptSig, None))
          } else if (sig.pubKey == p2pkWithTimeout.timeoutPubKey) {
            val scriptSig =
              P2PKWithTimeoutScriptSignature(beforeTimeout = false,
                                             signature = sig.signature)
            Success(wipeAndAdd(scriptSig, None))
          } else {
            Failure(new IllegalArgumentException(
              s"Cannot finalize the following input because the signature provided ($sig) signs for neither key in $p2pkWithTimeout: $this"))
          }
        }
      case conditional: ConditionalScriptPubKey =>
        val builder =
          Vector.newBuilder[(
              ConditionalPath,
              Vector[Sha256Hash160Digest],
              RawScriptPubKey)]

        /** Traverses the ConditionalScriptPubKey tree for leaves and adds them to builder */
        def addLeaves(rawSPK: RawScriptPubKey, path: Vector[Boolean]): Unit = {
          rawSPK match {
            case conditional: ConditionalScriptPubKey =>
              addLeaves(conditional.trueSPK, path :+ true)
              addLeaves(conditional.falseSPK, path :+ false)
            case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
              addLeaves(P2PKScriptPubKey(p2pkWithTimeout.pubKey), path :+ true)
              val timeoutSPK = CLTVScriptPubKey(
                p2pkWithTimeout.lockTime,
                P2PKScriptPubKey(p2pkWithTimeout.timeoutPubKey))
              addLeaves(timeoutSPK, path :+ false)
            case cltv: CLTVScriptPubKey =>
              addLeaves(cltv.nestedScriptPubKey, path)
            case csv: CSVScriptPubKey =>
              addLeaves(csv.nestedScriptPubKey, path)
            case p2pkh: P2PKHScriptPubKey =>
              builder += ((ConditionalPath.fromBranch(path),
                           Vector(p2pkh.pubKeyHash),
                           p2pkh))
            case p2pk: P2PKScriptPubKey =>
              val hash = CryptoUtil.sha256Hash160(p2pk.publicKey.bytes)
              builder += ((ConditionalPath.fromBranch(path),
                           Vector(hash),
                           p2pk))
            case multiSig: MultiSignatureScriptPubKey =>
              // If no sigs are required we handle in a special way below
              if (multiSig.requiredSigs == 0) {
                builder += ((ConditionalPath.fromBranch(path),
                             Vector.empty,
                             multiSig))
              } else {
                val hashes = multiSig.publicKeys.toVector.map(pubKey =>
                  CryptoUtil.sha256Hash160(pubKey.bytes))
                builder += ((ConditionalPath.fromBranch(path),
                             hashes,
                             multiSig))
              }
            case EmptyScriptPubKey =>
              builder += ((ConditionalPath.fromBranch(path),
                           Vector.empty,
                           EmptyScriptPubKey))
            case _: NonStandardScriptPubKey | _: WitnessCommitment =>
              throw new UnsupportedOperationException(
                s"$rawSPK is not yet supported")
          }
        }

        // Find the conditional leaf with the pubkeys for which sigs are provided
        // Hashes are used since we only have the pubkey hash in the p2pkh case
        val sigs = getRecords(PartialSignatureKeyId)
        val hashes = sigs.map(sig => CryptoUtil.sha256Hash160(sig.pubKey.bytes))
        addLeaves(conditional, Vector.empty)
        val leaves = builder.result()

        val leafOpt = if (hashes.isEmpty) {
          leaves.find(leaf => leaf._2.isEmpty)
        } else {
          leaves.find(leaf => hashes.forall(leaf._2.contains))
        }

        val leafT = toTry(
          leafOpt,
          s"no conditional branch using $sigs in $conditional could be found")

        leafT.flatMap {
          case (path, _, spk) =>
            val finalizedOpt = finalize(spk)
            finalizedOpt.map { finalized =>
              val nestedScriptSig = finalized
                .getRecords(FinalizedScriptSigKeyId)
                .head
              val scriptSig =
                ConditionalScriptSignature.fromNestedScriptSig(
                  nestedScriptSig.scriptSig,
                  path)
              wipeAndAdd(scriptSig)
            }
        }
      case locktime: LockTimeScriptPubKey =>
        finalize(locktime.nestedScriptPubKey)
      case _: P2PKHScriptPubKey =>
        collectSigs(
          required = 1,
          sigs => P2PKHScriptSignature(sigs.head.signature, sigs.head.pubKey))
      case _: P2PKScriptPubKey =>
        collectSigs(required = 1,
                    sigs => P2PKScriptSignature(sigs.head.signature))
      case multiSig: MultiSignatureScriptPubKey =>
        def generateScriptSig(
            sigs: Seq[PartialSignature]): MultiSignatureScriptSignature = {
          val sortedSigs = sigs
            .map { partialSig =>
              (partialSig.signature,
               multiSig.publicKeys.indexOf(partialSig.pubKey))
            }
            .sortBy(_._2)
            .map(_._1)

          MultiSignatureScriptSignature(sortedSigs)
        }

        collectSigs(required = multiSig.requiredSigs, generateScriptSig)
      case EmptyScriptPubKey =>
        val scriptSig = TrivialTrueScriptSignature
        Success(wipeAndAdd(scriptSig))
      case _: NonStandardScriptPubKey | _: UnassignedWitnessScriptPubKey |
          _: WitnessCommitment =>
        Failure(
          new UnsupportedOperationException(
            s"$spkToSatisfy is not yet supported"))
    }
  }

  /**
    * Takes another InputPSBTMap and adds all records that are not contained in this InputPSBTMap
    * A record's distinctness is determined by its key
    * @param other InputPSBTMap to be combined with
    * @return A InputPSBTMap with the combined data of the two InputPSBTMaps
    */
  def combine(other: InputPSBTMap): InputPSBTMap = {
    InputPSBTMap(distinctByKey(this.elements ++ other.elements))
  }

  /**
    * Takes the InputPSBTMap returns a NewSpendingInfoFull
    * that can be used to sign the input
    * @param txIn The transaction input that this InputPSBTMap represents
    * @param signers Signers that will be used to sign the input
    * @param conditionalPath Path that should be used for the script
    * @return A corresponding NewSpendingInfoFull
    */
  def toUTXOSatisfyingInfoUsingSigners(
      txIn: TransactionInput,
      signers: Vector[Sign],
      conditionalPath: ConditionalPath =
        ConditionalPath.NoCondition): ScriptSignatureParams[InputInfo] = {
    require(!isFinalized, s"Cannot update an InputPSBTMap that is finalized")

    val infoSingle =
      toUTXOSigningInfo(txIn, signers.head, conditionalPath)

    ScriptSignatureParams(
      infoSingle.inputInfo,
      infoSingle.prevTransaction,
      signers,
      infoSingle.hashType
    )
  }

  def toInputInfo(
      txIn: TransactionInput,
      conditionalPath: ConditionalPath = ConditionalPath.NoCondition,
      preImages: Vector[NetworkElement] = Vector.empty): InputInfo = {
    if (isFinalized)
      toInputInfoFinalized(txIn, conditionalPath, preImages)
    else toInputInfoNonFinalized(txIn, conditionalPath, preImages)
  }

  def toInputInfoFinalized(
      txIn: TransactionInput,
      conditionalPath: ConditionalPath,
      preImages: Vector[NetworkElement]): InputInfo = {
    val outPoint = txIn.previousOutput

    val witVec = getRecords(WitnessUTXOKeyId)
    val txVec = getRecords(NonWitnessUTXOKeyId)

    val output = if (witVec.size == 1) {
      witVec.head.witnessUTXO
    } else if (txVec.size == 1) {
      val tx = txVec.head.transactionSpent
      tx.outputs(txIn.previousOutput.vout.toInt)
    } else {
      throw new UnsupportedOperationException(
        "Not enough information in the InputPSBTMap to get a valid InputInfo")
    }

    val redeemScriptOpt = finalizedScriptSigOpt match {
      case Some(scriptSig) =>
        scriptSig.scriptSig match {
          case p2sh: P2SHScriptSignature =>
            Some(p2sh.redeemScript)
          case TrivialTrueScriptSignature | EmptyScriptSignature |
              _: CLTVScriptSignature | _: CSVScriptSignature |
              _: ConditionalScriptSignature | _: MultiSignatureScriptSignature |
              _: NonStandardScriptSignature | _: P2PKHScriptSignature |
              _: P2PKScriptSignature =>
            None
        }
      case None => None
    }

    val scriptWitnessOpt = finalizedScriptWitnessOpt.map(_.scriptWitness)

    InputInfo(outPoint,
              output,
              redeemScriptOpt,
              scriptWitnessOpt,
              conditionalPath,
              preImages)
  }

  private def toInputInfoNonFinalized(
      txIn: TransactionInput,
      conditionalPath: ConditionalPath,
      preImages: Vector[NetworkElement]): InputInfo = {
    val outPoint = txIn.previousOutput

    val witVec = getRecords(WitnessUTXOKeyId)
    val txVec = getRecords(NonWitnessUTXOKeyId)

    val output = if (witVec.size == 1) {
      witVec.head.witnessUTXO
    } else if (txVec.size == 1) {
      val tx = txVec.head.transactionSpent
      tx.outputs(txIn.previousOutput.vout.toInt)
    } else {
      throw new RuntimeException(
        "Not enough information in the InputPSBTMap to get a valid InputInfo")
    }

    val redeemScriptOpt = this.redeemScriptOpt.map(_.redeemScript)

    val scriptWitnessVec = getRecords(WitnessScriptKeyId)
    val scriptWitnessOpt =
      if (scriptWitnessVec.size == 1) {
        Some(P2WSHWitnessV0(scriptWitnessVec.head.witnessScript))
      } else if (
        output.scriptPubKey
          .isInstanceOf[P2WPKHWitnessSPKV0] || redeemScriptOpt.exists(
          _.isInstanceOf[P2WPKHWitnessSPKV0])
      ) {
        require(preImages.size == 1,
                "P2WPKHWitnessV0 must have it's public key as a pre-image")

        preImages.head match {
          case pubKey: ECPublicKey =>
            Some(P2WPKHWitnessV0(pubKey))
          case _: NetworkElement =>
            throw new IllegalArgumentException(
              "P2WPKHWitnessV0 must have it's public key as a pre-image")
        }
      } else {
        None
      }

    InputInfo(outPoint,
              output,
              redeemScriptOpt,
              scriptWitnessOpt,
              conditionalPath,
              preImages)
  }

  def toUTXOSigningInfo(
      txIn: TransactionInput,
      signer: Sign,
      conditionalPath: ConditionalPath =
        ConditionalPath.NoCondition): ECSignatureParams[InputInfo] = {
    require(!isFinalized, s"Cannot update an InputPSBTMap that is finalized")
    val txVec = getRecords(NonWitnessUTXOKeyId)

    val hashTypeVec = getRecords(SigHashTypeKeyId)
    val hashType =
      if (hashTypeVec.size == 1) hashTypeVec.head.hashType
      else HashType.sigHashAll

    val inputInfo = toInputInfo(txIn, conditionalPath, Vector(signer.publicKey))

    ECSignatureParams(inputInfo, txVec.head.transactionSpent, signer, hashType)
  }

  private def changeToWitnessUTXO(
      transactionOutput: TransactionOutput): InputPSBTMap = {
    val newElements = transactionOutput.scriptPubKey match {
      case _: P2SHScriptPubKey =>
        if (redeemScriptOpt.isDefined) {
          val redeemScript = redeemScriptOpt.get.redeemScript
          // SegwitV0 has a vulnerability where we need the full funding tx to be safe
          // future versions of segwit should be considered safe however
          if (
            redeemScript.isInstanceOf[WitnessScriptPubKey] && !redeemScript
              .isInstanceOf[WitnessScriptPubKeyV0]
          ) {
            filterRecords(NonWitnessUTXOKeyId) :+ WitnessUTXO(transactionOutput)
          } else {
            elements
          }
        } else {
          elements
        }
      case _: WitnessScriptPubKeyV0 | _: P2PKHScriptPubKey |
          _: P2PKScriptPubKey | _: P2PKWithTimeoutScriptPubKey |
          _: MultiSignatureScriptPubKey | EmptyScriptPubKey |
          _: LockTimeScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | _: ConditionalScriptPubKey =>
        elements
      case _: WitnessScriptPubKey =>
        filterRecords(NonWitnessUTXOKeyId) :+ WitnessUTXO(transactionOutput)
    }

    InputPSBTMap(newElements)
  }

  /**
    * After a discovered vulnerability in BIP-143, this is no longer safe for SegwitV0
    * Check if this satisfies criteria for witness. If it does, delete the NonWitnessOrUnknownUTXO field
    * This is useful for following reasons.
    * 1. Compresses the size of the data
    * 2. Allows for segwit only compatibility
    * @param txIn The TransactionInput that this InputPSBTMap represents
    * @return The compressed InputPSBTMap
    */
  def compressMap(txIn: TransactionInput): InputPSBTMap = {
    if (isFinalized) {
      this
    } else {
      val newElements = {
        if (nonWitnessOrUnknownUTXOOpt.isDefined) {
          val nonWitUtxo = nonWitnessOrUnknownUTXOOpt.get.transactionSpent
          if (txIn.previousOutput.vout.toInt < nonWitUtxo.outputs.size) {
            val out = nonWitUtxo.outputs(txIn.previousOutput.vout.toInt)
            changeToWitnessUTXO(out).elements
          } else {
            throw new IllegalArgumentException(
              s"Invalid txIn given, got: $txIn")
          }
        } else {
          elements
        }
      }
      InputPSBTMap(newElements)
    }
  }
}

object InputPSBTMap extends PSBTMapFactory[InputPSBTRecord, InputPSBTMap] {
  import org.bitcoins.core.psbt.InputPSBTRecord._

  /** Constructs a finalized InputPSBTMap from a NewSpendingInfoFull,
    * the corresponding PSBT's unsigned transaction, and if this is
    * a non-witness spend, the transaction being spent
    */
  def finalizedFromSpendingInfo(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction)(implicit
      ec: ExecutionContext): Future[InputPSBTMap] = {
    val sigComponentF = BitcoinSigner
      .sign(spendingInfo, unsignedTx, isDummySignature = false)

    sigComponentF.map { sigComponent =>
      val utxos = spendingInfo.inputInfo match {
        case _: UnassignedSegwitNativeInputInfo =>
          Vector(WitnessUTXO(spendingInfo.output))
        case _: RawInputInfo | _: P2SHNonSegwitInputInfo |
            _: SegwitV0NativeInputInfo | _: P2SHNestedSegwitV0InputInfo =>
          Vector(NonWitnessOrUnknownUTXO(spendingInfo.prevTransaction))
      }

      val scriptSig =
        FinalizedScriptSig(sigComponent.scriptSignature)
      sigComponent.transaction match {
        case _: NonWitnessTransaction =>
          InputPSBTMap(utxos ++ Vector(scriptSig))
        case wtx: WitnessTransaction =>
          val witness = wtx.witness(sigComponent.inputIndex.toInt)
          val scriptWitness = FinalizedScriptWitness(witness)
          val finalizedSigs =
            if (witness != EmptyScriptWitness) {
              Vector(scriptSig, scriptWitness)
            } else {
              Vector(scriptSig)
            }
          InputPSBTMap(utxos ++ finalizedSigs)
      }
    }
  }

  /** Constructs a full (ready to be finalized) but unfinalized InputPSBTMap
    * from a NewSpendingInfoFull, the corresponding PSBT's unsigned transaction,
    * and if this is a non-witness spend, the transaction being spent
    */
  def fromUTXOInfo(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction)(implicit
      ec: ExecutionContext): Future[InputPSBTMap] = {
    val sigsF = spendingInfo.toSingles.map { spendingInfoSingle =>
      BitcoinSigner.signSingle(spendingInfoSingle,
                               unsignedTx,
                               isDummySignature = false)
    }

    val sigFs = Future.sequence(sigsF)

    sigFs.map { sigs =>
      val builder = Vector.newBuilder[InputPSBTRecord]

      spendingInfo.inputInfo match {
        case _: UnassignedSegwitNativeInputInfo =>
          builder.+=(WitnessUTXO(spendingInfo.output))
        case _: RawInputInfo | _: P2SHNonSegwitInputInfo |
            _: SegwitV0NativeInputInfo | _: P2SHNestedSegwitV0InputInfo =>
          builder.+=(NonWitnessOrUnknownUTXO(spendingInfo.prevTransaction))
      }

      builder.++=(sigs)

      val sigHashType = SigHashType(spendingInfo.hashType)
      builder.+=(sigHashType)

      spendingInfo.inputInfo match {
        case p2sh: P2SHNonSegwitInputInfo =>
          builder.+=(RedeemScript(p2sh.redeemScript))
        case p2sh: P2SHNestedSegwitV0InputInfo =>
          builder.+=(RedeemScript(p2sh.redeemScript))
          p2sh.scriptWitness match {
            case p2wsh: P2WSHWitnessV0 =>
              builder.+=(WitnessScript(p2wsh.redeemScript))
            case _: P2WPKHWitnessV0 => ()
          }
        case p2wsh: P2WSHV0InputInfo =>
          builder.+=(WitnessScript(p2wsh.scriptWitness.redeemScript))
        case _: RawInputInfo | _: P2WPKHV0InputInfo |
            _: UnassignedSegwitNativeInputInfo =>
          ()
      }

      val inputMap = InputPSBTMap(builder.result())

      inputMap
    }
  }

  override def constructMap(elements: Vector[InputPSBTRecord]): InputPSBTMap =
    InputPSBTMap(elements)
  override def recordFactory: Factory[InputPSBTRecord] = InputPSBTRecord
}

case class OutputPSBTMap(elements: Vector[OutputPSBTRecord])
    extends SeqWrapper[OutputPSBTRecord]
    with PSBTMap[OutputPSBTRecord] {
  import org.bitcoins.core.psbt.OutputPSBTRecord._
  import org.bitcoins.core.psbt.PSBTOutputKeyId._

  override val wrapped: Vector[OutputPSBTRecord] = elements

  def redeemScriptOpt: Option[RedeemScript] = {
    getRecords(RedeemScriptKeyId).headOption
  }

  def witnessScriptOpt: Option[WitnessScript] = {
    getRecords(WitnessScriptKeyId).headOption
  }

  def BIP32DerivationPaths: Vector[BIP32DerivationPath] = {
    getRecords(BIP32DerivationPathKeyId)
  }

  def getRecords(key: PSBTOutputKeyId): Vector[key.RecordType] = {
    super.getRecords(key, PSBTOutputKeyId)
  }

  def filterRecords(key: PSBTOutputKeyId): Vector[OutputPSBTRecord] = {
    super.filterRecords(key, PSBTOutputKeyId)
  }

  /**
    * Takes another OutputPSBTMap and adds all records that are not contained in this OutputPSBTMap
    * A record's distinctness is determined by its key
    * @param other OutputPSBTMap to be combined with
    * @return A OutputPSBTMap with the combined data of the two OutputPSBTMaps
    */
  def combine(other: OutputPSBTMap): OutputPSBTMap = {
    OutputPSBTMap(distinctByKey(this.elements ++ other.elements))
  }
}

object OutputPSBTMap extends PSBTMapFactory[OutputPSBTRecord, OutputPSBTMap] {
  override def recordFactory: Factory[OutputPSBTRecord] = OutputPSBTRecord

  override def constructMap(elements: Vector[OutputPSBTRecord]): OutputPSBTMap =
    OutputPSBTMap(elements)
}
