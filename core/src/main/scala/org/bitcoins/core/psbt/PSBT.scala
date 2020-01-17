package org.bitcoins.core.psbt

import org.bitcoins.core.byteVectorOrdering
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{P2WSHWitnessV0, _}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.{CryptoUtil, Factory}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.signer.{BitcoinSigner, BitcoinSignerSingle}
import org.bitcoins.core.wallet.utxo._
import scodec.bits._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class PSBT(
    globalMap: GlobalPSBTMap,
    inputMaps: Vector[InputPSBTMap],
    outputMaps: Vector[OutputPSBTMap])
    extends NetworkElement {
  require(
    inputMaps.size == transaction.inputs.size,
    "There must be an input map for every input in the global transaction")
  require(
    outputMaps.size == transaction.outputs.size,
    "There must be an output map for every output in the global transaction")

  import org.bitcoins.core.psbt.InputPSBTRecord._
  import org.bitcoins.core.psbt.PSBTInputKeyId._

  // Need to define these so when we compare the PSBTs
  // the map lexicographical ordering is enforced
  def ==(p: PSBT): Boolean = this.bytes == p.bytes
  def !=(p: PSBT): Boolean = !(this == p)

  private val inputBytes: ByteVector =
    inputMaps.foldLeft(ByteVector.empty)(_ ++ _.bytes)

  private val outputBytes: ByteVector =
    outputMaps.foldLeft(ByteVector.empty)(_ ++ _.bytes)

  val bytes: ByteVector = PSBT.magicBytes ++
    globalMap.bytes ++
    inputBytes ++
    outputBytes

  def transaction: Transaction = globalMap.unsignedTransaction.transaction

  def isFinalized: Boolean = inputMaps.forall(_.isFinalized)

  def version: UInt32 = globalMap.version.version

  /**
    * Combiner defined by https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#combiner
    * Takes another PSBT and adds all records that are not contained in this PSBT
    * A record's distinctness is determined by its key
    * @param other PSBT to be combined with
    * @return A PSBT with the combined data of the two PSBTs
    */
  def combinePSBT(other: PSBT): PSBT = {
    require(this.transaction.txId == other.transaction.txId,
            "Can only combine PSBTs with the same global transaction.")

    val global = this.globalMap.combine(other.globalMap)
    val inputs = this.inputMaps
      .zip(other.inputMaps)
      .map { case (input, otherInput) => input.combine(otherInput) }
    val outputs = this.outputMaps
      .zip(other.outputMaps)
      .map { case (output, otherOutput) => output.combine(otherOutput) }

    PSBT(global, inputs, outputs)
  }

  /** Finalizes this PSBT if possible
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#input-finalizer]]
    * @return None if there exists any input which is not ready for signing
    */
  def finalizePSBT: Try[PSBT] = {
    if (isFinalized) {
      Failure(
        new IllegalStateException(
          s"Cannot finalize an already finalized PSBT: $this"))
    } else {
      val finalizedInputTs = inputMaps.zip(transaction.inputs).map {
        case (inputMap, input) => inputMap.finalize(input)
      }

      val finalizedInputMapsT = finalizedInputTs
        .foldLeft[Try[Vector[InputPSBTMap]]](Success(Vector.empty)) {
          case (inputsSoFarT, inputT) =>
            inputsSoFarT.flatMap { inputsSoFar =>
              inputT.map { input =>
                inputsSoFar :+ input
              }
            }
        }

      finalizedInputMapsT.map(finalizedInputMaps =>
        this.copy(inputMaps = finalizedInputMaps))
    }
  }

  /**
    * Signs the PSBT's input at the given input with the signer, then adds it to the PSBT
    * in a PartialSignature record
    * @param inputIndex Index of input to sign
    * @param signer Function or private key used to sign the PSBT
    * @param conditionalPath Represents the spending branch being taken in a ScriptPubKey's execution
    * @param isDummySignature Do not sign the tx for real, just use a dummy signature, this is useful for fee estimation
    * @return
    */
  def sign(
      inputIndex: Int,
      signer: Sign,
      conditionalPath: ConditionalPath = ConditionalPath.NoConditionsLeft,
      isDummySignature: Boolean = false)(
      implicit ec: ExecutionContext): Future[PSBT] = {
    BitcoinSignerSingle.sign(psbt = this,
                             inputIndex = inputIndex,
                             signer = signer,
                             conditionalPath = conditionalPath,
                             isDummySignature = isDummySignature)
  }

  def getUTXOSpendingInfo(
      index: Int,
      conditionalPath: ConditionalPath = ConditionalPath.NoConditionsLeft): UTXOSpendingInfoFull = {
    require(index >= 0 && index < inputMaps.size,
            s"Index must be within 0 and the number of inputs, got: $index")
    inputMaps(index)
      .toUTXOSpendingInfo(transaction.inputs(index), conditionalPath)
  }

  /**
    * Takes the InputPSBTMap at the given index and returns a UTXOSpendingInfoFull
    * that can be used to sign the input
    * @param index index of the InputPSBTMap
    * @param signers Signers that will be used to sign the input
    * @param conditionalPath Path that should be used for the script
    * @return A corresponding UTXOSpendingInfoFull
    */
  def getUTXOSpendingInfoUsingSigners(
      index: Int,
      signers: Vector[Sign],
      conditionalPath: ConditionalPath = ConditionalPath.NoConditionsLeft): UTXOSpendingInfoFull = {
    require(index >= 0 && index < inputMaps.size,
            s"Index must be within 0 and the number of inputs, got: $index")
    inputMaps(index)
      .toUTXOSpendingInfoUsingSigners(transaction.inputs(index),
                                      signers,
                                      conditionalPath)
  }

  /**
    * Adds tx to the indexed InputPSBTMap to either the NonWitnessOrUnknownUTXO
    * or WitnessUTXO field depending on the tx and available information in the PSBT
    * @param tx Transaction to add to PSBT
    * @param index index of the InputPSBTMap to add tx to
    * @return PSBT with added tx
    */
  def addUTXOToInput(tx: Transaction, index: Int): PSBT = {
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")
    require(!inputMaps(index).isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val previousElements = inputMaps(index).elements
    val txIn = transaction.inputs(index)
    val elements =
      if (txIn.previousOutput.vout.toInt < tx.outputs.size) {
        val out = tx.outputs(txIn.previousOutput.vout.toInt)

        val outIsWitnessScript =
          WitnessScriptPubKey.isWitnessScriptPubKey(out.scriptPubKey.asm)
        val hasWitScript = inputMaps(index).witnessScriptOpt.isDefined

        if (outIsWitnessScript || hasWitScript) {
          previousElements.filterNot(_.key.head == WitnessUTXOKeyId.byte) :+ WitnessUTXO(
            out)
        } else {
          previousElements.filterNot(_.key.head == NonWitnessUTXOKeyId.byte) :+ NonWitnessOrUnknownUTXO(
            tx)
        }
      } else {
        previousElements
      }

    val newInputMaps =
      inputMaps.updated(index, InputPSBTMap(elements))
    PSBT(globalMap, newInputMaps, outputMaps)

  }

  /**
    * Adds script to the indexed InputPSBTMap to either the RedeemScript
    * or WitnessScript field depending on the script and available information in the PSBT
    * @param script ScriptPubKey to add to PSBT
    * @param index index of the InputPSBTMap to add script to
    * @return PSBT with added script
    */
  def addRedeemOrWitnessScriptToInput(
      script: ScriptPubKey,
      index: Int): PSBT = {
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")
    require(!inputMaps(index).isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val previousElements = inputMaps(index).elements

    val isWitScript = WitnessScriptPubKey.isWitnessScriptPubKey(script.asm)
    val redeemScriptOpt = inputMaps(index).redeemScriptOpt
    val hasWitScript = redeemScriptOpt.isDefined && WitnessScriptPubKey
      .isWitnessScriptPubKey(redeemScriptOpt.get.redeemScript.asm)

    val elements = if (!isWitScript && hasWitScript) {
      previousElements.filterNot(
        _.key.head == PSBTInputKeyId.WitnessScriptKeyId.byte) :+ InputPSBTRecord
        .WitnessScript(script.asInstanceOf[RawScriptPubKey])
    } else {
      previousElements.filterNot(
        _.key.head == PSBTInputKeyId.RedeemScriptKeyId.byte) :+ InputPSBTRecord
        .RedeemScript(script)
    }

    val newMap = InputPSBTMap(elements).compressMap(transaction.inputs(index))
    val newInputMaps = inputMaps.updated(index, newMap)

    PSBT(globalMap, newInputMaps, outputMaps)
  }

  /**
    * Adds script to the indexed OutputPSBTMap to either the RedeemScript
    * or WitnessScript field depending on the script and available information in the PSBT
    * @param script ScriptPubKey to add to PSBT
    * @param index index of the OutputPSBTMap to add script to
    * @return PSBT with added script
    */
  def addRedeemOrWitnessScriptToOutput(
      script: ScriptPubKey,
      index: Int): PSBT = {
    require(
      index < outputMaps.size,
      s"index must be less than the number of output maps present in the psbt, $index >= ${outputMaps.size}")
    require(!isFinalized, "Cannot update a PSBT that is finalized")

    val previousElements = outputMaps(index).elements

    val isWitScript = WitnessScriptPubKey.isWitnessScriptPubKey(script.asm)
    val redeemScriptOpt =
      outputMaps(index).redeemScriptOpt
    val hasWitScript = redeemScriptOpt.isDefined && WitnessScriptPubKey
      .isWitnessScriptPubKey(redeemScriptOpt.get.redeemScript.asm)

    val elements = if (!isWitScript && hasWitScript) {
      previousElements.filterNot(
        _.key.head == PSBTOutputKeyId.WitnessScriptKeyId.byte) :+ OutputPSBTRecord
        .WitnessScript(script.asInstanceOf[RawScriptPubKey])
    } else {
      previousElements.filterNot(
        _.key.head == PSBTOutputKeyId.RedeemScriptKeyId.byte) :+ OutputPSBTRecord
        .RedeemScript(script)
    }

    val newMap = OutputPSBTMap(elements)
    val newOutputMaps = outputMaps.updated(index, newMap)

    PSBT(globalMap, inputMaps, newOutputMaps)
  }

  private def addKeyPathToMap[
      RecordType <: PSBTRecord,
      MapType <: PSBTMap[RecordType]](
      extKey: ExtKey,
      path: BIP32Path,
      index: Int,
      maps: Vector[MapType],
      makeRecord: (ECPublicKey, ByteVector, BIP32Path) => RecordType,
      makeMap: Vector[RecordType] => MapType): Vector[MapType] = {
    require(
      index < maps.size,
      s"index must be less than the number of output maps present in the psbt, $index >= ${outputMaps.size}")
    require(!isFinalized, "Cannot update a PSBT that is finalized")

    val previousElements = maps(index).elements
    val keyOpt = extKey.deriveChildPubKey(path)

    lazy val expectedBytes =
      keyOpt.get.bytes.+:(PSBTOutputKeyId.BIP32DerivationPathKeyId.byte)

    val elements =
      if (keyOpt.isSuccess && !previousElements.exists(_.key == expectedBytes)) {
        val fp =
          if (extKey.fingerprint == hex"00000000") {
            extKey.deriveChildPubKey(path.path.head).get.fingerprint
          } else {
            extKey.fingerprint
          }

        previousElements :+ makeRecord(keyOpt.get.key, fp, path)
      } else {
        previousElements
      }

    maps.updated(index, makeMap(elements))
  }

  /**
    * Adds the BIP32Path to the indexed InputPSBTMap to the BIP32DerivationPath field
    * @param extKey ExtKey to derive key from
    * @param path path of key to add to PSBT
    * @param index index of the InputPSBTMap to add the BIP32Path to
    * @return PSBT with added BIP32Path
    */
  def addKeyPathToInput(extKey: ExtKey, path: BIP32Path, index: Int): PSBT = {
    val newInputMaps = addKeyPathToMap[InputPSBTRecord, InputPSBTMap](
      extKey = extKey,
      path = path,
      index = index,
      maps = inputMaps,
      makeRecord = InputPSBTRecord.BIP32DerivationPath.apply,
      makeMap = InputPSBTMap.apply)

    PSBT(globalMap, newInputMaps, outputMaps)
  }

  /**
    * Adds the BIP32Path to the indexed OutputPSBTMap to the BIP32DerivationPath field
    * @param extKey ExtKey to derive key from
    * @param path path of key to add to PSBT
    * @param index index of the OutputPSBTMap to add the BIP32Path to
    * @return PSBT with added BIP32Path
    */
  def addKeyPathToOutput(extKey: ExtKey, path: BIP32Path, index: Int): PSBT = {
    val newOutputMaps = addKeyPathToMap[OutputPSBTRecord, OutputPSBTMap](
      extKey = extKey,
      path = path,
      index = index,
      maps = outputMaps,
      makeRecord = OutputPSBTRecord.BIP32DerivationPath.apply,
      makeMap = OutputPSBTMap.apply)

    PSBT(globalMap, inputMaps, newOutputMaps)
  }

  /**
    * @param hashType HashType to add to the input
    * @param index index of the InputPSBTMap to add the HashType to
    * @return PSBT with added HashType
    */
  def addSigHashTypeToInput(hashType: HashType, index: Int): PSBT = {
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")
    require(!inputMaps(index).isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val newElements = inputMaps(index).elements
      .filterNot(_.key.head == SigHashTypeKeyId.byte) :+ SigHashType(hashType)

    val newInputMaps =
      inputMaps.updated(index, InputPSBTMap(newElements))
    PSBT(globalMap, newInputMaps, outputMaps)
  }

  def addSignature(
      pubKey: ECPublicKey,
      sig: ECDigitalSignature,
      inputIndex: Int): PSBT =
    addSignature(PartialSignature(pubKey, sig), inputIndex)

  def addSignature(
      partialSignature: PartialSignature,
      inputIndex: Int): PSBT = {
    require(
      inputIndex < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $inputIndex >= ${inputMaps.size}")
    require(
      !inputMaps(inputIndex).isFinalized,
      s"Cannot update an InputPSBTMap that is finalized, index: $inputIndex")
    require(
      !inputMaps(inputIndex).partialSignatures
        .exists(_.pubKey == partialSignature.pubKey),
      s"Input has already been signed by ${partialSignature.pubKey}"
    )

    val newElements = inputMaps(inputIndex).elements :+ partialSignature

    val newInputMaps =
      inputMaps.updated(inputIndex, InputPSBTMap(newElements))

    PSBT(globalMap, newInputMaps, outputMaps)
  }

  def extractTransactionAndValidate: Try[Transaction] = {
    inputMaps.zipWithIndex.foldLeft(Try(extractTransaction)) {
      case (txT, (inputMap, index)) =>
        txT.flatMap { tx =>
          val wUtxoOpt = inputMap.witnessUTXOOpt
          val utxoOpt = inputMap.nonWitnessOrUnknownUTXOOpt

          (wUtxoOpt, tx) match {
            case (Some(wUtxo), wtx: WitnessTransaction) =>
              val output = wUtxo.spentWitnessTransaction
              val txSigComponent = WitnessTxSigComponent(wtx,
                                                         UInt32(index),
                                                         output,
                                                         Policy.standardFlags)
              val inputResult =
                ScriptInterpreter.run(PreExecutionScriptProgram(txSigComponent))
              if (inputResult == ScriptOk) {
                Success(tx)
              } else {
                Failure(
                  new RuntimeException(
                    s"Input $index was invalid: $inputResult"))
              }
            case (Some(_), _: BaseTransaction) =>
              Failure(new RuntimeException(
                s"Extracted program is not witness transaction, but input $index has WitnessUTXO record"))
            case (None, _) =>
              utxoOpt match {
                case Some(utxo) =>
                  val input = tx.inputs(index)
                  val output = utxo.transactionSpent.outputs(
                    input.previousOutput.vout.toInt)
                  val txSigComponent = BaseTxSigComponent(tx,
                                                          UInt32(index),
                                                          output,
                                                          Policy.standardFlags)
                  val inputResult = ScriptInterpreter.run(
                    PreExecutionScriptProgram(txSigComponent))
                  if (inputResult == ScriptOk) {
                    Success(tx)
                  } else {
                    Failure(
                      new RuntimeException(
                        s"Input $index was invalid: $inputResult"))
                  }
                case None =>
                  logger.info(
                    s"No UTXO record was provided for input $index, hence no validation was done for this input")

                  Success(tx)
              }
          }
        }
    }
  }

  /** @see [[https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#transaction-extractor]] */
  def extractTransaction: Transaction = {
    if (isFinalized) {
      val newInputs = transaction.inputs.zip(inputMaps).map {
        case (input, inputMap) =>
          val scriptSigOpt = inputMap.finalizedScriptSigOpt
          val scriptSig =
            scriptSigOpt.map(_.scriptSig).getOrElse(EmptyScriptSignature)
          TransactionInput(input.previousOutput, scriptSig, input.sequence)
      }

      if (inputMaps
            .flatMap(_.elements)
            .exists(_.isInstanceOf[FinalizedScriptWitness])) {
        val witness = inputMaps.zipWithIndex.foldLeft[TransactionWitness](
          EmptyWitness.fromInputs(transaction.inputs)) {
          case (witness, (inputMap, index)) =>
            inputMap.finalizedScriptWitnessOpt match {
              case None => witness
              case Some(
                  InputPSBTRecord.FinalizedScriptWitness(scriptWitness)) =>
                TransactionWitness(
                  witness.witnesses.updated(index, scriptWitness))
            }
        }
        WitnessTransaction(transaction.version,
                           newInputs,
                           transaction.outputs,
                           transaction.lockTime,
                           witness)
      } else {
        transaction match {
          case btx: BaseTransaction =>
            BaseTransaction(btx.version, newInputs, btx.outputs, btx.lockTime)
          case wtx: WitnessTransaction =>
            WitnessTransaction(wtx.version,
                               newInputs,
                               wtx.outputs,
                               wtx.lockTime,
                               wtx.witness)
        }
      }
    } else {
      throw new IllegalStateException(
        "PSBT must be finalized in order to extract")
    }
  }
}

object PSBT extends Factory[PSBT] {

  // The known version of PSBTs this library can process defined by https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#version-numbers
  final val knownVersions: Vector[UInt32] = Vector(UInt32.zero)

  // The magic bytes and separator defined by https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#specification
  final val magicBytes = hex"70736274ff"

  def fromBytes(bytes: ByteVector): PSBT = {
    require(bytes.startsWith(magicBytes),
            s"A PSBT must start with the magic bytes $magicBytes, got: $bytes")

    val globalBytes = bytes.drop(magicBytes.size)

    val global: GlobalPSBTMap = GlobalPSBTMap.fromBytes(globalBytes)

    val tx = global.unsignedTransaction.transaction

    @tailrec
    def mapLoop[MapType <: PSBTMap[PSBTRecord]](
        bytes: ByteVector,
        numMaps: Int,
        accum: Vector[MapType],
        factory: Factory[MapType]): Vector[MapType] = {
      if (numMaps <= 0 || bytes.isEmpty) {
        accum
      } else {
        val newMap = factory.fromBytes(bytes)
        mapLoop(bytes.drop(newMap.bytes.size),
                numMaps - 1,
                accum :+ newMap,
                factory)
      }
    }

    val inputBytes = globalBytes.drop(global.bytes.size)

    val inputMaps = mapLoop[InputPSBTMap](inputBytes,
                                          tx.inputs.size,
                                          Vector.empty,
                                          InputPSBTMap)

    val outputBytes =
      inputBytes.drop(inputMaps.foldLeft(0)(_ + _.bytes.size.toInt))

    val outputMaps = mapLoop[OutputPSBTMap](outputBytes,
                                            tx.outputs.size,
                                            Vector.empty,
                                            OutputPSBTMap)

    val remainingBytes =
      outputBytes.drop(outputMaps.foldLeft(0)(_ + _.bytes.size.toInt))

    require(remainingBytes.isEmpty,
            s"The PSBT should be empty now, got: $remainingBytes")

    PSBT(global, inputMaps, outputMaps)
  }

  def fromBase64(base64: String): PSBT = {
    ByteVector.fromBase64(base64) match {
      case None =>
        throw new IllegalArgumentException(
          s"String given was not in base64 format, got: $base64")
      case Some(bytes) => fromBytes(bytes)
    }
  }

  /**
    * Creates an empty PSBT with only the global transaction record filled
    * @param unsignedTx global transaction for the PSBT
    * @return Created PSBT
    */
  def fromUnsignedTx(unsignedTx: Transaction): PSBT = {
    val emptySigTx = BitcoinTxBuilder.emptyAllSigs(unsignedTx)
    val btx = emptySigTx match {
      case wtx: WitnessTransaction =>
        BaseTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime)
      case base: BaseTransaction => base
    }
    val globalMap = GlobalPSBTMap(
      Vector(GlobalPSBTRecord.UnsignedTransaction(btx)))
    val inputMaps = unsignedTx.inputs.map(_ => InputPSBTMap.empty).toVector
    val outputMaps = unsignedTx.outputs.map(_ => OutputPSBTMap.empty).toVector

    PSBT(globalMap, inputMaps, outputMaps)
  }

  /**
    * Wraps a Vector of pairs of UTXOSpendingInfos and the Transactions whose outputs are spent.
    * Note that this Transaction is only necessary when the output is non-segwit.
    */
  case class SpendingInfoAndNonWitnessTxs(
      infoAndTxOpts: Vector[(UTXOSpendingInfoFull, Option[Transaction])]) {
    val length: Int = infoAndTxOpts.length

    def matchesInputs(inputs: Seq[TransactionInput]): Boolean = {
      infoAndTxOpts
        .zip(inputs)
        .forall {
          case ((info, _), input) => info.outPoint == input.previousOutput
        }
    }

    def map[T](
        func: (UTXOSpendingInfoFull, Option[Transaction]) => T): Vector[T] = {
      infoAndTxOpts.map { case (info, txOpt) => func(info, txOpt) }
    }
  }

  object SpendingInfoAndNonWitnessTxs {

    /** For use in tests, makes dummy transactions with the correct output for the PSBT */
    def fromUnsignedTxAndInputs(
        unsignedTx: Transaction,
        creditingTxsInfo: Vector[UTXOSpendingInfoFull]): SpendingInfoAndNonWitnessTxs = {
      val elements = unsignedTx.inputs.toVector.map { input =>
        val infoOpt =
          creditingTxsInfo.find(_.outPoint == input.previousOutput)
        infoOpt match {
          case Some(info) =>
            val tx = BaseTransaction(Int32.zero,
                                     Vector.empty,
                                     Vector.fill(5)(info.output),
                                     UInt32.zero)
            (info, Some(tx))
          case None =>
            throw new RuntimeException(
              "CreditingTxGen.inputsAndOutputs is being inconsistent")
        }
      }

      SpendingInfoAndNonWitnessTxs(elements)
    }
  }

  /** Constructs a full (ready to be finalized) but unfinalized PSBT from an
    * unsigned transaction and a SpendingInfoAndNonWitnessTxs
    */
  def fromUnsignedTxAndInputs(
      unsignedTx: Transaction,
      spendingInfoAndNonWitnessTxs: SpendingInfoAndNonWitnessTxs)(
      implicit ec: ExecutionContext): Future[PSBT] = {
    fromUnsignedTxAndInputs(unsignedTx,
                            spendingInfoAndNonWitnessTxs,
                            finalized = false)
  }

  /** Constructs a finalized PSBT from an
    * unsigned transaction and a SpendingInfoAndNonWitnessTxs
    */
  def finalizedFromUnsignedTxAndInputs(
      unsignedTx: Transaction,
      spendingInfoAndNonWitnessTxs: SpendingInfoAndNonWitnessTxs)(
      implicit ec: ExecutionContext): Future[PSBT] = {
    fromUnsignedTxAndInputs(unsignedTx,
                            spendingInfoAndNonWitnessTxs,
                            finalized = true)
  }

  private def fromUnsignedTxAndInputs(
      unsignedTx: Transaction,
      spendingInfoAndNonWitnessTxs: SpendingInfoAndNonWitnessTxs,
      finalized: Boolean)(implicit ec: ExecutionContext): Future[PSBT] = {
    require(spendingInfoAndNonWitnessTxs.length == unsignedTx.inputs.length,
            "Must have a UTXOSpendingInfo for every input")
    require(
      spendingInfoAndNonWitnessTxs.matchesInputs(unsignedTx.inputs),
      "UTXOSpendingInfos must correspond to transaction inputs"
    )
    val emptySigTx = BitcoinTxBuilder.emptyAllSigs(unsignedTx)
    val btx = emptySigTx match {
      case wtx: WitnessTransaction =>
        BaseTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime)
      case base: BaseTransaction => base
    }

    val globalMap = GlobalPSBTMap(
      Vector(GlobalPSBTRecord.UnsignedTransaction(btx)))
    val inputMapFs = spendingInfoAndNonWitnessTxs.map {
      case (info, txOpt) =>
        if (finalized) {
          InputPSBTMap.finalizedFromUTXOSpendingInfo(info, unsignedTx, txOpt)
        } else {
          InputPSBTMap.fromUTXOSpendingInfo(info, unsignedTx, txOpt)
        }
    }
    val outputMaps = unsignedTx.outputs.map(_ => OutputPSBTMap.empty).toVector

    Future.sequence(inputMapFs).map { inputMaps =>
      PSBT(globalMap, inputMaps, outputMaps)
    }
  }
}

sealed trait PSBTRecord extends NetworkElement {

  def key: ByteVector

  def value: ByteVector

  def bytes: ByteVector = {
    val keySize = CompactSizeUInt.calc(key)
    val valueSize = CompactSizeUInt.calc(value)

    keySize.bytes ++ key ++ valueSize.bytes ++ value
  }
}

object PSBTRecord {

  def fromBytes(bytes: ByteVector): (ByteVector, ByteVector) = {
    val keyCmpctUInt = CompactSizeUInt.parse(bytes)

    if (keyCmpctUInt.toLong == 0) {
      (ByteVector.empty, ByteVector.empty)
    } else {
      val key = bytes.drop(keyCmpctUInt.size).take(keyCmpctUInt.toLong)
      val valueBytes = bytes.drop(keyCmpctUInt.size + keyCmpctUInt.toLong)
      val valueCmpctUInt = CompactSizeUInt.parse(valueBytes)
      val value = valueBytes
        .drop(valueCmpctUInt.size)
        .take(valueCmpctUInt.toLong)

      (key, value)
    }
  }
}

sealed trait GlobalPSBTRecord extends PSBTRecord

object GlobalPSBTRecord extends Factory[GlobalPSBTRecord] {
  case class UnsignedTransaction(transaction: BaseTransaction)
      extends GlobalPSBTRecord {
    require(
      transaction.inputs.forall(_.scriptSignature == EmptyScriptSignature),
      "All ScriptSignatures must be empty")

    override val key: ByteVector = hex"00"
    override val value: ByteVector = transaction.bytes
  }

  case class XPubKey(
      xpub: ExtPublicKey,
      masterFingerprint: ByteVector,
      derivationPath: BIP32Path)
      extends GlobalPSBTRecord {
    require(
      derivationPath.path.length == xpub.depth.toInt,
      s"Derivation path length does not match xpubkey depth, difference: ${derivationPath.path.length - xpub.depth.toInt}"
    )
    require(
      masterFingerprint.length == 4,
      s"Master key fingerprints are 4 bytes long, got: $masterFingerprint")

    override val key: ByteVector = hex"01" ++ xpub.bytes
    override val value: ByteVector = derivationPath.path
      .foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class Version(version: UInt32) extends GlobalPSBTRecord {
    override val key: ByteVector = hex"FB"
    override val value: ByteVector = version.bytesLE
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends GlobalPSBTRecord {
    private val keyId = PSBTGlobalKeyId.fromBytes(key)
    require(keyId == PSBTGlobalKeyId.UnknownKeyId,
            s"Cannot make an Unknown record with a $keyId")
  }

  override def fromBytes(bytes: ByteVector): GlobalPSBTRecord = {
    import org.bitcoins.core.psbt.PSBTGlobalKeyId._

    val (key, value) = PSBTRecord.fromBytes(bytes)
    PSBTGlobalKeyId.fromByte(key.head) match {
      case UnsignedTransactionKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        UnsignedTransaction(BaseTransaction.fromBytes(value))
      case XPubKeyKeyId =>
        val xpub = ExtPublicKey.fromBytes(key.tail.take(78))
        val fingerprint = value.take(4)
        val path = BIP32Path.fromBytesLE(value.drop(4))
        XPubKey(xpub, fingerprint, path)
      case VersionKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        val version = UInt32.fromBytesLE(value)

        require(PSBT.knownVersions.contains(version),
                s"Unknown version number given: $version")
        Version(version)
      case _ => GlobalPSBTRecord.Unknown(key, value)
    }
  }
}

sealed trait InputPSBTRecord extends PSBTRecord

object InputPSBTRecord extends Factory[InputPSBTRecord] {
  case class NonWitnessOrUnknownUTXO(transactionSpent: Transaction)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"00"
    override val value: ByteVector = transactionSpent.bytes
  }

  case class WitnessUTXO(spentWitnessTransaction: TransactionOutput)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"01"
    override val value: ByteVector = spentWitnessTransaction.bytes
  }

  case class PartialSignature(
      pubKey: ECPublicKey,
      signature: ECDigitalSignature)
      extends InputPSBTRecord {
    require(pubKey.size == 33, s"pubKey must be 33 bytes, got: ${pubKey.size}")

    override val key: ByteVector = hex"02" ++ pubKey.bytes
    override val value: ByteVector = signature.bytes
  }

  case class SigHashType(hashType: HashType) extends InputPSBTRecord {
    override val key: ByteVector = hex"03"
    override val value: ByteVector = hashType.num.bytesLE
  }

  case class RedeemScript(redeemScript: ScriptPubKey) extends InputPSBTRecord {
    override val key: ByteVector = hex"04"
    override val value: ByteVector = redeemScript.asmBytes
  }

  case class WitnessScript(witnessScript: RawScriptPubKey)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"05"
    override val value: ByteVector = witnessScript.asmBytes
  }

  case class BIP32DerivationPath(
      pubKey: ECPublicKey,
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends InputPSBTRecord {
    require(pubKey.size == 33, s"pubKey must be 33 bytes, got: ${pubKey.size}")

    override val key: ByteVector = hex"06" ++ pubKey.bytes
    override val value: ByteVector =
      path.path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class FinalizedScriptSig(scriptSig: ScriptSignature)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"07"
    override val value: ByteVector = scriptSig.asmBytes
  }

  case class FinalizedScriptWitness(scriptWitness: ScriptWitness)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"08"
    override val value: ByteVector = scriptWitness.bytes
  }

  case class ProofOfReservesCommitment(porCommitment: ByteVector)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"09"
    override val value: ByteVector = porCommitment
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends InputPSBTRecord {
    private val keyId = PSBTInputKeyId.fromBytes(key)
    require(keyId == PSBTInputKeyId.UnknownKeyId,
            s"Cannot make an Unknown record with a $keyId")
  }

  override def fromBytes(bytes: ByteVector): InputPSBTRecord = {
    import org.bitcoins.core.psbt.PSBTInputKeyId._

    val (key, value) = PSBTRecord.fromBytes(bytes)
    PSBTInputKeyId.fromByte(key.head) match {
      case NonWitnessUTXOKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        NonWitnessOrUnknownUTXO(Transaction(value))
      case WitnessUTXOKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        WitnessUTXO(TransactionOutput.fromBytes(value))
      case PartialSignatureKeyId =>
        val pubKey = ECPublicKey(key.tail)
        val sig = ECDigitalSignature(value)
        PartialSignature(pubKey, sig)
      case SigHashTypeKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        SigHashType(HashType.fromBytesLE(value))
      case PSBTInputKeyId.RedeemScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        InputPSBTRecord.RedeemScript(ScriptPubKey.fromAsmBytes(value))
      case PSBTInputKeyId.WitnessScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        InputPSBTRecord.WitnessScript(RawScriptPubKey.fromAsmBytes(value))
      case PSBTInputKeyId.BIP32DerivationPathKeyId =>
        val pubKey = ECPublicKey(key.tail)
        val fingerprint = value.take(4)
        val path = BIP32Path.fromBytesLE(value.drop(4))
        InputPSBTRecord.BIP32DerivationPath(pubKey, fingerprint, path)
      case FinalizedScriptSigKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        val sig = ScriptSignature.fromAsmBytes(value)
        FinalizedScriptSig(sig)
      case FinalizedScriptWitnessKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        FinalizedScriptWitness(RawScriptWitnessParser.read(value))
      case ProofOfReservesCommitmentKeyId =>
        ProofOfReservesCommitment(value)
      case _ =>
        InputPSBTRecord.Unknown(key, value)
    }
  }
}

sealed trait OutputPSBTRecord extends PSBTRecord

object OutputPSBTRecord extends Factory[OutputPSBTRecord] {

  case class RedeemScript(redeemScript: ScriptPubKey) extends OutputPSBTRecord {
    override val key: ByteVector = hex"00"
    override val value: ByteVector = redeemScript.asmBytes
  }

  case class WitnessScript(witnessScript: ScriptPubKey)
      extends OutputPSBTRecord {

    override val key: ByteVector = hex"01"
    override val value: ByteVector = witnessScript.asmBytes
  }

  case class BIP32DerivationPath(
      pubKey: ECPublicKey,
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends OutputPSBTRecord {
    require(pubKey.size == 33, s"pubKey must be 33 bytes, got: ${pubKey.size}")

    override val key: ByteVector = hex"02" ++ pubKey.bytes
    override val value: ByteVector =
      path.path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends OutputPSBTRecord {
    private val keyId = PSBTOutputKeyId.fromBytes(key)
    require(keyId == PSBTOutputKeyId.UnknownKeyId,
            s"Cannot make an Unknown record with a $keyId")
  }

  override def fromBytes(bytes: ByteVector): OutputPSBTRecord = {
    val (key, value) = PSBTRecord.fromBytes(bytes)
    PSBTOutputKeyId.fromByte(key.head) match {
      case PSBTOutputKeyId.RedeemScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        OutputPSBTRecord.RedeemScript(ScriptPubKey.fromAsmBytes(value))
      case PSBTOutputKeyId.WitnessScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        OutputPSBTRecord.WitnessScript(ScriptPubKey.fromAsmBytes(value))
      case PSBTOutputKeyId.BIP32DerivationPathKeyId =>
        val pubKey = ECPublicKey(key.tail)
        val fingerprint = value.take(4)
        val path = BIP32Path.fromBytesLE(value.drop(4))

        OutputPSBTRecord.BIP32DerivationPath(pubKey, fingerprint, path)
      case _ =>
        OutputPSBTRecord.Unknown(key, value)
    }
  }
}

/**
  * A PSBTKeyId refers to the first byte of a key that signifies which kind of key-value map
  * is in a given PSBTRecord
  */
sealed trait PSBTKeyId {
  def byte: Byte
  type RecordType <: PSBTRecord
}

sealed trait PSBTKeyIdFactory[KeyIdType <: PSBTKeyId]
    extends Factory[KeyIdType] {
  def fromByte(byte: Byte): KeyIdType
  def unknownKey: KeyIdType

  override def fromBytes(key: ByteVector): KeyIdType = {
    if (key.isEmpty) {
      unknownKey
    } else {
      fromByte(key.head)
    }
  }
}

sealed trait PSBTGlobalKeyId extends PSBTKeyId {
  type RecordType <: GlobalPSBTRecord
}

object PSBTGlobalKeyId extends PSBTKeyIdFactory[PSBTGlobalKeyId] {

  override def fromByte(byte: Byte): PSBTGlobalKeyId = byte match {
    case UnsignedTransactionKeyId.byte => UnsignedTransactionKeyId
    case XPubKeyKeyId.byte             => XPubKeyKeyId
    case VersionKeyId.byte             => VersionKeyId
    case _: Byte                       => UnknownKeyId
  }

  override def unknownKey: PSBTGlobalKeyId = UnknownKeyId

  final case object UnsignedTransactionKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0x00.byteValue
    type RecordType = GlobalPSBTRecord.UnsignedTransaction
  }

  final case object XPubKeyKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0x01.byteValue
    type RecordType = GlobalPSBTRecord.XPubKey
  }

  final case object VersionKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0xFB.byteValue
    type RecordType = GlobalPSBTRecord.Version
  }

  final case object UnknownKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = Byte.MaxValue
    type RecordType = GlobalPSBTRecord.Unknown
  }
}

sealed trait PSBTInputKeyId extends PSBTKeyId {
  type RecordType <: InputPSBTRecord
}

object PSBTInputKeyId extends PSBTKeyIdFactory[PSBTInputKeyId] {

  override def unknownKey: PSBTInputKeyId = UnknownKeyId

  override def fromByte(byte: Byte): PSBTInputKeyId = byte match {
    case NonWitnessUTXOKeyId.byte            => NonWitnessUTXOKeyId
    case WitnessUTXOKeyId.byte               => WitnessUTXOKeyId
    case PartialSignatureKeyId.byte          => PartialSignatureKeyId
    case SigHashTypeKeyId.byte               => SigHashTypeKeyId
    case RedeemScriptKeyId.byte              => RedeemScriptKeyId
    case WitnessScriptKeyId.byte             => WitnessScriptKeyId
    case BIP32DerivationPathKeyId.byte       => BIP32DerivationPathKeyId
    case FinalizedScriptSigKeyId.byte        => FinalizedScriptSigKeyId
    case FinalizedScriptWitnessKeyId.byte    => FinalizedScriptWitnessKeyId
    case ProofOfReservesCommitmentKeyId.byte => ProofOfReservesCommitmentKeyId
    case _: Byte                             => UnknownKeyId

  }

  final case object NonWitnessUTXOKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x00.byteValue
    type RecordType = InputPSBTRecord.NonWitnessOrUnknownUTXO
  }

  final case object WitnessUTXOKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x01.byteValue
    type RecordType = InputPSBTRecord.WitnessUTXO
  }

  final case object PartialSignatureKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x02.byteValue
    type RecordType = InputPSBTRecord.PartialSignature
  }

  final case object SigHashTypeKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x03.byteValue
    type RecordType = InputPSBTRecord.SigHashType
  }

  final case object RedeemScriptKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x04.byteValue
    type RecordType = InputPSBTRecord.RedeemScript
  }

  final case object WitnessScriptKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x05.byteValue
    type RecordType = InputPSBTRecord.WitnessScript
  }

  final case object BIP32DerivationPathKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x06.byteValue
    type RecordType = InputPSBTRecord.BIP32DerivationPath
  }

  final case object FinalizedScriptSigKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x07.byteValue
    type RecordType = InputPSBTRecord.FinalizedScriptSig
  }

  final case object FinalizedScriptWitnessKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x08.byteValue
    type RecordType = InputPSBTRecord.FinalizedScriptWitness
  }

  final case object ProofOfReservesCommitmentKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x09.byteValue
    type RecordType = InputPSBTRecord.ProofOfReservesCommitment
  }

  final case object UnknownKeyId extends PSBTInputKeyId {
    override val byte: Byte = Byte.MaxValue
    type RecordType = InputPSBTRecord.Unknown
  }
}

sealed trait PSBTOutputKeyId extends PSBTKeyId {
  type RecordType <: OutputPSBTRecord
}

object PSBTOutputKeyId extends PSBTKeyIdFactory[PSBTOutputKeyId] {

  override def unknownKey: PSBTOutputKeyId = UnknownKeyId

  override def fromByte(byte: Byte): PSBTOutputKeyId = byte match {
    case RedeemScriptKeyId.byte        => RedeemScriptKeyId
    case WitnessScriptKeyId.byte       => WitnessScriptKeyId
    case BIP32DerivationPathKeyId.byte => BIP32DerivationPathKeyId
    case _: Byte                       => UnknownKeyId
  }

  final case object RedeemScriptKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x00.byteValue
    type RecordType = OutputPSBTRecord.RedeemScript
  }

  final case object WitnessScriptKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x01.byteValue
    type RecordType = OutputPSBTRecord.WitnessScript
  }

  final case object BIP32DerivationPathKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x02.byteValue
    type RecordType = OutputPSBTRecord.BIP32DerivationPath
  }

  final case object UnknownKeyId extends PSBTOutputKeyId {
    override val byte: Byte = Byte.MaxValue
    type RecordType = OutputPSBTRecord.Unknown
  }
}

sealed trait PSBTMap[+RecordType <: PSBTRecord] extends NetworkElement {
  require(elements.map(_.key).groupBy(identity).values.forall(_.length == 1),
          "All keys must be unique.")

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
}

object PSBTMap {
  final val separatorByte: Byte = 0x00.byteValue
}

sealed trait PSBTMapFactory[
    RecordType <: PSBTRecord, MapType <: PSBTMap[RecordType]]
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
    extends PSBTMap[GlobalPSBTRecord] {
  import org.bitcoins.core.psbt.GlobalPSBTRecord._
  import org.bitcoins.core.psbt.PSBTGlobalKeyId._
  require(getRecords(UnsignedTransactionKeyId).nonEmpty,
          "A GlobalPSBTMap must have a Unsigned Transaction")

  def unsignedTransaction: UnsignedTransaction = {
    getRecords(UnsignedTransactionKeyId).head
  }

  def extendedPublicKeys: Vector[XPubKey] = {
    getRecords(XPubKeyKeyId)
  }

  def version: Version = {
    getRecords(VersionKeyId).headOption.getOrElse(Version(UInt32.zero))
  }

  private def getRecords(key: PSBTGlobalKeyId): Vector[key.RecordType] = {
    super.getRecords(key, PSBTGlobalKeyId)
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
    GlobalPSBTMap((this.elements ++ other.elements).distinct)
  }
}

object GlobalPSBTMap extends PSBTMapFactory[GlobalPSBTRecord, GlobalPSBTMap] {

  override def recordFactory: Factory[GlobalPSBTRecord] = GlobalPSBTRecord
  override def constructMap(elements: Vector[GlobalPSBTRecord]): GlobalPSBTMap =
    GlobalPSBTMap(elements)
}

case class InputPSBTMap(elements: Vector[InputPSBTRecord])
    extends PSBTMap[InputPSBTRecord] {
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

  private def getRecords(key: PSBTInputKeyId): Vector[key.RecordType] = {
    super.getRecords(key, PSBTInputKeyId)
  }

  def isFinalized: Boolean =
    getRecords(FinalizedScriptSigKeyId).nonEmpty || getRecords(
      FinalizedScriptWitnessKeyId).nonEmpty

  /** Finalizes this input if possible, returning None if not */
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
            Success(witnessUtxo.spentWitnessTransaction.scriptPubKey)
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
      val utxos = getRecords(WitnessUTXOKeyId) ++ getRecords(
        NonWitnessUTXOKeyId)
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
        constructScriptSig: Seq[(ECDigitalSignature, ECPublicKey)] => ScriptSignature): Try[
      InputPSBTMap] = {
      val sigs = getRecords(PartialSignatureKeyId)
      if (sigs.length != required) {
        Failure(new IllegalArgumentException(
          s"Could not collect $required signatures when only the following were present: $sigs"))
      } else {
        val scriptSig = constructScriptSig(
          sigs.map(sig => (sig.signature, sig.pubKey)))

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
                ConditionalScriptSignature(nestedScriptSig.scriptSig, path)
              wipeAndAdd(scriptSig)
            }
        }
      case locktime: LockTimeScriptPubKey =>
        finalize(locktime.nestedScriptPubKey)
      case _: P2PKHScriptPubKey =>
        collectSigs(required = 1,
                    sigs => P2PKHScriptSignature(sigs.head._1, sigs.head._2))
      case _: P2PKScriptPubKey =>
        collectSigs(required = 1, sigs => P2PKScriptSignature(sigs.head._1))
      case multiSig: MultiSignatureScriptPubKey =>
        def generateScriptSig(
            sigs: Seq[(ECDigitalSignature, ECPublicKey)]): MultiSignatureScriptSignature = {
          val sortedSigs = sigs
            .map {
              case (sig, pubKey) =>
                (sig, multiSig.publicKeys.indexOf(pubKey))
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
    InputPSBTMap((this.elements ++ other.elements).distinct)
  }

  def toUTXOSpendingInfo(
      txIn: TransactionInput,
      conditionalPath: ConditionalPath = ConditionalPath.NoConditionsLeft): UTXOSpendingInfoFull = {
    val signersVec = getRecords(PartialSignatureKeyId)
    val signers =
      signersVec.map(sig => Sign.constant(sig.signature, sig.pubKey))

    toUTXOSpendingInfoUsingSigners(txIn, signers, conditionalPath)
  }

  /**
    * Takes the InputPSBTMap returns a UTXOSpendingInfoFull
    * that can be used to sign the input
    * @param txIn The transaction input that this InputPSBTMap represents
    * @param signers Signers that will be used to sign the input
    * @param conditionalPath Path that should be used for the script
    * @return A corresponding UTXOSpendingInfoFull
    */
  def toUTXOSpendingInfoUsingSigners(
      txIn: TransactionInput,
      signers: Vector[Sign],
      conditionalPath: ConditionalPath = ConditionalPath.NoConditionsLeft): UTXOSpendingInfoFull = {
    require(!isFinalized, s"Cannot update an InputPSBTMap that is finalized")
    BitcoinUTXOSpendingInfoFull(
      toUTXOSpendingInfoSingle(txIn, signers.head, conditionalPath),
      signers
    )
  }

  def toUTXOSpendingInfoSingle(
      txIn: TransactionInput,
      signer: Sign,
      conditionalPath: ConditionalPath = ConditionalPath.NoConditionsLeft): UTXOSpendingInfoSingle = {
    require(!isFinalized, s"Cannot update an InputPSBTMap that is finalized")
    val outPoint = txIn.previousOutput

    val witVec = getRecords(WitnessUTXOKeyId)
    val txVec = getRecords(NonWitnessUTXOKeyId)

    val output = if (witVec.size == 1) {
      witVec.head.spentWitnessTransaction
    } else if (txVec.size == 1) {
      val tx = txVec.head.transactionSpent
      tx.outputs(txIn.previousOutput.vout.toInt)
    } else {
      throw new UnsupportedOperationException(
        "Not enough information in the InputPSBTMap to get a valid UTXOSpendingInfo")
    }

    val redeemScriptVec = getRecords(RedeemScriptKeyId)
    val redeemScriptOpt =
      if (redeemScriptVec.size == 1) Some(redeemScriptVec.head.redeemScript)
      else None

    val scriptWitnessVec = getRecords(WitnessScriptKeyId)
    val scriptWitnessOpt =
      if (scriptWitnessVec.size == 1) {
        Some(P2WSHWitnessV0(scriptWitnessVec.head.witnessScript))
      } else if (output.scriptPubKey
                   .isInstanceOf[P2WPKHWitnessSPKV0] || redeemScriptOpt.exists(
                   _.isInstanceOf[P2WPKHWitnessSPKV0])) {
        Some(P2WPKHWitnessV0(signer.publicKey))
      } else {
        None
      }

    val hashTypeVec = getRecords(SigHashTypeKeyId)
    val hashType =
      if (hashTypeVec.size == 1) hashTypeVec.head.hashType
      else HashType.sigHashAll

    BitcoinUTXOSpendingInfoSingle(outPoint,
                                  output,
                                  signer,
                                  redeemScriptOpt,
                                  scriptWitnessOpt,
                                  hashType,
                                  conditionalPath)
  }

  /**
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
        val nonWitUtxoVec =
          this.getRecords(NonWitnessUTXOKeyId)
        val witScriptVec =
          this.getRecords(WitnessScriptKeyId)
        if (nonWitUtxoVec.size == 1 && witScriptVec.size == 1) {
          val nonWitUtxo = nonWitUtxoVec.head.transactionSpent
          if (txIn.previousOutput.vout.toInt < nonWitUtxo.outputs.size) {
            val out = nonWitUtxo.outputs(txIn.previousOutput.vout.toInt)
            elements
              .filterNot(_.key.head == NonWitnessUTXOKeyId.byte)
              .filterNot(_.key.head == WitnessUTXOKeyId.byte) :+ WitnessUTXO(
              out)
          } else {
            elements.filterNot(_.key.head == NonWitnessUTXOKeyId.byte)
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

  /** Constructs a finalized InputPSBTMap from a UTXOSpendingInfoFull,
    * the corresponding PSBT's unsigned transaction, and if this is
    * a non-witness spend, the transaction being spent
    */
  def finalizedFromUTXOSpendingInfo(
      spendingInfo: UTXOSpendingInfoFull,
      unsignedTx: Transaction,
      nonWitnessTxOpt: Option[Transaction])(
      implicit ec: ExecutionContext): Future[InputPSBTMap] = {
    val sigComponentF = BitcoinSigner
      .sign(spendingInfo, unsignedTx, isDummySignature = false)

    sigComponentF.map { sigComponent =>
      val utxos = spendingInfo match {
        case _: SegwitV0NativeUTXOSpendingInfoFull |
            _: P2SHNestedSegwitV0UTXOSpendingInfoFull =>
          Vector(WitnessUTXO(spendingInfo.output))
        case _: RawScriptUTXOSpendingInfoFull | _: P2SHNoNestSpendingInfo |
            _: UnassignedSegwitNativeUTXOSpendingInfo =>
          nonWitnessTxOpt match {
            case None => Vector.empty
            case Some(nonWitnessTx) =>
              Vector(NonWitnessOrUnknownUTXO(nonWitnessTx))
          }
      }

      val scriptSig =
        FinalizedScriptSig(sigComponent.scriptSignature)
      sigComponent.transaction match {
        case _: BaseTransaction => InputPSBTMap(utxos ++ Vector(scriptSig))
        case wtx: WitnessTransaction =>
          val witness = wtx.witness.witnesses(sigComponent.inputIndex.toInt)
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
    * from a UTXOSpendingInfoFull, the corresponding PSBT's unsigned transaction,
    * and if this is a non-witness spend, the transaction being spent
    */
  def fromUTXOSpendingInfo(
      spendingInfo: UTXOSpendingInfoFull,
      unsignedTx: Transaction,
      nonWitnessTxOpt: Option[Transaction])(
      implicit ec: ExecutionContext): Future[InputPSBTMap] = {
    val sigsF = spendingInfo.toSingles.map { spendingInfoSingle =>
      BitcoinSignerSingle.signSingle(spendingInfoSingle,
                                     unsignedTx,
                                     isDummySignature = false)
    }

    val sigFs = Future.sequence(sigsF)

    sigFs.map { sigs =>
      val builder = Vector.newBuilder[InputPSBTRecord]

      spendingInfo match {
        case _: SegwitV0NativeUTXOSpendingInfoFull |
            _: P2SHNestedSegwitV0UTXOSpendingInfoFull =>
          builder.+=(WitnessUTXO(spendingInfo.output))
        case _: RawScriptUTXOSpendingInfoFull | _: P2SHNoNestSpendingInfo |
            _: UnassignedSegwitNativeUTXOSpendingInfo =>
          nonWitnessTxOpt match {
            case None => ()
            case Some(nonWitnessTx) =>
              builder.+=(NonWitnessOrUnknownUTXO(nonWitnessTx))
          }
      }

      builder.++=(sigs)

      val sigHashType = SigHashType(spendingInfo.hashType)
      builder.+=(sigHashType)

      spendingInfo match {
        case p2sh: P2SHNoNestSpendingInfo =>
          builder.+=(RedeemScript(p2sh.redeemScript))
        case p2sh: P2SHNestedSegwitV0UTXOSpendingInfoFull =>
          builder.+=(RedeemScript(p2sh.redeemScript))
          p2sh.scriptWitness match {
            case p2wsh: P2WSHWitnessV0 =>
              builder.+=(WitnessScript(p2wsh.redeemScript))
            case _: P2WPKHWitnessV0 => ()
          }
        case p2wsh: P2WSHV0SpendingInfo =>
          builder.+=(WitnessScript(p2wsh.scriptWitness.redeemScript))
        case _: RawScriptUTXOSpendingInfoFull | _: P2WPKHV0SpendingInfo |
            _: UnassignedSegwitNativeUTXOSpendingInfo =>
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
    extends PSBTMap[OutputPSBTRecord] {
  import org.bitcoins.core.psbt.OutputPSBTRecord._
  import org.bitcoins.core.psbt.PSBTOutputKeyId._

  def redeemScriptOpt: Option[RedeemScript] = {
    getRecords(RedeemScriptKeyId).headOption
  }

  def witnessScriptOpt: Option[WitnessScript] = {
    getRecords(WitnessScriptKeyId).headOption
  }

  def BIP32DerivationPaths: Vector[BIP32DerivationPath] = {
    getRecords(BIP32DerivationPathKeyId)
  }

  private def getRecords(key: PSBTOutputKeyId): Vector[key.RecordType] = {
    super.getRecords(key, PSBTOutputKeyId)
  }

  /**
    * Takes another OutputPSBTMap and adds all records that are not contained in this OutputPSBTMap
    * A record's distinctness is determined by its key
    * @param other OutputPSBTMap to be combined with
    * @return A OutputPSBTMap with the combined data of the two OutputPSBTMaps
    */
  def combine(other: OutputPSBTMap): OutputPSBTMap = {
    OutputPSBTMap((this.elements ++ other.elements).distinct)
  }
}

object OutputPSBTMap extends PSBTMapFactory[OutputPSBTRecord, OutputPSBTMap] {
  override def recordFactory: Factory[OutputPSBTRecord] = OutputPSBTRecord
  override def constructMap(elements: Vector[OutputPSBTRecord]): OutputPSBTMap =
    OutputPSBTMap(elements)
}
