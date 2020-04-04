package org.bitcoins.core.psbt

import org.bitcoins.core.crypto._
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.util.Factory
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.signer.BitcoinSignerSingle
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
    s"There must be an input map for every input in the global transaction, inputs: ${transaction.inputs}")
  require(
    outputMaps.size == transaction.outputs.size,
    s"There must be an output map for every output in the global transaction, outputs: ${transaction.outputs}")

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

  override val bytes: ByteVector = PSBT.magicBytes ++
    globalMap.bytes ++
    inputBytes ++
    outputBytes

  val base64: String = bytes.toBase64

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

  /** Finalizes this PSBT if possible, returns a Failure otherwise
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#input-finalizer]]
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

    val inputMap = inputMaps(index)
    require(!inputMap.isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val txIn = transaction.inputs(index)
    val elements =
      if (txIn.previousOutput.vout.toInt < tx.outputs.size) {
        val out = tx.outputs(txIn.previousOutput.vout.toInt)

        val outIsWitnessScript =
          WitnessScriptPubKey.isWitnessScriptPubKey(out.scriptPubKey.asm)
        val hasWitScript = inputMap.witnessScriptOpt.isDefined
        val hasWitRedeemScript = inputMap.redeemScriptOpt.isDefined && WitnessScriptPubKey
          .isWitnessScriptPubKey(inputMap.redeemScriptOpt.get.redeemScript.asm)

        if (outIsWitnessScript || hasWitScript || hasWitRedeemScript) {
          inputMap.filterRecords(WitnessUTXOKeyId) :+ WitnessUTXO(out)
        } else {
          inputMap.filterRecords(NonWitnessUTXOKeyId) :+ NonWitnessOrUnknownUTXO(
            tx)
        }
      } else {
        throw new IllegalArgumentException(
          s"Transaction does not correspond to map at given index($index), got: $tx")
      }

    val newInputMaps =
      inputMaps.updated(index, InputPSBTMap(elements))
    PSBT(globalMap, newInputMaps, outputMaps)

  }

  def addWitnessUTXOToInput(output: TransactionOutput, index: Int): PSBT = {
    require(WitnessScriptPubKey.isWitnessScriptPubKey(output.scriptPubKey.asm),
            s"Given output was not a Witness UTXO: $output")
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")

    val inputMap = inputMaps(index)
    require(!inputMap.isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val elements = inputMap.filterRecords(WitnessUTXOKeyId) :+ WitnessUTXO(
      output)
    val newInputMaps = inputMaps.updated(index, InputPSBTMap(elements))

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

    val inputMap = inputMaps(index)

    val isWitScript = WitnessScriptPubKey.isWitnessScriptPubKey(script.asm)
    val redeemScriptOpt = inputMap.redeemScriptOpt
    val hasRedeemScript = redeemScriptOpt.isDefined && WitnessScriptPubKey
      .isWitnessScriptPubKey(redeemScriptOpt.get.redeemScript.asm)

    val elements = if (!isWitScript && hasRedeemScript) {
      inputMap.filterRecords(WitnessScriptKeyId) :+ InputPSBTRecord
        .WitnessScript(script.asInstanceOf[RawScriptPubKey])
    } else {
      inputMap.filterRecords(RedeemScriptKeyId) :+ InputPSBTRecord.RedeemScript(
        script)
    }
    val newMap = InputPSBTMap(elements).compressMap(transaction.inputs(index))
    val newInputMaps = inputMaps.updated(index, newMap)

    PSBT(globalMap, newInputMaps, outputMaps)
  }

  private def redeemScriptToOutputRecord(
      outputScript: ScriptPubKey,
      redeemScript: ScriptPubKey): OutputPSBTRecord = {
    outputScript match {
      case p2sh: P2SHScriptPubKey =>
        val scriptHash = P2SHScriptPubKey(redeemScript).scriptHash
        if (scriptHash != p2sh.scriptHash) {
          throw new IllegalArgumentException(
            s"The given script's hash does not match the expected script has, got: $scriptHash, expected ${p2sh.scriptHash}")
        } else {
          OutputPSBTRecord.RedeemScript(redeemScript)
        }
      case p2wsh: P2WSHWitnessSPKV0 =>
        val scriptHash = P2WSHWitnessSPKV0(redeemScript).scriptHash
        if (scriptHash != p2wsh.scriptHash) {
          throw new IllegalArgumentException(
            s"The given script's hash does not match the expected script has, got: $scriptHash, expected ${p2wsh.scriptHash}")
        } else {
          OutputPSBTRecord.WitnessScript(redeemScript)
        }
      case _: NonStandardScriptPubKey | _: WitnessCommitment |
          EmptyScriptPubKey | _: MultiSignatureScriptPubKey |
          _: ConditionalScriptPubKey | _: LockTimeScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: WitnessScriptPubKey |
          _: P2PKScriptPubKey | _: P2PKHScriptPubKey =>
        throw new IllegalArgumentException(
          s"Output script does not need a redeem script, got: $outputScript")
    }
  }

  def addScriptWitnessToInput(
      scriptWitness: ScriptWitness,
      index: Int): PSBT = {
    require(index >= 0,
            s"index must be greater than or equal to 0, got: $index")
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")
    require(!inputMaps(index).isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")
    require(
      inputMaps(index).witnessScriptOpt.isEmpty,
      s"Input map already contains a ScriptWitness: ${inputMaps(index).witnessScriptOpt.get}")

    val previousElements = inputMaps(index).elements

    val newMap = scriptWitness match {
      case _: P2WPKHWitnessV0 =>
        // We do not need to add the ScriptWitness because it will be known
        // by having a 20 byte hash in the script
        InputPSBTMap(previousElements)
      case p2wsh: P2WSHWitnessV0 =>
        val newElement = InputPSBTRecord.WitnessScript(p2wsh.redeemScript)
        InputPSBTMap(previousElements :+ newElement)
      case EmptyScriptWitness =>
        throw new IllegalArgumentException(
          s"Invalid scriptWitness given, got: $scriptWitness")
    }
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

    val outputMap = outputMaps(index)
    val redeemScriptOpt = outputMap.redeemScriptOpt.map(_.redeemScript)
    val isWitScript = WitnessScriptPubKey.isWitnessScriptPubKey(script.asm)
    val hasWitScript = redeemScriptOpt.isDefined && WitnessScriptPubKey
      .isWitnessScriptPubKey(redeemScriptOpt.get.asm)

    val newElement =
      if (!isWitScript && hasWitScript)
        redeemScriptToOutputRecord(redeemScriptOpt.get, script)
      else
        redeemScriptToOutputRecord(transaction.outputs(index).scriptPubKey,
                                   script)

    val newMap = OutputPSBTMap(outputMap.elements :+ newElement)
    val newOutputMaps = outputMaps.updated(index, newMap)

    PSBT(globalMap, inputMaps, newOutputMaps)
  }

  def addScriptWitnessToOutput(
      scriptWitness: ScriptWitness,
      index: Int): PSBT = {
    require(index >= 0,
            s"index must be greater than or equal to 0, got: $index")
    require(
      index < outputMaps.size,
      s"index must be less than the number of output maps present in the psbt, $index >= ${inputMaps.size}")
    require(!isFinalized, "Cannot update a PSBT that is finalized")
    require(
      outputMaps(index).witnessScriptOpt.isEmpty,
      s"Output map already contains a ScriptWitness: ${outputMaps(index).witnessScriptOpt.get}")

    val outputMap = outputMaps(index)

    val newMap = scriptWitness match {
      case _: P2WPKHWitnessV0 =>
        // We do not need to add a WitnessScript for P2WPKH because it will
        // be assumed because of the ScriptPubKey having the 20-byte hash
        outputMap
      case p2wsh: P2WSHWitnessV0 =>
        OutputPSBTMap(
          outputMap.filterRecords(PSBTOutputKeyId.WitnessScriptKeyId) :+
            OutputPSBTRecord.WitnessScript(p2wsh.redeemScript))
      case EmptyScriptWitness =>
        throw new IllegalArgumentException(
          s"Invalid scriptWitness given, got: $scriptWitness")
    }

    val newOutputMaps = outputMaps.updated(index, newMap)
    PSBT(globalMap, inputMaps, newOutputMaps)
  }

  private def addKeyPathToMap[
      RecordType <: PSBTRecord,
      MapType <: PSBTMap[RecordType]](
      extKey: ExtKey,
      path: BIP32Path,
      index: Int,
      keyIdByte: Byte,
      maps: Vector[MapType],
      makeRecord: (ECPublicKey, ByteVector, BIP32Path) => RecordType,
      makeMap: Vector[RecordType] => MapType): Vector[MapType] = {
    require(
      index < maps.size,
      s"index must be less than the number of output maps present in the psbt, $index >= ${outputMaps.size}")
    require(!isFinalized, "Cannot update a PSBT that is finalized")

    val previousElements = maps(index).elements
    val keyT = extKey.deriveChildPubKey(path)

    keyT match {
      case Success(key) =>
        lazy val expectedBytes = key.bytes.+:(keyIdByte)

        val elements =
          if (!previousElements.exists(_.key == expectedBytes)) {
            val fp =
              if (extKey.fingerprint == ExtKey.masterFingerprint) {
                extKey.deriveChildPubKey(path.head).get.fingerprint
              } else {
                extKey.fingerprint
              }

            previousElements :+ makeRecord(key.key, fp, path)
          } else {
            previousElements
          }

        maps.updated(index, makeMap(elements))
      case Failure(err) =>
        throw err
    }
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
      keyIdByte = PSBTInputKeyId.BIP32DerivationPathKeyId.byte,
      maps = inputMaps,
      makeRecord = InputPSBTRecord.BIP32DerivationPath.apply,
      makeMap = InputPSBTMap.apply
    )

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
      keyIdByte = PSBTOutputKeyId.BIP32DerivationPathKeyId.byte,
      maps = outputMaps,
      makeRecord = OutputPSBTRecord.BIP32DerivationPath.apply,
      makeMap = OutputPSBTMap.apply
    )

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

    val newElements = inputMaps(index).filterRecords(SigHashTypeKeyId) :+ SigHashType(
      hashType)

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

  def addSignatures(
      partialSignatures: Vector[PartialSignature],
      inputIndex: Int): PSBT = {
    require(
      inputIndex < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $inputIndex >= ${inputMaps.size}")
    require(
      !inputMaps(inputIndex).isFinalized,
      s"Cannot update an InputPSBTMap that is finalized, index: $inputIndex")
    val intersect = inputMaps(inputIndex).partialSignatures
      .map(_.pubKey)
      .intersect(partialSignatures.map(_.pubKey))
    require(
      intersect.isEmpty,
      s"Input has already been signed by one or more of the associated public keys given ${intersect}"
    )
    val newElements = inputMaps(inputIndex).elements ++ partialSignatures

    val newInputMaps =
      inputMaps.updated(inputIndex, InputPSBTMap(newElements))

    PSBT(globalMap, newInputMaps, outputMaps)
  }

  /**
    * Extracts the serialized from the serialized, fully signed transaction from
    * this PSBT and validates the script signatures using the ScriptInterpreter.
    * Only inputs for which UTXO records are present get validated.
    *
    * Note: This PSBT must be finalized.
    */
  def extractTransactionAndValidate: Try[Transaction] = {
    inputMaps.zipWithIndex.foldLeft(Try(extractTransaction)) {
      case (txT, (inputMap, index)) =>
        txT.flatMap { tx =>
          val wUtxoOpt = inputMap.witnessUTXOOpt
          val utxoOpt = inputMap.nonWitnessOrUnknownUTXOOpt

          (wUtxoOpt, tx) match {
            case (Some(wUtxo), wtx: WitnessTransaction) =>
              val output = wUtxo.witnessUTXO
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
                  witness.updated(index, scriptWitness).toVector)
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

  final val empty = fromUnsignedTx(EmptyTransaction)

  def fromString(str: String): PSBT = {
    ByteVector.fromHex(str) match {
      case Some(hex) =>
        PSBT(hex)
      case None =>
        ByteVector.fromBase64(str) match {
          case Some(base64) =>
            PSBT(base64)
          case None =>
            throw new IllegalArgumentException(
              s"String given must be in base64 or hexadecimal, got: $str")
        }
    }
  }

  override def fromBytes(bytes: ByteVector): PSBT = {
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
    require(unsignedTx.inputs.forall(_.scriptSignature == EmptyScriptSignature),
            s"The transaction must not have any signatures, got: $unsignedTx")

    val btx = unsignedTx match {
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
      infoAndTxOpts: Vector[(UTXOSpendingInfoFull, Option[BaseTransaction])]) {
    val length: Int = infoAndTxOpts.length

    def matchesInputs(inputs: Seq[TransactionInput]): Boolean = {
      infoAndTxOpts
        .zip(inputs)
        .forall {
          case ((info, _), input) => info.outPoint == input.previousOutput
        }
    }

    def map[T](
        func: (UTXOSpendingInfoFull, Option[BaseTransaction]) => T): Vector[T] = {
      infoAndTxOpts.map { case (info, txOpt) => func(info, txOpt) }
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
    val emptySigTx = BitcoinTxBuilder.emptyAllScriptSigs(unsignedTx)
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
