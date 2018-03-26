package org.bitcoins.core.crypto

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.serializers.transaction.RawTransactionOutputParser
import org.bitcoins.core.util.{ BitcoinSLogger, BitcoinSUtil, BitcoinScriptUtil, CryptoUtil }

/**
 * Created by chris on 2/16/16.
 * Wrapper that serializes like Transaction, but with the modifications
 * required for the signature hash done
 * [[https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/interpreter.cpp#L1016-L1105]]
 * bitcoinj version of this
 * [[https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/org/bitcoinj/core/Transaction.java#L924-L1008]]
 */
sealed abstract class TransactionSignatureSerializer {

  private val logger = BitcoinSLogger.logger

  /**
   * Bitcoin Core's bug is that SignatureHash was supposed to return a hash and on this codepath it
   * actually returns the constant "1" to indicate an error
   */
  private def errorHash: DoubleSha256Digest = DoubleSha256Digest(BitcoinSUtil.decodeHex("0100000000000000000000000000000000000000000000000000000000000000"))

  /**
   * Serializes a transaction to be signed by an ECKey
   * follows the bitcoinj implementation
   * hashing is done in the [[hashForSignature]] function
   */
  def serializeForSignature(spendingTransaction: Transaction, inputIndex: UInt32, script: Seq[ScriptToken], hashType: HashType): Seq[Byte] = {
    logger.debug("Serializing for signature")
    logger.debug("Script: " + script)
    // Clear input scripts in preparation for signing. If we're signing a fresh
    // CScript's inside the Bitcoin Core codebase retain their compactSizeUInt
    // while clearing out all of the actual asm operations in the CScript
    val inputSigsRemoved = for {
      input <- spendingTransaction.inputs
      s = input.scriptSignature
    } yield TransactionInput(input.previousOutput, NonStandardScriptSignature(s.compactSizeUInt.hex), input.sequence)

    //make sure all scriptSigs have empty asm
    inputSigsRemoved.map(input =>
      require(input.scriptSignature.asm.isEmpty, "Input asm was not empty " + input.scriptSignature.asm))

    // This step has no purpose beyond being synchronized with Bitcoin Core's bugs. OP_CODESEPARATOR
    // is a legacy holdover from a previous, broken design of executing scripts that shipped in Bitcoin 0.1.
    // It was seriously flawed and would have let anyone take anyone elses money. Later versions switched to
    // the design we use today where scripts are executed independently but share a stack. This left the
    // OP_CODESEPARATOR instruction having no purpose as it was only meant to be used internally, not actually
    // ever put into scripts. Deleting OP_CODESEPARATOR is a step that should never be required but if we don't
    // do it, we could split off the main chain.
    logger.debug("Before Bitcoin-S Script to be connected: " + script)
    val scriptWithOpCodeSeparatorsRemoved: Seq[ScriptToken] = removeOpCodeSeparators(script)

    logger.debug("After Bitcoin-S Script to be connected: " + scriptWithOpCodeSeparatorsRemoved)

    val inputToSign = inputSigsRemoved(inputIndex.toInt)

    // Set the input to the script of its output. Bitcoin Core does this but the step has no obvious purpose as
    // the signature covers the hash of the prevout transaction which obviously includes the output script
    // already. Perhaps it felt safer to him in some way, or is another leftover from how the code was written.
    val scriptSig = ScriptSignature.fromAsm(scriptWithOpCodeSeparatorsRemoved)
    val inputWithConnectedScript = TransactionInput(inputToSign.previousOutput, scriptSig, inputToSign.sequence)

    //update the input at index i with inputWithConnectScript
    val updatedInputs = for {
      (input, index) <- inputSigsRemoved.zipWithIndex
    } yield {
      if (UInt32(index) == inputIndex) {
        inputWithConnectedScript
      } else input
    }

    val txWithInputSigsRemoved = BaseTransaction(spendingTransaction.version, updatedInputs, spendingTransaction.outputs, spendingTransaction.lockTime)
    val sigHashBytes = hashType.num.bytes.reverse
    //check the hash type
    //TODO: could probably be optimized w/ HO function
    hashType match {
      case _: SIGHASH_NONE =>
        val sigHashNoneTx: Transaction = sigHashNone(txWithInputSigsRemoved, inputIndex)
        sigHashNoneTx.bytes ++ sigHashBytes

      case _: SIGHASH_SINGLE =>
        if (inputIndex >= UInt32(spendingTransaction.outputs.size)) {
          // comment copied from bitcoinj
          // The input index is beyond the number of outputs, it's a buggy signature made by a broken
          // Bitcoin implementation. Bitcoin Core also contains a bug in handling this case:
          // any transaction output that is signed in this case will result in both the signed output
          // and any future outputs to this public key being steal-able by anyone who has
          // the resulting signature and the public key (both of which are part of the signed tx input).

          // Bitcoin Core's bug is that SignatureHash was supposed to return a hash and on this codepath it
          // actually returns the constant "1" to indicate an error, which is never checked for. Oops.
          errorHash.bytes
        } else {
          val sigHashSingleTx = sigHashSingle(txWithInputSigsRemoved, inputIndex)
          sigHashSingleTx.bytes ++ sigHashBytes
        }

      case _: SIGHASH_ALL =>
        val sigHashAllTx: Transaction = sigHashAll(txWithInputSigsRemoved, inputIndex)
        sigHashAllTx.bytes ++ sigHashBytes

      case _: SIGHASH_ANYONECANPAY =>
        val txWithInputsRemoved = sigHashAnyoneCanPay(txWithInputSigsRemoved, inputWithConnectedScript)
        txWithInputsRemoved.bytes ++ sigHashBytes

      case _: SIGHASH_ALL_ANYONECANPAY =>
        val sigHashAllTx = sigHashAll(txWithInputSigsRemoved, inputIndex)
        val sigHashAllAnyoneCanPayTx = sigHashAnyoneCanPay(sigHashAllTx, inputWithConnectedScript)
        sigHashAllAnyoneCanPayTx.bytes ++ sigHashBytes

      case _: SIGHASH_NONE_ANYONECANPAY =>
        val sigHashNoneTx = sigHashNone(txWithInputSigsRemoved, inputIndex)
        val sigHashNoneAnyoneCanPay = sigHashAnyoneCanPay(sigHashNoneTx, inputWithConnectedScript)
        sigHashNoneAnyoneCanPay.bytes ++ sigHashBytes

      case _: SIGHASH_SINGLE_ANYONECANPAY =>
        val sigHashSingleTx = sigHashSingle(txWithInputSigsRemoved, inputIndex)
        val sigHashSingleAnyoneCanPay = sigHashAnyoneCanPay(sigHashSingleTx, inputWithConnectedScript)
        sigHashSingleAnyoneCanPay.bytes ++ sigHashBytes
    }
  }

  /**
   * Serializes then hashes a transaction for signing
   * this is an implementation of it's bitcoinj equivalent found here
   * [[https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/org/bitcoinj/core/Transaction.java#L924]]
   * @param spendingTransaction the transaction we are hashing
   * @param inputIndex the inputIndex we are hashing for signing
   * @param script the script which we are spending
   * @param hashType the hash type we are serializing this tx for
   * @return
   */
  def hashForSignature(spendingTransaction: Transaction, inputIndex: UInt32, script: Seq[ScriptToken],
                       hashType: HashType): DoubleSha256Digest = {
    //these first two checks are in accordance with behavior in bitcoin core
    //[[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L1112-L1123]]
    if (inputIndex >= UInt32(spendingTransaction.inputs.size)) {
      logger.warn("Our inputIndex is out of the range of the inputs in the spending transaction")
      errorHash
    } else if ((hashType.isInstanceOf[SIGHASH_SINGLE] || hashType.isInstanceOf[SIGHASH_SINGLE_ANYONECANPAY]) &&
      inputIndex >= UInt32(spendingTransaction.outputs.size)) {
      logger.warn("When we have a SIGHASH_SINGLE we cannot have more inputs than outputs")
      errorHash
    } else {
      val serializedTxForSignature = serializeForSignature(spendingTransaction, inputIndex, script, hashType)
      logger.debug("Serialized tx for signature: " + BitcoinSUtil.encodeHex(serializedTxForSignature))
      logger.debug("HashType: " + hashType.num)
      CryptoUtil.doubleSHA256(serializedTxForSignature)
    }
  }

  /**
   * Implements the new serialization algorithm defined in BIP143
   * [[https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki]]
   * [[https://github.com/bitcoin/bitcoin/blob/f8528134fc188abc5c7175a19680206964a8fade/src/script/interpreter.cpp#L1113]]
   */
  def serializeForSignature(spendingTx: Transaction, inputIndex: UInt32, script: Seq[ScriptToken], hashType: HashType,
                            amount: CurrencyUnit, signatureVersion: SignatureVersion): Seq[Byte] = signatureVersion match {
    case SigVersionBase =>
      serializeForSignature(spendingTx, inputIndex, script, hashType)
    case SigVersionWitnessV0 =>
      val isNotAnyoneCanPay = !HashType.isAnyoneCanPay(hashType)
      val isNotSigHashSingle = !(HashType.isSIGHASH_SINGLE(hashType.num))
      val isNotSigHashNone = !(HashType.isSIGHASH_NONE(hashType.num))
      val inputIndexInt = inputIndex.toInt
      val emptyHash = CryptoUtil.emptyDoubleSha256Hash

      val outPointHash: Seq[Byte] = if (isNotAnyoneCanPay) {
        val bytes: Seq[Byte] = spendingTx.inputs.flatMap(_.previousOutput.bytes)
        CryptoUtil.doubleSHA256(bytes).bytes
      } else emptyHash.bytes

      val sequenceHash: Seq[Byte] = if (isNotAnyoneCanPay && isNotSigHashNone && isNotSigHashSingle) {
        val bytes = spendingTx.inputs.flatMap(_.sequence.bytes)
        CryptoUtil.doubleSHA256(bytes).bytes
      } else emptyHash.bytes

      val outputHash: Seq[Byte] = if (isNotSigHashSingle && isNotSigHashNone) {
        val bytes = spendingTx.outputs.flatMap(o => o.bytes)
        CryptoUtil.doubleSHA256(bytes).bytes
      } else if (HashType.isSIGHASH_SINGLE(hashType.num) && inputIndex < UInt32(spendingTx.outputs.size)) {
        val output = spendingTx.outputs(inputIndexInt)
        val bytes = CryptoUtil.doubleSHA256(RawTransactionOutputParser.write(output)).bytes
        bytes
      } else emptyHash.bytes

      val scriptBytes = script.flatMap(_.bytes)

      val i = spendingTx.inputs(inputIndexInt)
      val serializationForSig: Seq[Byte] = spendingTx.version.bytes.reverse ++ outPointHash ++ sequenceHash ++
        i.previousOutput.bytes ++ CompactSizeUInt.calc(scriptBytes).bytes ++
        scriptBytes ++ amount.bytes ++ i.sequence.bytes.reverse ++
        outputHash ++ spendingTx.lockTime.bytes.reverse ++ hashType.num.bytes.reverse
      logger.debug("Serialization for signature for WitnessV0Sig: " + BitcoinSUtil.encodeHex(serializationForSig))
      serializationForSig
  }

  /**
   * Hashing function for a [[SigVersionWitnessV0]] as specified by BIP143
   * [[https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki]]
   * NOTE: This covers the amount of [[CurrencyUnit]] we are spending in the output
   */
  def hashForSignature(spendingTx: Transaction, inputIndex: UInt32, script: Seq[ScriptToken], hashType: HashType,
                       amount: CurrencyUnit, sigVersion: SignatureVersion): DoubleSha256Digest = {

    val serialization = serializeForSignature(spendingTx, inputIndex, script, hashType, amount, sigVersion)
    CryptoUtil.doubleSHA256(serialization)
  }
  /** Wrapper function for hashForSignature. */
  def hashForSignature(txSigComponent: TxSigComponent, hashType: HashType): DoubleSha256Digest = {
    val script = BitcoinScriptUtil.calculateScriptForSigning(txSigComponent, txSigComponent.scriptPubKey.asm)
    txSigComponent match {
      case t: BaseTxSigComponent =>
        hashForSignature(t.transaction, t.inputIndex, script, hashType)
      case w: WitnessTxSigComponent =>
        hashForSignature(w.transaction, w.inputIndex, script, hashType, w.amount, w.sigVersion)
      case r: WitnessTxSigComponentRebuilt =>
        hashForSignature(r.transaction, r.inputIndex, script, hashType, r.amount, r.sigVersion)
    }
  }

  /** Sets the input's sequence number to zero EXCEPT for the input at inputIndex. */
  private def setSequenceNumbersZero(inputs: Seq[TransactionInput], inputIndex: UInt32): Seq[TransactionInput] = for {
    (input, index) <- inputs.zipWithIndex
  } yield {
    if (UInt32(index) == inputIndex) input
    else TransactionInput(input.previousOutput, input.scriptSignature, UInt32.zero)
  }

  /** Executes the [[SIGHASH_NONE]] procedure on a spending transaction for the input specified by inputIndex. */
  private def sigHashNone(spendingTransaction: Transaction, inputIndex: UInt32): Transaction = {
    //following this implementation from bitcoinj
    //[[https://github.com/bitcoinj/bitcoinj/blob/09a2ca64d2134b0dcbb27b1a6eb17dda6087f448/core/src/main/java/org/bitcoinj/core/Transaction.java#L957]]
    //means that no outputs are signed at all
    //set the sequence number of all inputs to 0 EXCEPT the input at inputIndex
    val updatedInputs: Seq[TransactionInput] = setSequenceNumbersZero(spendingTransaction.inputs, inputIndex)
    val sigHashNoneTx = BaseTransaction(spendingTransaction.version, updatedInputs, Nil, spendingTransaction.lockTime)
    //append hash type byte onto the end of the tx bytes
    sigHashNoneTx
  }

  /** Executes the [[SIGHASH_SINGLE]] procedure on a spending transaction for the input specified by inputIndex */
  private def sigHashSingle(spendingTransaction: Transaction, inputIndex: UInt32): Transaction = {
    //following this implementation from bitcoinj
    //[[https://github.com/bitcoinj/bitcoinj/blob/09a2ca64d2134b0dcbb27b1a6eb17dda6087f448/core/src/main/java/org/bitcoinj/core/Transaction.java#L964]]
    // In SIGHASH_SINGLE the outputs after the matching input index are deleted, and the outputs before
    // that position are "nulled out". Unintuitively, the value in a "null" transaction is set to -1.
    val updatedOutputsOpt: Seq[Option[TransactionOutput]] = for {
      (output, index) <- spendingTransaction.outputs.zipWithIndex
    } yield {
      if (UInt32(index) < inputIndex) {
        logger.debug("Updating tx output to null in bitcoin core")
        Some(EmptyTransactionOutput)
      } else if (UInt32(index) == inputIndex) Some(output)
      else None
    }
    val updatedOutputs: Seq[TransactionOutput] = updatedOutputsOpt.flatten

    //create blank inputs with sequence numbers set to zero EXCEPT
    //the input at the inputIndex
    val updatedInputs: Seq[TransactionInput] = setSequenceNumbersZero(spendingTransaction.inputs, inputIndex)
    val sigHashSingleTx = BaseTransaction(spendingTransaction.version, updatedInputs, updatedOutputs, spendingTransaction.lockTime)
    sigHashSingleTx
  }

  /** Executes the [[SIGHASH_ALL]] procedure on a spending transaction at inputIndex. */
  private def sigHashAll(spendingTransaction: Transaction, inputIndex: UInt32): Transaction = {
    spendingTransaction
  }

  /** Executes the [[SIGHASH_ANYONECANPAY]] procedure on a spending transaction at inputIndex. */
  private def sigHashAnyoneCanPay(spendingTransaction: Transaction, input: TransactionInput): Transaction = {
    BaseTransaction(spendingTransaction.version, Seq(input), spendingTransaction.outputs, spendingTransaction.lockTime)
  }

  /** Removes [[OP_CODESEPARATOR]] operations then returns the script. */
  def removeOpCodeSeparators(script: Seq[ScriptToken]): Seq[ScriptToken] = {
    if (script.contains(OP_CODESEPARATOR)) {
      //TODO: This needs to be tested
      val scriptWithoutOpCodeSeparators: Seq[ScriptToken] = script.filterNot(_ == OP_CODESEPARATOR)
      scriptWithoutOpCodeSeparators
    } else script
  }
}

object TransactionSignatureSerializer extends TransactionSignatureSerializer
