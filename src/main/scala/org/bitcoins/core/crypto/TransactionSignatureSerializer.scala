package org.bitcoins.core.crypto

import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.serializers.RawBitcoinSerializerHelper
import org.bitcoins.core.serializers.transaction.RawTransactionOutputParser
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.script.stack.OP_DUP
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil, BitcoinSUtil, CryptoUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 2/16/16.
 * Wrapper that serializes like Transaction, but with the modifications
 * required for the signature hash done
 * https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/interpreter.cpp#L1016-L1105
 * bitcoinj version of this
 * https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/org/bitcoinj/core/Transaction.java#L924-L1008
 */
trait TransactionSignatureSerializer extends RawBitcoinSerializerHelper with BitcoinSLogger {

  /**
   * Bitcoin Core's bug is that SignatureHash was supposed to return a hash and on this codepath it
   * actually returns the constant "1" to indicate an error
   * @return
   */
  private def errorHash : DoubleSha256Digest = DoubleSha256Digest(BitcoinSUtil.decodeHex("0100000000000000000000000000000000000000000000000000000000000000"))

  /**
   * Serializes a transaction to be signed by an ECKey
   * follows the bitcoinj implementation which can be found here
   * hashing is done in the hashForSignature function
   * @param inputIndex
   * @param script
   * @param hashType
   * @return
   */
  def serializeForSignature(spendingTransaction : Transaction, inputIndex : UInt32, script : Seq[ScriptToken], hashType : HashType) : Seq[Byte] = {
    logger.debug("Serializing for signature")
    logger.debug("Script: " + script)
    // Clear input scripts in preparation for signing. If we're signing a fresh
    // transaction that step isn't very helpful, but it doesn't add much cost relative to the actual
    // EC math so we'll do it anyway.
    val inputSigsRemoved = for {
      input <- spendingTransaction.inputs
    } yield TransactionInput(input,ScriptSignature.empty)

    inputSigsRemoved.map(input =>
      require(input.scriptSignature.bytes.size == 0,"Input byte size was " + input.scriptSignature.bytes))

    // This step has no purpose beyond being synchronized with Bitcoin Core's bugs. OP_CODESEPARATOR
    // is a legacy holdover from a previous, broken design of executing scripts that shipped in Bitcoin 0.1.
    // It was seriously flawed and would have let anyone take anyone elses money. Later versions switched to
    // the design we use today where scripts are executed independently but share a stack. This left the
    // OP_CODESEPARATOR instruction having no purpose as it was only meant to be used internally, not actually
    // ever put into scripts. Deleting OP_CODESEPARATOR is a step that should never be required but if we don't
    // do it, we could split off the main chain.
    logger.info("Before Bitcoin-S Script to be connected: " + script)
    val scriptWithOpCodeSeparatorsRemoved : Seq[ScriptToken] = removeOpCodeSeparators(script)

    logger.info("After Bitcoin-S Script to be connected: " + scriptWithOpCodeSeparatorsRemoved)

    val inputToSign = inputSigsRemoved(inputIndex.toInt)

    // Set the input to the script of its output. Bitcoin Core does this but the step has no obvious purpose as
    // the signature covers the hash of the prevout transaction which obviously includes the output script
    // already. Perhaps it felt safer to him in some way, or is another leftover from how the code was written.
    val inputWithConnectedScript = TransactionInput(inputToSign,scriptWithOpCodeSeparatorsRemoved)

    //update the input at index i with inputWithConnectScript
    val updatedInputs = for {
      (input,index) <- inputSigsRemoved.zipWithIndex
    } yield {
        if (UInt32(index) == inputIndex) {
          inputWithConnectedScript
        }
        else input
      }

    val txWithInputSigsRemoved = Transaction(spendingTransaction,UpdateTransactionInputs(updatedInputs))
    val sigHashBytes : List[Byte] = hashType.num.bytes.reverse.toList
    //check the hash type
    hashType match {
      case _ : SIGHASH_NONE =>
        val sigHashNoneTx : Transaction = sigHashNone(txWithInputSigsRemoved,inputIndex)
        sigHashNoneTx.bytes ++ sigHashBytes

      case _ : SIGHASH_SINGLE =>
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
          val sigHashSingleTx = sigHashSingle(txWithInputSigsRemoved,inputIndex)
          sigHashSingleTx.bytes ++ sigHashBytes
        }

      case _ : SIGHASH_ALL =>
        val sigHashAllTx : Transaction = sigHashAll(txWithInputSigsRemoved,inputIndex)
        sigHashAllTx.bytes ++ sigHashBytes

      case _ : SIGHASH_ANYONECANPAY =>
        val txWithInputsRemoved = sigHashAnyoneCanPay(txWithInputSigsRemoved,inputWithConnectedScript)
        txWithInputsRemoved.bytes ++ sigHashBytes

      case _ : SIGHASH_ALL_ANYONECANPAY =>
        val sigHashAllTx = sigHashAll(txWithInputSigsRemoved,inputIndex)
        val sigHashAllAnyoneCanPayTx = sigHashAnyoneCanPay(sigHashAllTx,inputWithConnectedScript)
        sigHashAllAnyoneCanPayTx.bytes ++ sigHashBytes

      case _ : SIGHASH_NONE_ANYONECANPAY =>
        val sigHashNoneTx = sigHashNone(txWithInputSigsRemoved,inputIndex)
        val sigHashNoneAnyoneCanPay = sigHashAnyoneCanPay(sigHashNoneTx,inputWithConnectedScript)
        sigHashNoneAnyoneCanPay.bytes ++ sigHashBytes

      case _ : SIGHASH_SINGLE_ANYONECANPAY =>
        val sigHashSingleTx = sigHashSingle(txWithInputSigsRemoved,inputIndex)
        val sigHashSingleAnyoneCanPay = sigHashAnyoneCanPay(sigHashSingleTx,inputWithConnectedScript)
        sigHashSingleAnyoneCanPay.bytes  ++ sigHashBytes
    }
  }


  /**
    * Serializes then hashes a transaction for signing
    * this is an implementation of it's bitcoinj equivalent found here
    * https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/org/bitcoinj/core/Transaction.java#L924
    * @param spendingTransaction the transaction we are hashing
    * @param inputIndex the inputIndex we are hashing for signing
    * @param script the script which we are spending
    * @param hashType the hash type we are serializign this tx for
    * @return
    */
  def hashForSignature(spendingTransaction : Transaction, inputIndex : UInt32, script : Seq[ScriptToken], hashType : HashType) : DoubleSha256Digest = {
    //these first two checks are in accordance with behavior in bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L1112-L1123
    if (inputIndex >= UInt32(spendingTransaction.inputs.size)) {
      logger.warn("Our inputIndex is out of the range of the inputs in the spending transaction")
      errorHash
    } else if(HashType.isSIGHASH_SINGLE(hashType.num) && inputIndex >= UInt32(spendingTransaction.outputs.size)) {
      logger.warn("When we have a SIGHASH_SINGLE we cannot have more inputs than outputs")
      errorHash
    } else {
      val serializedTxForSignature = serializeForSignature(spendingTransaction,inputIndex,script,hashType)
      logger.debug("Serialized tx for signature: " + BitcoinSUtil.encodeHex(serializedTxForSignature))
      logger.debug("HashType: " + hashType.num)
      CryptoUtil.doubleSHA256(serializedTxForSignature)
    }
  }

  /**
    * Wrapper function for hashForSignature
    * @param txSignatureComponent this contains the transaction and inputIndex for hashForSignature
    * @param hashType
    * @return
    */
  def hashForSignature(txSignatureComponent: TransactionSignatureComponent, hashType: HashType): DoubleSha256Digest = {
    hashForSignature(txSignatureComponent.transaction,txSignatureComponent.inputIndex,
      txSignatureComponent.scriptPubKey.asm,hashType)
  }



  /**
   * Sets the input's sequence number to zero EXCEPT for the input at inputIndex
   * @param inputs
   * @param inputIndex
   * @return
   */
  private def setSequenceNumbersZero(inputs : Seq[TransactionInput], inputIndex : UInt32) : Seq[TransactionInput] =  {
    for {
      (input,index) <- inputs.zipWithIndex
    } yield {
      if (UInt32(index) == inputIndex) input
      else TransactionInput(input,UInt32.zero)
    }
  }

  /**
   * Executes the SIGHASH_NONE procedure on a spending transaction for the input specified by inputIndex
   * @param spendingTransaction
   * @param inputIndex
   * @return
   */
  private def sigHashNone(spendingTransaction : Transaction, inputIndex : UInt32) : Transaction = {
    //following this implementation from bitcoinj
    //https://github.com/bitcoinj/bitcoinj/blob/09a2ca64d2134b0dcbb27b1a6eb17dda6087f448/core/src/main/java/org/bitcoinj/core/Transaction.java#L957
    //means that no outputs are signed at all
    val txWithNoOutputs = Transaction.emptyOutputs(spendingTransaction)
    //set the sequence number of all inputs to 0 EXCEPT the input at inputIndex
    val updatedInputs :  Seq[TransactionInput] = setSequenceNumbersZero(spendingTransaction.inputs,inputIndex)
    val sigHashNoneTx = Transaction(txWithNoOutputs,UpdateTransactionInputs(updatedInputs))
    //append hash type byte onto the end of the tx bytes
    sigHashNoneTx
  }

  /**
   * Executes the SIGHASH_SINGLE procedure on a spending transaction for the input specified by inputIndex
   * @param spendingTransaction
   * @param inputIndex
   * @return
   */
  private def sigHashSingle(spendingTransaction : Transaction, inputIndex : UInt32) : Transaction = {
    //following this implementation from bitcoinj
    //https://github.com/bitcoinj/bitcoinj/blob/09a2ca64d2134b0dcbb27b1a6eb17dda6087f448/core/src/main/java/org/bitcoinj/core/Transaction.java#L964
    // In SIGHASH_SINGLE the outputs after the matching input index are deleted, and the outputs before
    // that position are "nulled out". Unintuitively, the value in a "null" transaction is set to -1.
    val updatedOutputsOpt : Seq[Option[TransactionOutput]] = for {
      (output,index) <- spendingTransaction.outputs.zipWithIndex
    } yield {
      if (UInt32(index) < inputIndex) {
        logger.debug("Updating tx output to null in bitcoin core")
        Some(EmptyTransactionOutput)
      }
      else if (UInt32(index) == inputIndex) Some(output)
      else None
    }
    val updatedOutputs : Seq[TransactionOutput] = updatedOutputsOpt.flatten

    val spendingTxOutputsEmptied = Transaction(spendingTransaction,UpdateTransactionOutputs(updatedOutputs))
    //create blank inputs with sequence numbers set to zero EXCEPT
    //the input at the inputIndex
    val updatedInputs : Seq[TransactionInput] = setSequenceNumbersZero(spendingTxOutputsEmptied.inputs,inputIndex)

    val sigHashSingleTx = Transaction(spendingTxOutputsEmptied,UpdateTransactionInputs(updatedInputs))
    sigHashSingleTx
  }

  /**
   * Executes the SIGHASH_ALL procedure on a spending transaction at inputIndex
   * @param spendingTransaction
   * @param inputIndex
   * @return
   */
  private def sigHashAll(spendingTransaction : Transaction, inputIndex : UInt32) : Transaction = {
    spendingTransaction
  }

  /**
   * Executes the SIGHASH_ANYONECANPAY procedure on a spending transaction at inputIndex
   * @param spendingTransaction
   * @param input
   * @return
   */
  private def sigHashAnyoneCanPay(spendingTransaction : Transaction, input : TransactionInput) : Transaction = {
    val txWithEmptyInputs = Transaction.emptyInputs(spendingTransaction)
    val txWithInputsRemoved = Transaction(txWithEmptyInputs,UpdateTransactionInputs(Seq(input)))
    txWithInputsRemoved
  }

  /**
    * Removes OP_CODESEPARATOR operations then returns the script
    * format
    * @return
    */
  def removeOpCodeSeparators(script : Seq[ScriptToken]) : Seq[ScriptToken] = {
    logger.info("Tokens: " + script)
    if (script.contains(OP_CODESEPARATOR)) {
      //TODO: This needs to be tested
      val scriptWithoutOpCodeSeparators : Seq[ScriptToken] = script.filterNot(_ == OP_CODESEPARATOR)
      scriptWithoutOpCodeSeparators
    } else script
  }
}

object TransactionSignatureSerializer extends TransactionSignatureSerializer
