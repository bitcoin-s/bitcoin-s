package org.scalacoin.crypto

import org.scalacoin.currency.CurrencyUnits
import org.scalacoin.marshallers.RawBitcoinSerializerHelper
import org.scalacoin.marshallers.transaction.RawTransactionOutputParser
import org.scalacoin.protocol.script.{ScriptSignatureFactory, ScriptSignatureImpl, ScriptPubKeyFactory, ScriptPubKey}
import org.scalacoin.protocol.transaction._
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.script.crypto._
import org.scalacoin.util.{ScalacoinUtil, CryptoUtil}

/**
 * Created by chris on 2/16/16.
 * Wrapper that serializes like Transaction, but with the modifications
 * required for the signature hash done in-place
 * https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/interpreter.cpp#L1016-L1105
 * bitcoinj version of this
 * https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/org/bitcoinj/core/Transaction.java#L924-L1008
 */
trait TransactionSignatureSerializer extends RawBitcoinSerializerHelper {

  def spendingTransaction : Transaction

  /**
   * Bitcoin Core's bug is that SignatureHash was supposed to return a hash and on this codepath it
   * actually returns the constant "1" to indicate an error
   * @return
   */
  private def errorHash : Seq[Byte] = ScalacoinUtil.decodeHex("0100000000000000000000000000000000000000000000000000000000000000")

  /**
   * Serialized the passed in script code, skipping OP_CODESEPARATORs
   * definition for CScript https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/script.h#L373
   * @param script
   * @return
   */
  def serializeScriptCode(script : ScriptPubKey) : ScriptPubKey = removeOpCodeSeparators(script)


  /**
   * Serializes a transaction to be signed by an ECKey
   * follows the bitcoinj implementation which can be found here
   * hashing is done in the hashForSignature function
   * hashing is NOT done in this function
   * @param inputIndex
   * @param script
   * @param hashType
   * @return
   */
  def serializeForSignature(inputIndex : Int, script : ScriptPubKey, hashType : HashType) : Seq[Byte] = {
    // Clear input scripts in preparation for signing. If we're signing a fresh
    // transaction that step isn't very helpful, but it doesn't add much cost relative to the actual
    // EC math so we'll do it anyway.
    val inputSigsRemoved = for {
      input <- spendingTransaction.inputs
    } yield input.factory(ScriptSignatureFactory.empty)

    inputSigsRemoved.map(input =>
      require(input.scriptSignature.bytes.size == 0,"Input byte size was " + input.scriptSignature.bytes))

    // This step has no purpose beyond being synchronized with Bitcoin Core's bugs. OP_CODESEPARATOR
    // is a legacy holdover from a previous, broken design of executing scripts that shipped in Bitcoin 0.1.
    // It was seriously flawed and would have let anyone take anyone elses money. Later versions switched to
    // the design we use today where scripts are executed independently but share a stack. This left the
    // OP_CODESEPARATOR instruction having no purpose as it was only meant to be used internally, not actually
    // ever put into scripts. Deleting OP_CODESEPARATOR is a step that should never be required but if we don't
    // do it, we could split off the main chain.
    val scriptWithOpCodeSeparatorsRemoved : ScriptPubKey = removeOpCodeSeparators(script)

    val inputToSign = inputSigsRemoved(inputIndex)

    // Set the input to the script of its output. Bitcoin Core does this but the step has no obvious purpose as
    // the signature covers the hash of the prevout transaction which obviously includes the output script
    // already. Perhaps it felt safer to him in some way, or is another leftover from how the code was written.
    val inputWithConnectedScript = inputToSign.factory(scriptWithOpCodeSeparatorsRemoved)

    //update the input at index i with inputWithConnectScript
    val updatedInputs = for {
      (input,index) <- inputSigsRemoved.zipWithIndex
    } yield {
        if (index == inputIndex) inputWithConnectedScript
        else input
      }

    val txWithInputSigsRemoved = spendingTransaction.factory(UpdateTransactionInputs(updatedInputs))

    //just need to add the hash type and hash the tx
    //txWithInputSigsRemoved.bytes
    val sigHashBytes : List[Byte] = List(0x00.toByte, 0x00.toByte, 0x00.toByte, hashType.byte).reverse

    //check the hash type
    hashType match {
      case SIGHASH_NONE =>
        //following this implementation from bitcoinj
        //https://github.com/bitcoinj/bitcoinj/blob/09a2ca64d2134b0dcbb27b1a6eb17dda6087f448/core/src/main/java/org/bitcoinj/core/Transaction.java#L957
        //means that no outputs are signed at all
        val txWithNoOutputs = txWithInputSigsRemoved.emptyOutputs
        //set the sequence number of all inputs to 0 EXCEPT the input at inputIndex
        val updatedInputs :  Seq[TransactionInput] = setSequenceNumbersZero(txWithInputSigsRemoved.inputs,inputIndex)
        val sigHashNoneTx = txWithNoOutputs.factory(UpdateTransactionInputs(updatedInputs))
        //append hash type byte onto the end of the tx bytes
        sigHashNoneTx.bytes ++ sigHashBytes
      case SIGHASH_SINGLE =>
        //following this implementation from bitcoinj
        //https://github.com/bitcoinj/bitcoinj/blob/09a2ca64d2134b0dcbb27b1a6eb17dda6087f448/core/src/main/java/org/bitcoinj/core/Transaction.java#L964
        if (inputIndex >= spendingTransaction.outputs.size) {
          // comment copied from bitcoinj
          // The input index is beyond the number of outputs, it's a buggy signature made by a broken
          // Bitcoin implementation. Bitcoin Core also contains a bug in handling this case:
          // any transaction output that is signed in this case will result in both the signed output
          // and any future outputs to this public key being steal-able by anyone who has
          // the resulting signature and the public key (both of which are part of the signed tx input).

          // Bitcoin Core's bug is that SignatureHash was supposed to return a hash and on this codepath it
          // actually returns the constant "1" to indicate an error, which is never checked for. Oops.
          errorHash
        } else {
          // In SIGHASH_SINGLE the outputs after the matching input index are deleted, and the outputs before
          // that position are "nulled out". Unintuitively, the value in a "null" transaction is set to -1.
          val updatedOutputsOpt : Seq[Option[TransactionOutput]] = for {
            (output,index) <- txWithInputSigsRemoved.outputs.zipWithIndex
          } yield {
              if (index < inputIndex) Some(output.empty.factory(CurrencyUnits.negativeSatoshi))
              else None
            }
          val updatedOutputs : Seq[TransactionOutput] = updatedOutputsOpt.flatten

          val spendingTxOutputsEmptied = txWithInputSigsRemoved.factory(UpdateTransactionOutputs(updatedOutputs))
          //create blank inputs with sequence numbers set to zero EXCEPT
          //the input at the inputIndex
          val updatedInputs = setSequenceNumbersZero(spendingTxOutputsEmptied.inputs,inputIndex)

          val sigHashSingleTx = spendingTxOutputsEmptied.factory(UpdateTransactionInputs(updatedInputs))
          //append hash type byte onto the end of the tx bytes
          sigHashSingleTx.bytes ++ sigHashBytes
        }
      case SIGHASH_ALL =>
        txWithInputSigsRemoved.bytes ++ sigHashBytes

      case SIGHASH_ANYONECANPAY =>
        val txWithInputsRemoved = txWithInputSigsRemoved.emptyInputs.factory(UpdateTransactionInputs(Seq(inputWithConnectedScript)))
        txWithInputsRemoved.bytes ++ sigHashBytes
    }

  }


  /**
   * Serializes then hashes a transaction for signing
   * this is an implementation of it's bitcoinj equivalent found here
   * https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/org/bitcoinj/core/Transaction.java#L924
   * @param inputIndex
   * @param script
   * @param hashType
   * @return
   */
  def hashForSignature(inputIndex : Int, script : ScriptPubKey, hashType : HashType) : Seq[Byte] = {
    val serializedTxForSignature = serializeForSignature(inputIndex,script,hashType)
    CryptoUtil.doubleSHA256(serializedTxForSignature)
  }

  /**
   * Removes OP_CODESEPARATOR operations then returns the script in hex
   * format
   * @return
   */
  def removeOpCodeSeparators(script : ScriptPubKey) : ScriptPubKey = {
    val scriptWithoutOpCodeSeparators : Seq[ScriptToken] = script.asm.filterNot(_ == OP_CODESEPARATOR)
    ScriptPubKeyFactory.factory(scriptWithoutOpCodeSeparators.flatMap(_.bytes))
  }

  /**
   * Sets the input's sequence number to zero EXCEPT for the input at inputIndex
   * @param inputs
   * @param inputIndex
   * @return
   */
  private def setSequenceNumbersZero(inputs : Seq[TransactionInput], inputIndex : Int) : Seq[TransactionInput] =  {
    for {
      (input,index) <- inputs.zipWithIndex
    } yield {
      if (index == inputIndex) input
      else input.factory(0)
    }
  }
}


class BaseTransactionSignatureSerializer(override val spendingTransaction : Transaction) extends TransactionSignatureSerializer
