package org.scalacoin.crypto

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
  def serializeScriptCode(script : Seq[ScriptToken]) : Seq[Byte] = removeOpCodeSeparators(script)


  def serializeInput(input : TransactionInput, nType : Int, nVersion : Int) : String = ???

  /**
   * Serializes an output of a transaction
   * https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/interpreter.cpp#L1079
   * @param output
   * @param nType
   * @param nVersion
   * @return
   */
  def serializeOutput(output : TransactionOutput, nType : Int, nVersion : Int,
                      hashType : HashType, inputIndex : Int, outputIndex : Int ) : String = {
    //check if it is a SIGHASH_SINGLE
    //check if the output index is not the same as the input index
    if (hashType == SIGHASH_SINGLE && inputIndex != outputIndex) {
      // Do not lock-in the txout payee at other indices as txin
      //::Serialize(s, CTxOut(), nType, nVersion)
      //need to write an empty output i think
      ""
    } else {
      RawTransactionOutputParser.write(output)
    }


  }

  def serialize(inputIndex : Int, nType : Int, script : ScriptPubKey, hashType : HashType) : Seq[Byte] = {
    // Clear input scripts in preparation for signing. If we're signing a fresh
    // transaction that step isn't very helpful, but it doesn't add much cost relative to the actual
    // EC math so we'll do it anyway.
    val txWithInputSigsRemoved = for {
      input <- spendingTransaction.inputs
    } yield input.factory(ScriptSignatureFactory.empty)


    // This step has no purpose beyond being synchronized with Bitcoin Core's bugs. OP_CODESEPARATOR
    // is a legacy holdover from a previous, broken design of executing scripts that shipped in Bitcoin 0.1.
    // It was seriously flawed and would have let anyone take anyone elses money. Later versions switched to
    // the design we use today where scripts are executed independently but share a stack. This left the
    // OP_CODESEPARATOR instruction having no purpose as it was only meant to be used internally, not actually
    // ever put into scripts. Deleting OP_CODESEPARATOR is a step that should never be required but if we don't
    // do it, we could split off the main chain.
    val scriptWithOpCodeSeparatorsRemoved : Seq[Byte] = serializeScriptCode(script.asm)

    val inputToSign = spendingTransaction.inputs(inputIndex)

    // Set the input to the script of its output. Bitcoin Core does this but the step has no obvious purpose as
    // the signature covers the hash of the prevout transaction which obviously includes the output script
    // already. Perhaps it felt safer to him in some way, or is another leftover from how the code was written.
    val inputWithConnectedScript = inputToSign.factory(script)

    //check the hash type

    hashType match {
      case SIGHASH_NONE =>
        //means that no outputs are signed at all
        val txWithNoOutputs = spendingTransaction.emptyOutputs
        //set the sequence number of all inputs to 0 EXCEPT the input at inputIndex
        val updatedInputs :  Seq[TransactionInput] = for {
          (input,index) <- spendingTransaction.inputs.zipWithIndex
        } yield {
            if (index == inputIndex) input
            else input.factory(0)
          }
        val updatedTransactionWithInputsNoOutputs = txWithNoOutputs.factory(UpdateTransactionInputs(updatedInputs))

        CryptoUtil.doubleSHA256(updatedTransactionWithInputsNoOutputs.bytes)
      case SIGHASH_SINGLE =>
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

          //create blank outputs with sequence numbers set to zero EXCEPT
          //the output at the inputIndex
          //val outputs = spendingTransaction.outputs.map(_)

        }
    }
    ???

  }

  /**
   * Removes OP_CODESEPARATOR operations then returns the script in hex
   * format
   * @return
   */
  def removeOpCodeSeparators(script : Seq[ScriptToken]) : Seq[Byte] = {
    val scriptWithoutOpCodeSeparators : String = script.filterNot(_ == OP_CODESEPARATOR).map(_.hex).mkString
    val scriptWithoutOpCodeSeparatorSize = addPrecedingZero((scriptWithoutOpCodeSeparators.size / 2).toHexString)
    val expectedScript : ScriptPubKey = ScriptPubKeyFactory.factory(
      scriptWithoutOpCodeSeparatorSize + scriptWithoutOpCodeSeparators)
    expectedScript.bytes
  }
}


class BaseTransactionSignatureSerializer(override val spendingTransaction : Transaction) extends TransactionSignatureSerializer
