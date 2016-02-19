package org.scalacoin.crypto

import org.scalacoin.marshallers.RawBitcoinSerializerHelper
import org.scalacoin.marshallers.transaction.RawTransactionOutputParser
import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKey}
import org.scalacoin.protocol.transaction.{Transaction, TransactionOutput, TransactionInput}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.script.crypto._

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
   * Serialized the passed in script code, skipping OP_CODESEPARATORs
   * definition for CScript https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/script.h#L373
   * @param script
   * @return
   */
  def serializeScriptCode(script : Seq[ScriptToken]) : String = removeOpCodeSeparators(script)


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

  def serialize(spendingTransaction : Transaction, nIn : Int, nType : Int, nVersion : Int, nHashTypeIn : HashType) : String = {
    val serializedVersion =spendingTransaction.version.toHexString
    val serializedNIn = nHashTypeIn match {
      case SIGHASH_ANYONECANPAY => "01"
      case _ => addPrecedingZero(spendingTransaction.inputs.size.toHexString)
    }
    val serializedInputs = for {
      input <- spendingTransaction.inputs
    } yield serializeInput(input,nType,nVersion)

    val serializedNOut = nHashTypeIn match {
      case SIGHASH_NONE => "00"
      case SIGHASH_SINGLE => addPrecedingZero(nIn.toHexString + 1)
      case _ => addPrecedingZero(spendingTransaction.outputs.size.toHexString)
    }

    val serializedOutputs = for {
      (output,index) <- spendingTransaction.outputs.zipWithIndex
    } yield serializeOutput(output, nType,nVersion,SIGHASH_NONE,-1,-1)

    val serializedLockTime = addPrecedingZero(spendingTransaction.lockTime.toHexString)

    serializedVersion + serializedNIn + serializedInputs.mkString + serializedNOut +
      serializedOutputs.mkString + serializedLockTime
  }

  /**
   * Removes OP_CODESEPARATOR operations then returns the script in hex
   * format
   * @return
   */
  protected def removeOpCodeSeparators(script : Seq[ScriptToken]) : String = {
    val scriptWithoutOpCodeSeparators : String = script.filterNot(_ == OP_CODESEPARATOR).map(_.hex).mkString
    val scriptWithoutOpCodeSeparatorSize = addPrecedingZero((scriptWithoutOpCodeSeparators.size / 2).toHexString)
    val expectedScript : ScriptPubKey = ScriptPubKeyFactory.factory(
      scriptWithoutOpCodeSeparatorSize + scriptWithoutOpCodeSeparators)
    expectedScript.hex
  }
}


class TransactionSignatureSerializerr(override val spendingTransaction : Transaction) extends TransactionSignatureSerializer
