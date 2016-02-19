package org.scalacoin.crypto

import org.scalacoin.marshallers.RawBitcoinSerializerHelper
import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKey}
import org.scalacoin.protocol.transaction.{Transaction, TransactionOutput, TransactionInput}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.script.crypto._

/**
 * Created by chris on 2/16/16.
 * A trait used to serialize various components of a bitcoin transaction for
 * hashing to compare against a digital signature
 * https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/interpreter.cpp#L1016-L1105
 */
trait TransactionSignatureSerializer extends RawBitcoinSerializerHelper {

  /**
   * Serialized the passed in script code, skipping OP_CODESEPARATORs
   * definition for CScript https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/script.h#L373
   * @param script
   * @return
   */
  def serializeScriptCode(script : Seq[ScriptToken]) : String = {
    val serializedScript : String = removeOpCodeSeparators(script)
    serializedScript
  }

  def serializeInput(input : TransactionInput, nType : Int, nVersion : Int) : String = ???

  /**
   * Serializes an output of a transaction
   * https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/interpreter.cpp#L1079
   * @param output
   * @param nType
   * @param nVersion
   * @return
   */
  def serializeOutput(output : TransactionOutput, nType : Int, nVersion : Int) : String = ???

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
      output <- spendingTransaction.outputs
    } yield serializeOutput(output, nType,nVersion)

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


object TransactionSignatureSerializer extends TransactionSignatureSerializer
