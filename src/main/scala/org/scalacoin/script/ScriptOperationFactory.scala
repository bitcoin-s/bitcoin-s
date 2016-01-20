package org.scalacoin.script

import org.scalacoin.script.arithmetic.ArithmeticOperationsFactory
import org.scalacoin.script.bitwise.BitwiseOperationsFactory
import org.scalacoin.script.constant._
import org.scalacoin.script.control.ControlOperationsFactory
import org.scalacoin.script.crypto.CryptoOperationFactory
import org.scalacoin.script.locktime.LocktimeOperationFactory
import org.scalacoin.script.stack.StackOperationFactory
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/8/16.
 * Responsible for matching script op codes with their given
 * hexadecimal representation or byte representation
 */
trait ScriptOperationFactory[T <: ScriptOperation] extends ScalacoinUtil {

  /**
   * All of the script operations for a particular T
   * @tparam T
   * @return
   */
  def operations : Seq[T]

  /**
   * Finds a script operation from a given string
   * @param str
   * @return
   */
  def fromString(str : String) : Option[T] = {
    val result : Option[T] = operations.find(_.toString == str)
    if (result.isEmpty) {
      //try and remove the 'OP_' prefix on the operations and see if it matches anything.
      operations.find(op => removeOP_Prefix(op.toString) == removeOP_Prefix(str))
    } else result
  }

  /**
   * Finds a script operation from its hexadecimal representation
   * @param hex
   * @return
   */
  def fromHex(hex : String) : Option[T] = operations.find(_.hex == hex)

  /**
   * Removes the 'OP_' prefix from a given operation.
   * Example: OP_EQUALVERIFY would be transformed into EQUALVERIFY
   * @param str
   * @return
   */
  private def removeOP_Prefix(str : String) : String = {
    str.replace("OP_","")
  }

  /**
   * Finds a script operation from a given byte
   * @param byte
   * @return
   */
  def fromByte(byte : Byte) : Option[T] = {
    val hex = encodeHex(byte)
    fromHex(hex)
  }
}


object ScriptOperationFactory extends ScriptOperationFactory[ScriptOperation] {

  override def operations = StackOperationFactory.operations ++ LocktimeOperationFactory.operations ++
    CryptoOperationFactory.operations ++ ControlOperationsFactory.operations ++ BitwiseOperationsFactory.operations ++
    ArithmeticOperationsFactory.operations ++  ScriptNumberFactory.operations ++
    Seq(OP_0,OP_1,OP_1NEGATE, OP_2,OP_3,OP_4,OP_5,OP_6,OP_7,OP_8,
    OP_9,OP_10,OP_11,OP_12,OP_13,OP_14,OP_15,OP_16,OP_FALSE,OP_PUSHDATA1, OP_PUSHDATA2,OP_PUSHDATA4,OP_TRUE)

}
