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
  def fromString(str : String) : Option[T] = operations.find(_.toString == str)

  def fromOpCode(byte : Byte) : Option[T] = {
    val hex = encodeHex(byte)
    operations.find(op => op.hex == hex)
  }
}


object ScriptOperationFactory extends ScriptOperationFactory[ScriptOperation] {

  override def operations = StackOperationFactory.operations ++ LocktimeOperationFactory.operations ++
    CryptoOperationFactory.operations ++ ControlOperationsFactory.operations ++ BitwiseOperationsFactory.operations ++
    ArithmeticOperationsFactory.operations ++  ScriptNumberFactory.operations ++
    Seq(OP_0,OP_1,OP_1NEGATE, OP_2,OP_3,OP_4,OP_5,OP_6,OP_7,OP_8,
    OP_9,OP_10,OP_11,OP_12,OP_13,OP_14,OP_15,OP_16,OP_FALSE,OP_PUSHDATA1, OP_PUSHDATA2,OP_PUSHDATA4,OP_TRUE)

}
