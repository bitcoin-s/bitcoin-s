package org.scalacoin.script

import org.scalacoin.script.arithmetic.ArithmeticOperationsFactory
import org.scalacoin.script.bitwise.BitwiseOperationsFactory
import org.scalacoin.script.control.ControlOperationsFactory
import org.scalacoin.script.crypto.CryptoOperationFactory
import org.scalacoin.script.locktime.LocktimeOperationFactory
import org.scalacoin.script.stack.StackOperationFactory

/**
 * Created by chris on 1/8/16.
 */
trait ScriptOperationFactory[T] {

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

}


object ScriptOperationFactory extends ScriptOperationFactory[ScriptOperation] {

  override def operations = StackOperationFactory.operations ++ LocktimeOperationFactory.operations ++
    CryptoOperationFactory.operations ++ ControlOperationsFactory.operations ++ BitwiseOperationsFactory.operations ++
    ArithmeticOperationsFactory.operations

}
