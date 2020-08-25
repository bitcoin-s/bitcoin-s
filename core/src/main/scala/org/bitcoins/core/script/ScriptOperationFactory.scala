package org.bitcoins.core.script

import org.bitcoins.core.script.arithmetic.ArithmeticOperation
import org.bitcoins.core.script.bitwise.BitwiseOperation
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.ControlOperations
import org.bitcoins.core.script.crypto.CryptoOperation
import org.bitcoins.core.script.locktime.LocktimeOperation
import org.bitcoins.core.script.reserved.ReservedOperation
import org.bitcoins.core.script.splice.SpliceOperation
import org.bitcoins.core.script.stack.StackOperation
import org.bitcoins.core.util.{BitcoinSLogger, BytesUtil}
import org.bitcoins.crypto.StringFactory
import scodec.bits.ByteVector

/**
  * Created by chris on 1/8/16.
  * Responsible for matching script op codes with their given
  * hexadecimal representation or byte representation
  */
trait ScriptOperationFactory[T <: ScriptOperation]
    extends StringFactory[T]
    with BitcoinSLogger {

  /** All of the [[org.bitcoins.core.script.ScriptOperation ScriptOperation]]s for a particular `T`. */
  def operations: Vector[T]

  /**
    * Finds a [[org.bitcoins.core.script.ScriptOperation ScriptOperation]] from a given string
    */
  override def fromStringOpt(str: String): Option[T] = {
    val result: Option[T] = operations.find(_.toString == str)
    if (result.isEmpty) {
      //try and remove the 'OP_' prefix on the operations and see if it matches anything.
      operations.find(op =>
        removeOP_Prefix(op.toString) == removeOP_Prefix(str))
    } else result
  }

  override def fromString(str: String): T = {
    fromStringOpt(str) match {
      case Some(op) => op
      case None =>
        sys.error(s"Could not find script operation=${str}")
    }
  }

  /**
    * Finds a [[org.bitcoins.core.script.ScriptOperation ScriptOperation]] from its hexadecimal representation.
    */
  def fromHex(hex: String): Option[T] = {
    val bytes = BytesUtil.decodeHex(hex)
    fromBytes(bytes)
  }

  /**
    * Removes the 'OP_' prefix from a given operation.
    * Example: `OP_EQUALVERIFY` would be transformed into `EQUALVERIFY`
    */
  private def removeOP_Prefix(str: String): String = {
    str.replace("OP_", "")
  }

  /** Finds a [[org.bitcoins.core.script.ScriptOperation ScriptOperation]] from a given [[scala.Byte Byte]]. */
  @inline final def fromByte(byte: Byte): T = {
    scriptOpMap(byte).asInstanceOf[T]
  }

  def fromBytes(bytes: ByteVector): Option[T] = {
    if (bytes.length == 1) {
      val op = fromByte(bytes.head)
      Some(op)
    } else {
      None
    }
  }

  private lazy val scriptOpMap: Map[Byte, ScriptOperation] = {
    operations.map(o => (o.toByte, o)).toMap
  }
  def apply(byte: Byte): T = fromByte(byte)

  def apply(hex: String): Option[T] = fromHex(hex)
}

object ScriptOperation extends ScriptOperationFactory[ScriptOperation] {

  /** This contains duplicate operations
    * There is an optimization here by moving popular opcodes
    * to the front of the vector so when we iterate through it,
    * we are more likely to find the op code we are looking for
    * sooner
    */
  final override val operations: Vector[ScriptOperation] = {
    StackPushOperationFactory.pushDataOperations ++
      StackOperation.operations ++
      LocktimeOperation.operations ++
      CryptoOperation.operations ++
      ControlOperations.operations ++
      BitwiseOperation.operations ++
      ArithmeticOperation.operations ++
      BytesToPushOntoStack.operations ++
      SpliceOperation.operations ++
      ReservedOperation.operations ++
      ScriptNumberOperation.operations
  }

}
