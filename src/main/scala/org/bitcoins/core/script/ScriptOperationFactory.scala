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
import org.bitcoins.core.util.{BitcoinSUtil, BitcoinSLogger}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/8/16.
 * Responsible for matching script op codes with their given
 * hexadecimal representation or byte representation
 */
trait ScriptOperationFactory[T <: ScriptOperation] extends BitcoinSLogger {

  /**
   * All of the script operations for a particular T
    *
    * @tparam T
   * @return
   */
  def operations : Seq[T]

  /**
   * Finds a script operation from a given string
    *
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
    *
    * @param hex
   * @return
   */
  def fromHex(hex : String) : Option[T] = operations.find(_.hex == hex.toLowerCase)

  /**
   * Removes the 'OP_' prefix from a given operation.
   * Example: OP_EQUALVERIFY would be transformed into EQUALVERIFY
    *
    * @param str
   * @return
   */
  private def removeOP_Prefix(str : String) : String = {
    str.replace("OP_","")
  }

  /**
   * Finds a script operation from a given byte
    *
    * @param byte
   * @return
   */
  def fromByte(byte : Byte) : Option[T] = {
    val hex = BitcoinSUtil.encodeHex(byte)
    fromHex(hex)
  }

  def apply(byte : Byte) : Option[T] = fromByte(byte)

  def apply(hex : String) : Option[T] = fromHex(hex)
}


object ScriptOperation extends ScriptOperationFactory[ScriptOperation] {

  lazy val operations = ScriptNumberOperation.operations ++ Seq(OP_FALSE,OP_PUSHDATA1, OP_PUSHDATA2,OP_PUSHDATA4,OP_TRUE) ++ StackOperation.operations ++ LocktimeOperation.operations ++
    CryptoOperation.operations ++ ControlOperations.operations ++ BitwiseOperation.operations ++
    ArithmeticOperation.operations ++  BytesToPushOntoStack.operations ++ SpliceOperation.operations ++
    ReservedOperation.operations

}
