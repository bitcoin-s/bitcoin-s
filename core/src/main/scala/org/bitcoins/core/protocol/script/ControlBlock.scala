package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.{Factory, NetworkElement, XOnlyPubKey}
import scodec.bits.ByteVector

/** Control block as defined by BIP341
  *
  * The last stack element is called the control block c, and must have length 33 + 32m,
  * for a value of m that is an integer between 0 and 128[6], inclusive. Fail if it does not have such a length.
  *
  * @see https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#script-validation-rules
  */
case class ControlBlock(bytes: ByteVector) extends NetworkElement {
  //invariants from: https://github.com/bitcoin/bitcoin/blob/37633d2f61697fc719390767aae740ece978b074/src/script/interpreter.cpp#L1835
  require(bytes.size >= TaprootScriptPath.TAPROOT_CONTROL_BASE_SIZE)
  require(bytes.size <= TaprootScriptPath.TAPROOT_CONTROL_MAX_SIZE)
  require(
    (bytes.size - TaprootScriptPath.TAPROOT_CONTROL_BASE_SIZE) % TaprootScriptPath.TAPROOT_CONTROL_NODE_SIZE == 0)

  /** Let p = c[1:33] and let P = lift_x(int(p)) where lift_x and [:] are defined as in BIP340. Fail if this point is not on the curve.
    */
  def p: XOnlyPubKey = {
    XOnlyPubKey.fromBytes(bytes.slice(1, 33))
  }

  val leafVersion: Byte =
    (bytes.head & TaprootScriptPath.TAPROOT_LEAF_MASK).toByte

  val isTapLeafMask: Boolean = {
    (bytes.head & TaprootScriptPath.TAPROOT_LEAF_MASK).toByte == TaprootScriptPath.TAPROOT_LEAF_TAPSCRIPT
  }
}

object ControlBlock extends Factory[ControlBlock] {

  override def fromBytes(bytes: ByteVector): ControlBlock = {
    new ControlBlock(bytes)
  }
}
