package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.*
import scodec.bits.ByteVector

/** Control block as defined by BIP341
  *
  * The last stack element is called the control block c, and must have length
  * 33 + 32m, for a value of m that is an integer between 0 and 128[6],
  * inclusive. Fail if it does not have such a length.
  *
  * @see
  *   https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#script-validation-rules
  */
sealed abstract class ControlBlock extends NetworkElement {
  require(ControlBlock.isValid(bytes), s"Bytes for control block are not valid")

  /** Let p = c[1:33] and let P = lift_x(int(p)) where lift_x and [:] are
    * defined as in BIP340. Fail if this point is not on the curve.
    */
  def p: XOnlyPubKey = {
    XOnlyPubKey.fromBytes(bytes.slice(1, 33))
  }

  val leafVersion: LeafVersion =
    LeafVersion.fromMaskedByte(bytes.head)

  val isTapLeafMask: Boolean = {
    leafVersion == LeafVersion.Tapscript
  }

  /** Leaf or branch hashes embedded in the control block */
  def hashes: Vector[Sha256Digest] = {
    bytes.drop(33).grouped(32).map(Sha256Digest.fromBytes).toVector
  }
}

case class TapscriptControlBlock(bytes: ByteVector) extends ControlBlock {
  require(TapscriptControlBlock.isValid(bytes),
          s"Invalid tapscript control block, got=$bytes")

  def parity: KeyParity = {
    if ((bytes.head & 1) == 1) OddParity
    else EvenParity
  }
}

/** A control block that does not have a leaf version defined as per BIP342 This
  * is needed for future soft fork compatability where we introduce new leaf
  * versions to correspond to new spending rules
  */
case class UnknownControlBlock(bytes: ByteVector) extends ControlBlock

object ControlBlock extends Factory[ControlBlock] {

  override def fromBytes(bytes: ByteVector): ControlBlock = {
    new TapscriptControlBlock(bytes)
  }

  /** invariants from:
    * https://github.com/bitcoin/bitcoin/blob/37633d2f61697fc719390767aae740ece978b074/src/script/interpreter.cpp#L1835
    */
  def isValid(bytes: ByteVector): Boolean = {
    bytes.size >= TaprootScriptPath.TAPROOT_CONTROL_BASE_SIZE &&
    bytes.size <= TaprootScriptPath.TAPROOT_CONTROL_MAX_SIZE &&
    (bytes.size - TaprootScriptPath.TAPROOT_CONTROL_BASE_SIZE) % TaprootScriptPath.TAPROOT_CONTROL_NODE_SIZE == 0
  }
}

object TapscriptControlBlock extends Factory[TapscriptControlBlock] {

  /** invariants from:
    * https://github.com/bitcoin/bitcoin/blob/37633d2f61697fc719390767aae740ece978b074/src/script/interpreter.cpp#L1835
    */
  def isValid(bytes: ByteVector): Boolean = {
    if (bytes.isEmpty) {
      false
    } else {
      LeafVersion.knownLeafVersions.contains(
        LeafVersion.fromMaskedByte(bytes.head)) &&
      ControlBlock.isValid(bytes) &&
      XOnlyPubKey.fromBytesT(bytes.slice(1, 33)).isSuccess
    }
  }

  /** Creates a control block with no scripts, just an internal key */
  def fromXOnlyPubKey(internalKey: XOnlyPubKey): TapscriptControlBlock = {
    fromBytes(LeafVersion.Tapscript.toByte +: internalKey.bytes)
  }

  override def fromBytes(bytes: ByteVector): TapscriptControlBlock = {
    new TapscriptControlBlock(bytes)
  }

  def apply(
      leafVersion: LeafVersion,
      internalKey: XOnlyPubKey,
      parity: KeyParity,
      leafHashes: Vector[Sha256Digest]): TapscriptControlBlock = {
    val parityByte: Byte = parity match {
      case OddParity  => 0x01
      case EvenParity => 0x0
    }
    val bytes =
      ((parityByte | leafVersion.toByte).toByte +: internalKey.bytes) ++ ByteVector
        .concat(leafHashes.map(_.bytes))
    TapscriptControlBlock(bytes)
  }

  /** Constructs a tapscript control block for the case where we have one single
    * [[TapLeaf]] in the entire tree In this case, we don't have any elements in
    * the control block besides the internal key, leaf version and parity of the
    * output xonly pubkey
    */
  def fromSingleLeaf(
      leafVersion: LeafVersion,
      internalKey: XOnlyPubKey,
      parity: KeyParity): TapscriptControlBlock = {
    TapscriptControlBlock(leafVersion, internalKey, parity, Vector.empty)
  }
}

object UnknownControlBlock extends Factory[UnknownControlBlock] {

  override def fromBytes(bytes: ByteVector): UnknownControlBlock =
    new UnknownControlBlock(bytes)
}
