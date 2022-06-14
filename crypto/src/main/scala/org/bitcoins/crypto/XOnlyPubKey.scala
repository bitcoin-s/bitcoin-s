package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.Try

/** Represents the x-coordinate of an ECPublicKey, with undetermined y-coordinate parity */
case class XOnlyPubKey(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 32,
          s"x-only public keys must be 32 bytes, got $bytes")
  require(Try(publicKey(EvenParity)).isSuccess,
          s"x-only public key must be a valid x coordinate, got $bytes")

  def publicKey(parity: KeyParity): ECPublicKey = {
    val pubKeyBytes = parity.bytes ++ bytes

    ECPublicKey(pubKeyBytes)
  }

  def schnorrPublicKey: SchnorrPublicKey = {
    SchnorrPublicKey(bytes)
  }

  def publicKey: ECPublicKey = schnorrPublicKey.publicKey

  def coord: CurveCoordinate = CurveCoordinate(bytes)

  /** @see https://github.com/bitcoin/bitcoin/blob/3340d46cd363e568ce842b2a9930e30902d150ca/src/pubkey.cpp#L227
    * @see https://github.com/bitcoin-core/secp256k1/blob/9a5a87e0f1276e0284446af1172056ea4693737f/src/modules/extrakeys/main_impl.h#L151
    * @param pubKey the internal tapscript pubkey
    * @param merkleRootOpt the merkle root of the tapscript tree, if empty means we have no scripts in the tapscript tree
    * @param parity the expected parity of the public key reproduced
    * @return
    */
  def checkTapTweak(
      internal: XOnlyPubKey,
      merkleRootOpt: Option[Sha256Digest],
      parity: Boolean): Boolean = {
    // Q = point_add(lift_x(pubkey), point_mul(G, t))
    val tweaked = internal.computeTapTweakHash(merkleRootOpt)
    val fe = FieldElement.fromBytes(tweaked.bytes)
    val multi = CryptoParams.getG.multiply(fe)
    val add = internal.publicKey.add(multi)
    this == add.toXOnly && add.parity.isOdd == parity
  }

  /** @see https://github.com/bitcoin/bitcoin/blob/5174a139c92c1238f9700d06e362dc628d81a0a9/src/pubkey.cpp#L216
    * @param merkleRootOpt if merkle root is empty we have no scripts.
    * The actual tweak does not matter, but follow BIP341 here to
    *
    * @return
    */
  def computeTapTweakHash(merkleRootOpt: Option[Sha256Digest]): Sha256Digest = {
    merkleRootOpt match {
      case Some(merkleRoot) =>
        tapTweakHash(bytes ++ merkleRoot.bytes)
      case None =>
        // We have no scripts. The actual tweak does not matter, but follow BIP341 here to
        // allow for reproducible tweaking.
        tapTweakHash(bytes)
    }
  }

  private def tapTweakHash(bytes: ByteVector): Sha256Digest = {
    CryptoUtil.taggedSha256(bytes, "TapTweak")
  }
}

object XOnlyPubKey extends Factory[XOnlyPubKey] {

  @tailrec
  def fromBytes(bytes: ByteVector): XOnlyPubKey = {
    require(bytes.length <= 33,
            s"XOnlyPublicKey must be less than 33 bytes, got $bytes")

    if (bytes.length == 32)
      new XOnlyPubKey(bytes)
    else if (bytes.length < 32) {
      // means we need to pad the private key with 0 bytes so we have 32 bytes
      XOnlyPubKey.fromBytes(bytes.padLeft(32))
    } else if (bytes.length == 33) {
      // this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
      XOnlyPubKey.fromBytes(bytes.tail)
    } else {
      throw new IllegalArgumentException(
        "XOnlyPublicKey cannot be greater than 33 bytes in size, got: " +
          CryptoBytesUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
    }
  }

  def apply(coord: CurveCoordinate): XOnlyPubKey = {
    XOnlyPubKey(coord.bytes)
  }
}
