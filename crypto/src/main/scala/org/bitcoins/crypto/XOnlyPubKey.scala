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

  def coord: CurveCoordinate = CurveCoordinate(bytes)
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
