package org.bitcoins.crypto.musig

import org.bitcoins.crypto.*
import scodec.bits.ByteVector

/** Wraps the ephemeral private keys making up a MuSig2 nonce */
case class MuSigNoncePriv(override val bytes: ByteVector)
    extends NetworkElement
    with MaskedToString {
  require(bytes.length == 97,
          s"MuSigNoncePriv must be 97 bytes, got: ${bytes.length}")
  override def toStringSensitive: String = s"MuSigNoncePriv(${bytes})"

  val k1: ECPrivateKey = ECPrivateKey.fromBytes(bytes.slice(0, 32))

  val k2: ECPrivateKey = ECPrivateKey.fromBytes(bytes.slice(32, 64))

  /** The signer's (compressed) public key is stored alongside the ephemeral
    * private nonces. This is needed when verifying partial signatures and when
    * working with tweaked individual keys (see BIP-327: "Signing with tweaked
    * individual keys"). In MuSig/MuSig2 flows the verifier often needs the
    * original public key (or the aggregate public key) to compute the
    * appropriate parity/tweak and to reconstruct the adjusted public key used
    * in verification. Storing the public key here keeps the nonce packet
    * self-contained for those checks.
    *
    * Reference:
    * https://github.com/bitcoin/bips/blob/master/bip-0327.mediawiki#signing-with-tweaked-individual-keys
    */
  val publicKey: ECPublicKey = ECPublicKey.fromBytes(bytes.takeRight(33))

  def toNoncePub: MuSigNoncePub = {
    MuSigNoncePub(k1.publicKey, k2.publicKey)
  }

  def negate: MuSigNoncePriv = {
    MuSigNoncePriv(k1.negate, k2.negate, publicKey)
  }

  /** Collapses this into a single ephemeral private key */
  def sumToKey(b: FieldElement): FieldElement = {
    val fes = Vector(k1, k2).map(_.fieldElement)
    MuSigUtil.nonceSum[FieldElement](nonces = fes,
                                     b = b,
                                     add = _.add(_),
                                     multiply = _.multiply(_),
                                     identity = FieldElement.zero)
  }
}

object MuSigNoncePriv extends Factory[MuSigNoncePriv] {

  override def fromBytes(bytes: ByteVector): MuSigNoncePriv = {
    new MuSigNoncePriv(bytes)
  }

  def apply(
      k1: ECPrivateKey,
      k2: ECPrivateKey,
      pubKey: ECPublicKey): MuSigNoncePriv = {
    fromBytes(k1.bytes ++ k2.bytes ++ pubKey.bytes)
  }
}
