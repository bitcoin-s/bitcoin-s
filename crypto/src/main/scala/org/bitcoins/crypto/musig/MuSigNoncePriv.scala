package org.bitcoins.crypto.musig

import org.bitcoins.crypto.*
import scodec.bits.ByteVector

/** Wraps the ephemeral private keys making up a MuSig2 nonce */
case class MuSigNoncePriv(bytes: ByteVector)
    extends NetworkElement
    with MaskedToString {
  require(bytes.length == 97,
          s"MuSigNoncePriv must be 97 bytes, got: ${bytes.length}")
  override def toStringSensitive: String = s"MuSigNoncePriv(${bytes.length})"

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

  /** Collapses this into a single ephemeral private key */
  def sumToKey(b: FieldElement): FieldElement = {
    val fes = Vector(k1, k2).map(_.fieldElement)
    MuSigUtil.nonceSum[FieldElement](fes,
                                     b,
                                     _.add(_),
                                     _.multiply(_),
                                     FieldElement.zero)
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

  /** Generates a MuSigNoncePriv given 32 bytes of entropy from preRand, and
    * possibly some other sources, as specified in the BIP.
    */
  def genInternal(
      preRand: ByteVector,
      publicKey: ECPublicKey,
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[SchnorrPublicKey] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): MuSigNoncePriv = {
    require(preRand.length == 32,
            s"32 bytes of entropy must be provided, found $preRand")
    require(msgOpt.forall(msg => msg.length == 32),
            s"The message to be signed must be 32 bytes, found $msgOpt")
    require(
      extraInOpt.forall(_.length <= 4294967295L),
      "extraIn too long, its length must be represented by at most four bytes")

    def serializeWithLen(
        bytesOpt: Option[ByteVector],
        lengthSize: Int = 1): ByteVector = {
      bytesOpt match {
        case Some(bytes) =>
          ByteVector.fromLong(bytes.length, lengthSize) ++ bytes
        case None => ByteVector.fromLong(0, lengthSize)
      }
    }

    val rand = privKeyOpt match {
      case Some(privKey) => MuSigUtil.auxHash(preRand).xor(privKey.bytes)
      case None          => preRand
    }

    val aggPubKeyBytes = serializeWithLen(aggPubKeyOpt.map(_.bytes))
    val msgBytes = serializeWithLen(msgOpt)
    val extraInBytes = serializeWithLen(extraInOpt, lengthSize = 4)
    val dataBytes = rand ++ aggPubKeyBytes ++ msgBytes ++ extraInBytes

    val privNonceKeys = 0.until(MuSigUtil.nonceNum).toVector.map { index =>
      val indexByte = ByteVector.fromByte(index.toByte)
      val noncePreBytes = MuSigUtil.nonHash(dataBytes ++ indexByte)
      val noncePreNum = new java.math.BigInteger(1, noncePreBytes.toArray)

      FieldElement(noncePreNum).toPrivateKey
    }

    MuSigNoncePriv(privNonceKeys(0), privNonceKeys(1), publicKey)
  }

  /** Generates 32 bytes of entropy and constructs a MuSigNoncePriv from this,
    * and possibly some other sources, as specified in the BIP.
    */
  def gen(
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[SchnorrPublicKey] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): MuSigNoncePriv = {
    val preRand = CryptoUtil.randomBytes(32)

    genInternal(preRand, privKeyOpt, aggPubKeyOpt, msgOpt, extraInOpt)
  }
}
