package org.bitcoins.crypto

import scodec.bits.ByteVector

// TODO partial signature verification
// TODO implement tweaking
object MuSig2Util {

  private val nonceNum: Int = 2

  type MultiNoncePriv = Vector[ECPrivateKey]
  type MultiNoncePub = Vector[ECPublicKey]

  case class KeyGenContext(
      aggPubKey: ECPublicKey,
      tacc: FieldElement,
      gacc: Boolean) {
    def getPubKey: ByteVector = aggPubKey.bytes.tail
    def getPubKey33: ByteVector = aggPubKey.bytes
  }

  case class KeySet(keys: Vector[SchnorrPublicKey]) {
    keys.init.zip(keys.tail).foreach { case (key1, key2) =>
      require(key1.hex.compareTo(key2.hex) <= 0,
              "Keys must be sorted lexicographically")
    }

    lazy val serialize: ByteVector = {
      keys.map(_.bytes).reduce(_ ++ _)
    }

    def keyAggCoef(key: SchnorrPublicKey): FieldElement = {
      if (key == secondKey) FieldElement.one
      else {
        val listHashBytes = aggListHash(serialize)
        val bytes = aggCoefHash(listHashBytes ++ key.bytes)

        FieldElement(new java.math.BigInteger(1, bytes.toArray))
      }
    }

    lazy val aggPubKey: ECPublicKey = {
      keys
        .map { key =>
          val coef = keyAggCoef(key)
          key.publicKey.multiply(coef)
        }
        .reduce(_.add(_))
    }

    lazy val secondKey: SchnorrPublicKey = keys.tail.head
  }

  object KeySet {

    def apply(keys: Vector[SchnorrPublicKey]): KeySet = {
      val sortedKeys = keys.sorted(NetworkElement.lexicographicalOrdering)
      new KeySet(sortedKeys)
    }

    def apply(keys: SchnorrPublicKey*): KeySet = {
      KeySet(keys.toVector)
    }
  }

  def aggListHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig2/agg_list").bytes
  }

  def aggCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig2/agg_coef").bytes
  }

  def nonHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig2/non").bytes
  }

  def nonCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig2/non_coef").bytes
  }

  def sigHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig2/sig").bytes
  }

  def auxHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig2/aux").bytes
  }

  def genMultiNonce(
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[SchnorrPublicKey] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): (
      MultiNoncePub,
      MultiNoncePriv) = {
    msgOpt.foreach(msg => require(msg.length == 32))
    require(extraInOpt.forall(_.length <= 4294967295L))

    val preRand = CryptoUtil.randomBytes(32)
    val rand = privKeyOpt match {
      case Some(privKey) => auxHash(preRand).xor(privKey.bytes)
      case None          => preRand
    }

    val aggPubKeyBytes = aggPubKeyOpt match {
      case Some(aggPubKey) => aggPubKey.bytes.+:(aggPubKey.bytes.length.toByte)
      case None            => ByteVector.fromByte(0)
    }

    val msgBytes = msgOpt match {
      case Some(msg) => msg.+:(msg.length.toByte)
      case None      => ByteVector.fromByte(0)
    }

    val extraInBytes = extraInOpt match {
      case Some(extraIn) =>
        ByteVector.fromLong(extraIn.length).padLeft(4) ++ extraIn
      case None => ByteVector.fromByte(0)
    }

    val privNonces = 0.until(nonceNum).toVector.map { index =>
      val indexByte = ByteVector.fromByte(index.toByte)

      val noncePreBytes =
        nonHash(rand ++ aggPubKeyBytes ++ msgBytes ++ extraInBytes ++ indexByte)

      val noncePreNum = new java.math.BigInteger(1, noncePreBytes.toArray)

      FieldElement(noncePreNum).toPrivateKey
    }
    val pubNonces = privNonces.map(_.publicKey)

    (pubNonces, privNonces)
  }

  def aggNonces(nonces: Vector[MultiNoncePub]): MultiNoncePub = {
    0.until(nonceNum).toVector.map { i =>
      nonces.map(multiNonce => multiNonce(i)).reduce(_.add(_))
    }
  }

  def getSessionValues(
      aggMultiNoncePub: MultiNoncePub,
      keySet: KeySet,
      message: ByteVector): (FieldElement, ECPublicKey, FieldElement) = {
    require(aggMultiNoncePub.length == nonceNum)

    val aggPubKey = keySet.aggPubKey.schnorrPublicKey

    val bHash = nonCoefHash(
      aggMultiNoncePub
        .map(_.bytes)
        .reduce(_ ++ _) ++ aggPubKey.bytes ++ message)
    val b = FieldElement(new java.math.BigInteger(1, bHash.toArray))

    val aggNonce = aggMultiNoncePub.tail
      .foldLeft((FieldElement.one, aggMultiNoncePub.head)) {
        case ((prevPow, sumSoFar), nonce) =>
          val pow = prevPow.multiply(b)
          val point = nonce.multiply(pow)

          (pow, sumSoFar.add(point)) // TODO Deal with infinity here
      }
      ._2
    val eBytes = CryptoUtil
      .sha256SchnorrChallenge(
        aggNonce.schnorrNonce.bytes ++ aggPubKey.bytes ++ message)
      .bytes
    val e = FieldElement(new java.math.BigInteger(1, eBytes.toArray))

    (b, aggNonce, e)
  }

  def sign(
      noncePriv: MultiNoncePriv,
      aggMultiNoncePub: MultiNoncePub,
      privKey: ECPrivateKey,
      message: ByteVector,
      keySet: KeySet): (ECPublicKey, FieldElement) = {
    val pubKey = privKey.publicKey
    val coef = keySet.keyAggCoef(pubKey.schnorrPublicKey)
    val (b, aggNonce, e) = getSessionValues(aggMultiNoncePub, keySet, message)

    val adjustedNoncePriv = aggNonce.parity match {
      case EvenParity => noncePriv
      case OddParity  => noncePriv.map(_.negate)
    }

    val gp = pubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val g = keySet.aggPubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val adjustedPrivKey = privKey.fieldElement.multiply(gp).multiply(g)

    val privNonceSum = adjustedNoncePriv
      .foldLeft((FieldElement.one, FieldElement.zero)) {
        case ((pow, sumSoFar), privNonce) =>
          val scalar = privNonce.fieldElement.multiply(pow)
          val nextPow = pow.multiply(b)

          (nextPow, sumSoFar.add(scalar))
      }
      ._2

    val s = adjustedPrivKey.multiply(e).multiply(coef).add(privNonceSum)

    (aggNonce, s) // TODO require passes internal verification
  }

  def signAgg(
      sVals: Vector[FieldElement],
      aggPubNonce: ECPublicKey): SchnorrDigitalSignature = {
    val s = sVals.reduce(_.add(_))

    SchnorrDigitalSignature(aggPubNonce.schnorrNonce, s)
  }
}
