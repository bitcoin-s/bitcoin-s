package org.bitcoins.crypto

import scodec.bits.ByteVector

// TODO test against secp256k1-zkp
// TODO refactor for niceness
// TODO easy optimizations (e.g. remove parity multiplications)
object MuSig2Util {

  private val nonceNum: Int = 2

  case class MultiNoncePriv(privNonces: Vector[ECPrivateKey])
      extends NetworkElement {
    require(privNonces.length == nonceNum)

    def toPublicNonces: MultiNoncePub = {
      MultiNoncePub(privNonces.map(_.publicKey.toPoint))
    }

    def toFieldElements: Vector[FieldElement] = {
      privNonces.map(_.fieldElement)
    }

    def length: Int = privNonces.length

    override def bytes: ByteVector = {
      privNonces.map(_.bytes).reduce(_ ++ _)
    }

    def negate: MultiNoncePriv = {
      MultiNoncePriv(privNonces.map(_.negate))
    }
  }

  object MultiNoncePriv extends Factory[MultiNoncePriv] {

    override def fromBytes(bytes: ByteVector): MultiNoncePriv = {
      val privs =
        CryptoBytesUtil.splitEvery(bytes, 32).map(ECPrivateKey.fromBytes)
      MultiNoncePriv(privs)
    }
  }

  case class MultiNoncePub(pubNonces: Vector[SecpPoint])
      extends NetworkElement {
    require(pubNonces.length == nonceNum)

    def apply(i: Int): SecpPoint = {
      pubNonces(i)
    }

    def length: Int = pubNonces.length

    override def bytes: ByteVector = {
      pubNonces
        .map {
          case SecpPointInfinity  => ByteVector.fill(33)(0)
          case p: SecpPointFinite => p.toPublicKey.bytes
        }
        .reduce(_ ++ _)
    }
  }

  object MultiNoncePub extends Factory[MultiNoncePub] {

    val infPtBytes: ByteVector = ByteVector.fill(33)(0)

    override def fromBytes(bytes: ByteVector): MultiNoncePub = {
      val pubs =
        CryptoBytesUtil.splitEvery(bytes, 33).map { b =>
          if (b == infPtBytes) SecpPointInfinity
          else ECPublicKey.fromBytes(b).toPoint
        }

      MultiNoncePub(pubs)
    }
  }

  case class Tweak(tweak: FieldElement, isXOnlyT: Boolean) {
    def point: ECPublicKey = tweak.getPublicKey
  }

  case class TweakContext(parityAcc: FieldElement, tweakAcc: FieldElement) {

    def applyTweak(
        tweak: Tweak,
        aggPubKey: ECPublicKey): (ECPublicKey, TweakContext) = {
      val parityMult =
        if (tweak.isXOnlyT && aggPubKey.parity == OddParity)
          FieldElement.orderMinusOne
        else FieldElement.one

      val newAggPubKey = aggPubKey.multiply(parityMult).add(tweak.point)
      val newParityAcc = parityAcc.multiply(parityMult)
      val newTweakAcc = tweakAcc.multiply(parityMult).add(tweak.tweak)
      (newAggPubKey, TweakContext(newParityAcc, newTweakAcc))
    }
  }

  object TweakContext {
    val empty: TweakContext = TweakContext(FieldElement.one, FieldElement.zero)
  }

  sealed trait KeySet {
    def keys: Vector[SchnorrPublicKey]

    def tweaks: Vector[Tweak]

    def withTweaks(newTweaks: Vector[Tweak]): KeySet = {
      require(tweaks.isEmpty, "withTweaks is not meant for replacing tweaks")

      this match {
        case ks: LexicographicKeySet => ks.copy(tweaks = newTweaks)
        case ks: UnsortedKeySet      => ks.copy(tweaks = newTweaks)
      }
    }

    def length: Int = keys.length

    def apply(i: Int): SchnorrPublicKey = {
      keys(i)
    }

    lazy val serialize: ByteVector = {
      keys.map(_.bytes).reduce(_ ++ _)
    }

    def keyAggCoef(key: SchnorrPublicKey): FieldElement = {
      if (secondKeyOpt.contains(key)) FieldElement.one
      else {
        val listHashBytes = aggListHash(serialize)
        val bytes = aggCoefHash(listHashBytes ++ key.bytes)

        FieldElement(new java.math.BigInteger(1, bytes.toArray))
      }
    }

    private lazy val computeAggPubKeyAndTweakContext: (
        ECPublicKey,
        TweakContext) = {
      val untweakedAggPubKey = keys
        .map { key =>
          val coef = keyAggCoef(key)
          key.publicKey.multiply(coef)
        }
        .reduce(_.add(_))

      tweaks.foldLeft((untweakedAggPubKey, TweakContext.empty)) {
        case ((pubKeySoFar, context), tweak) =>
          context.applyTweak(tweak, pubKeySoFar)
      }
    }

    lazy val aggPubKey: ECPublicKey = computeAggPubKeyAndTweakContext._1

    lazy val tweakContext: TweakContext = computeAggPubKeyAndTweakContext._2

    // In truth this represents the first key different from the head key
    lazy val secondKeyOpt: Option[SchnorrPublicKey] = {
      keys.find(_ != keys.head)
    }
  }

  case class LexicographicKeySet(
      override val keys: Vector[SchnorrPublicKey],
      override val tweaks: Vector[Tweak] = Vector.empty)
      extends KeySet {
    keys.init.zip(keys.tail).foreach { case (key1, key2) =>
      require(key1.hex.compareTo(key2.hex) <= 0,
              "Keys must be sorted lexicographically")
    }
  }

  object KeySet {

    def apply(keys: Vector[SchnorrPublicKey]): LexicographicKeySet = {
      val sortedKeys = keys.sorted(NetworkElement.lexicographicalOrdering)
      LexicographicKeySet(sortedKeys)
    }

    def apply(keys: SchnorrPublicKey*): LexicographicKeySet = {
      KeySet(keys.toVector)
    }

    def apply(
        keys: Vector[SchnorrPublicKey],
        tweaks: Vector[Tweak]): LexicographicKeySet = {
      val sortedKeys = keys.sorted(NetworkElement.lexicographicalOrdering)
      LexicographicKeySet(sortedKeys, tweaks)
    }
  }

  case class UnsortedKeySet(
      override val keys: Vector[SchnorrPublicKey],
      override val tweaks: Vector[Tweak] = Vector.empty)
      extends KeySet

  def aggListHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "KeyAgg list").bytes
  }

  def aggCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "KeyAgg coefficient").bytes
  }

  def nonHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/nonce").bytes
  }

  def nonCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/noncecoef").bytes
  }

  def auxHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/aux").bytes
  }

  // TODO change aggPubKey back to SchnorrPublicKey and remove requirement once test vector is changed to valid x-coordinate
  def genMultiNonceInternal(
      preRand: ByteVector,
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[ByteVector] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): (
      MultiNoncePub,
      MultiNoncePriv) = {
    require(preRand.length == 32)
    require(msgOpt.forall(msg => msg.length == 32))
    require(aggPubKeyOpt.forall(aggPubKey => aggPubKey.length == 32))
    require(extraInOpt.forall(_.length <= 4294967295L))

    val rand = privKeyOpt match {
      case Some(privKey) => auxHash(preRand).xor(privKey.bytes)
      case None          => preRand
    }

    val aggPubKeyBytes = aggPubKeyOpt match {
      case Some(aggPubKey) => aggPubKey.+:(aggPubKey.length.toByte)
      case None            => ByteVector.fromByte(0)
    }

    val msgBytes = msgOpt match {
      case Some(msg) => msg.+:(msg.length.toByte)
      case None      => ByteVector.fromByte(0)
    }

    val extraInBytes = extraInOpt match {
      case Some(extraIn) =>
        ByteVector.fromLong(extraIn.length, size = 4) ++ extraIn
      case None => ByteVector.fromLong(0, size = 4)
    }

    val privNonceKeys = 0.until(nonceNum).toVector.map { index =>
      val indexByte = ByteVector.fromByte(index.toByte)

      val noncePreBytes =
        nonHash(rand ++ aggPubKeyBytes ++ msgBytes ++ extraInBytes ++ indexByte)

      val noncePreNum = new java.math.BigInteger(1, noncePreBytes.toArray)

      FieldElement(noncePreNum).toPrivateKey
    }
    val privNonces = MultiNoncePriv(privNonceKeys)
    val pubNonces = privNonces.toPublicNonces

    (pubNonces, privNonces)
  }

  def genMultiNonce(
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[SchnorrPublicKey] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): (
      MultiNoncePub,
      MultiNoncePriv) = {
    val preRand = CryptoUtil.randomBytes(32)

    genMultiNonceInternal(preRand,
                          privKeyOpt,
                          aggPubKeyOpt.map(_.bytes),
                          msgOpt,
                          extraInOpt)
  }

  def aggNonces(nonces: Vector[MultiNoncePub]): MultiNoncePub = {
    val aggNonceKeys = 0.until(nonceNum).toVector.map { i =>
      nonces.map(multiNonce => multiNonce(i)).reduce(_.add(_))
    }

    MultiNoncePub(aggNonceKeys)
  }

  private def nonceSum[T](
      nonces: Vector[T],
      b: FieldElement,
      add: (T, T) => T,
      multiply: (T, FieldElement) => T,
      identity: T): T = {
    nonces
      .foldLeft((FieldElement.one, identity)) { case ((pow, sumSoFar), nonce) =>
        val prod = multiply(nonce, pow)

        (pow.multiply(b), add(sumSoFar, prod))
      }
      ._2
  }

  def multiNoncePubSum(
      multiNoncePub: MultiNoncePub,
      b: FieldElement): ECPublicKey = {
    nonceSum[SecpPoint](multiNoncePub.pubNonces,
                        b,
                        _.add(_),
                        _.multiply(_),
                        SecpPointInfinity) match {
      case SecpPointInfinity  => CryptoParams.getG
      case p: SecpPointFinite => p.toPublicKey
    }
  }

  def multiNoncePrivSum(
      multiNoncePriv: MultiNoncePriv,
      b: FieldElement): FieldElement = {
    nonceSum[FieldElement](multiNoncePriv.toFieldElements,
                           b,
                           _.add(_),
                           _.multiply(_),
                           FieldElement.zero)
  }

  def getSessionValues(
      aggMultiNoncePub: MultiNoncePub,
      keySet: KeySet,
      message: ByteVector): (FieldElement, ECPublicKey, FieldElement) = {
    require(aggMultiNoncePub.length == nonceNum)

    val aggPubKey = keySet.aggPubKey.schnorrPublicKey

    val bHash = nonCoefHash(
      aggMultiNoncePub.bytes ++ aggPubKey.bytes ++ message)
    val b = FieldElement(new java.math.BigInteger(1, bHash.toArray))

    val aggNonce = multiNoncePubSum(aggMultiNoncePub, b)
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
      case OddParity  => noncePriv.negate
    }

    val gp = pubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val g = keySet.aggPubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val adjustedPrivKey = privKey.fieldElement
      .multiply(gp)
      .multiply(g)
      .multiply(keySet.tweakContext.parityAcc)

    val privNonceSum = multiNoncePrivSum(adjustedNoncePriv, b)

    val s = adjustedPrivKey.multiply(e).multiply(coef).add(privNonceSum)

    require(
      partialSigVerify(s,
                       noncePriv.toPublicNonces,
                       pubKey.schnorrPublicKey,
                       keySet,
                       b,
                       aggNonce,
                       e))

    (aggNonce, s)
  }

  def partialSigVerify(
      partialSig: FieldElement,
      pubNonces: Vector[MultiNoncePub],
      keySet: KeySet,
      message: ByteVector,
      signerIndex: Int): Boolean = {
    require(signerIndex >= 0 && signerIndex < keySet.length)

    partialSigVerify(partialSig,
                     pubNonces(signerIndex),
                     aggNonces(pubNonces),
                     keySet(signerIndex),
                     keySet,
                     message)
  }

  def partialSigVerify(
      partialSig: FieldElement,
      multiNoncePub: MultiNoncePub,
      aggMultiNoncePub: MultiNoncePub,
      pubKey: SchnorrPublicKey,
      keySet: KeySet,
      message: ByteVector): Boolean = {
    val (b, aggNonce, e) = getSessionValues(aggMultiNoncePub, keySet, message)

    partialSigVerify(partialSig, multiNoncePub, pubKey, keySet, b, aggNonce, e)
  }

  def partialSigVerify(
      partialSig: FieldElement,
      multiNoncePub: MultiNoncePub,
      pubKey: SchnorrPublicKey,
      keySet: KeySet,
      b: FieldElement,
      aggNonce: ECPublicKey,
      e: FieldElement): Boolean = {
    val nonceSum = multiNoncePubSum(multiNoncePub, b)
    val nonceSumAdjusted = aggNonce.parity match {
      case EvenParity => nonceSum
      case OddParity  => nonceSum.multiply(FieldElement.orderMinusOne)
    }

    val g = keySet.aggPubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val aggKeyParity = g.multiply(keySet.tweakContext.parityAcc) match {
      case FieldElement.one           => EvenParity
      case FieldElement.orderMinusOne => OddParity
      case _: FieldElement => // TODO TweakContext.parityAcc needs an ADT
        throw new RuntimeException("Something has gone very wrong.")
    }

    val aggKey = pubKey.toXOnly.publicKey(aggKeyParity)
    val a = keySet.keyAggCoef(pubKey)
    partialSig.getPublicKey == nonceSumAdjusted.add(
      aggKey.multiply(e.multiply(a)))
  }

  def signAgg(
      sVals: Vector[FieldElement],
      aggMultiNoncePub: MultiNoncePub,
      keySet: KeySet,
      message: ByteVector): SchnorrDigitalSignature = {
    val (_, aggPubNonce, e) =
      getSessionValues(aggMultiNoncePub, keySet, message)
    val tweakData = TweakData(keySet.tweakContext, keySet.aggPubKey.parity, e)

    signAgg(sVals, aggPubNonce, Some(tweakData))
  }

  case class TweakData(
      context: TweakContext,
      aggPubKeyParity: KeyParity,
      e: FieldElement) {

    def additiveTweak: FieldElement = {
      val g = aggPubKeyParity match {
        case EvenParity => FieldElement.one
        case OddParity  => FieldElement.orderMinusOne
      }

      e.multiply(g).multiply(context.tweakAcc)
    }
  }

  def signAgg(
      sVals: Vector[FieldElement],
      aggPubNonce: ECPublicKey,
      tweakDataOpt: Option[TweakData] = None): SchnorrDigitalSignature = {
    val sSum = sVals.reduce(_.add(_))
    val s = tweakDataOpt match {
      case Some(tweakData) => sSum.add(tweakData.additiveTweak)
      case None            => sSum
    }

    SchnorrDigitalSignature(aggPubNonce.schnorrNonce, s)
  }
}
