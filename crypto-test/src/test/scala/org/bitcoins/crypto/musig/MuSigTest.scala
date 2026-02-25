package org.bitcoins.crypto.musig

import org.bitcoins.crypto.*
import org.bitcoins.crypto.musig.MuSigUtil.*
import org.scalacheck.Gen

class MuSigTest extends BitcoinSCryptoTest {
  behavior of "MuSig2 Implementation"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it should "work for a single party" in {
    forAll(CryptoGenerators.privateKey, NumberGenerator.bytevector(32)) {
      case (privKey, msg) =>
        val pubKey = privKey.publicKey
        val noncePriv: MuSigNoncePriv = MuSigNoncePriv.gen(pubKey)
        val noncePub: MuSigNoncePub = noncePriv.toNoncePub
        val keySet = KeySet(pubKey)
        val aggMuSigNoncePub = MuSigNoncePub.aggregate(Vector(noncePub))

        assert(aggMuSigNoncePub == noncePub)

        val (aggNonce, s) =
          sign(noncePriv, aggMuSigNoncePub, privKey, msg, keySet)

        assert(
          partialSigVerify(
            s,
            noncePub,
            aggMuSigNoncePub,
            pubKey,
            keySet,
            msg
          )
        )

        val sig = signAgg(Vector(s), aggNonce)

        assert(
          sig == SchnorrDigitalSignature(aggNonce.schnorrNonce,
                                         s,
                                         hashTypeOpt = None))

        val aggPub = keySet.aggPubKey

        assert(
          aggPub.schnorrPublicKey == pubKey
            .multiply(keySet.keyAggCoef(pubKey))
            .schnorrPublicKey
        )

        assert(aggPub.schnorrPublicKey.verify(msg, sig))
    }
  }

  it should "work for two parties" in {
    forAll(
      CryptoGenerators.privateKey,
      CryptoGenerators.privateKey,
      NumberGenerator.bytevector(32)
    ) { case (priv1, priv2, msg) =>
      val pub1 = priv1.publicKey
      val noncePriv1: MuSigNoncePriv = MuSigNoncePriv.gen(pub1)
      val noncePub1: MuSigNoncePub = noncePriv1.toNoncePub
      val pub2 = priv2.publicKey
      val noncePriv2: MuSigNoncePriv = MuSigNoncePriv.gen(pub2)
      val noncePub2: MuSigNoncePub = noncePriv2.toNoncePub
      val keySet: KeySet = KeySet(pub1, pub2)
      val aggMuSigNoncePub =
        MuSigNoncePub.aggregate(Vector(noncePub1, noncePub2))
      val (aggNonce1, s1) =
        sign(noncePriv1, aggMuSigNoncePub, priv1, msg, keySet)
      val (aggNonce2, s2) =
        sign(noncePriv2, aggMuSigNoncePub, priv2, msg, keySet)

      assert(aggNonce1 == aggNonce2)
      assert(
        partialSigVerify(
          s1,
          noncePub1,
          aggMuSigNoncePub,
          pub1,
          keySet,
          msg
        )
      )
      assert(
        partialSigVerify(
          s2,
          noncePub2,
          aggMuSigNoncePub,
          pub2,
          keySet,
          msg
        )
      )

      val sig = signAgg(Vector(s1, s2), aggNonce1)
      val aggPub = keySet.aggPubKey

      assert(aggPub.schnorrPublicKey.verify(msg, sig))
    }
  }

  it should "work for more parties" in {
    val privKeysGen: Gen[Vector[ECPrivateKey]] = Gen
      .choose[Int](2, 20)
      .flatMap(n => Gen.listOfN(n, CryptoGenerators.privateKey))
      .map(_.toVector)

    forAll(
      privKeysGen,
      NumberGenerator.bytevector(32)
    ) { case (privKeysUnsorted, msg) =>
      val keySet: KeySet = KeySet(privKeysUnsorted.map(_.publicKey))
      val privKeys = keySet.keys.map(pubKey =>
        privKeysUnsorted.find(_.publicKey == pubKey).get)
      val noncePrivs = privKeys.map(pk => MuSigNoncePriv.gen(pk.publicKey))
      val noncePubs = noncePrivs.map(_.toNoncePub)
      val aggMuSigNoncePub = MuSigNoncePub.aggregate(noncePubs)
      val partialSigs: Vector[(ECPublicKey, FieldElement)] =
        privKeys.zipWithIndex.map { case (privKey, i) =>
          sign(noncePrivs(i), aggMuSigNoncePub, privKey, msg, keySet)
        }

      // All aggregate nonces are the same
      assert(partialSigs.map(_._1).forall(_ == partialSigs.head._1))
      // All partial sigs are valid
      assert(partialSigs.map(_._2).zipWithIndex.forall { case (s, i) =>
        partialSigVerify(
          s,
          noncePubs(i),
          aggMuSigNoncePub,
          privKeys(i).publicKey,
          keySet,
          msg
        )
      })

      val sig = signAgg(partialSigs.map(_._2), partialSigs.head._1)
      val aggPub = keySet.aggPubKey

      assert(aggPub.schnorrPublicKey.verify(msg, sig))
    }
  }

  it should "work with tweaks" in {
    val privKeysGen: Gen[Vector[ECPrivateKey]] = Gen
      .choose[Int](2, 20)
      .flatMap(n => Gen.listOfN(n, CryptoGenerators.privateKey))
      .map(_.toVector)

    val tweaksGen: Gen[Vector[MuSigTweak]] = Gen
      .choose[Int](0, 10)
      .flatMap(n =>
        Gen.listOfN(
          n,
          CryptoGenerators.fieldElement.flatMap(x =>
            NumberGenerator.bool.map((x, _)))
        ))
      .map(_.toVector)
      .map(_.map { case (x, b) => MuSigTweak(x, b) })

    forAll(
      privKeysGen,
      NumberGenerator.bytevector(32),
      tweaksGen
    ) { case (privKeysUnsorted, msg, tweaks) =>
      val keySet: KeySet =
        KeySet(privKeysUnsorted.map(_.publicKey), tweaks)
      val privKeys = keySet.keys.map(pubKey =>
        privKeysUnsorted.find(_.publicKey == pubKey).get)
      val noncePrivs = privKeys.map(pk => MuSigNoncePriv.gen(pk.publicKey))
      val noncePubs = noncePrivs.map(_.toNoncePub)
      val aggMuSigNoncePub = MuSigNoncePub.aggregate(noncePubs)
      val partialSigs: Vector[(ECPublicKey, FieldElement)] =
        privKeys.zipWithIndex.map { case (privKey, i) =>
          sign(noncePrivs(i), aggMuSigNoncePub, privKey, msg, keySet)
        }

      // All aggregate nonces are the same
      assert(partialSigs.map(_._1).forall(_ == partialSigs.head._1))
      // All partial sigs are valid
      assert(partialSigs.map(_._2).zipWithIndex.forall { case (s, i) =>
        partialSigVerify(
          s,
          noncePubs(i),
          aggMuSigNoncePub,
          privKeys(i).publicKey,
          keySet,
          msg
        )
      })

      val sig = signAgg(partialSigs.map(_._2), aggMuSigNoncePub, keySet, msg)
      val aggPub = keySet.aggPubKey

      assert(aggPub.schnorrPublicKey.verify(msg, sig))
    }
  }

  // https://github.com/jonasnick/bips/blob/263a765a77e20efe883ed3b28dc155a0d8c7d61a/bip-musig2/reference.py#L461
  it should "pass nonce aggregation test vectors" in {
    val pnonce = Vector(
      "020151C80F435648DF67A22B749CD798CE54E0321D034B92B709B567D60A42E666" ++
        "03BA47FBC1834437B3212E89A84D8425E7BF12E0245D98262268EBDCB385D50641",
      "03FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6" ++
        "0248C264CDD57D3C24D79990B0F865674EB62A0F9018277A95011B41BFC193B833"
    ).map(MuSigNoncePub.fromHex)

    val expected = MuSigNoncePub(
      "035FE1873B4F2967F52FEA4A06AD5A8ECCBE9D0FD73068012C894E2E87CCB5804B" ++
        "024725377345BDE0E9C33AF3C43C0A29A9249F2F2956FA8CFEB55C8573D0262DC8"
    )

    // Vector 1
    assert(MuSigNoncePub.aggregate(pnonce) == expected)

    // The following errors must be handled by the caller as we can't even represent them
    // Vector 2
    assertThrows[IllegalArgumentException](
      MuSigNoncePub(
        "04FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6" ++
          "0248C264CDD57D3C24D79990B0F865674EB62A0F9018277A95011B41BFC193B833"
      )
    )
    // Vector 3
    assertThrows[IllegalArgumentException](
      MuSigNoncePub(
        "03FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6" ++
          "0248C264CDD57D3C24D79990B0F865674EB62A0F9018277A95011B41BFC193B831"
      )
    )
    // Vector 4
    assertThrows[IllegalArgumentException](
      MuSigNoncePub(
        "03FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6" ++
          "02FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"
      )
    )

    // Vector 5
    val g = CryptoParams.getG.toPoint
    val negG = g.multiply(FieldElement.orderMinusOne)
    val pnonce1 = MuSigNoncePub(Vector(pnonce.head.pubNonces.head, g))
    val pnonce2 = MuSigNoncePub(Vector(pnonce.last.pubNonces.head, negG))
    val expected5 =
      MuSigNoncePub(expected.bytes.take(33) ++ MuSigNoncePub.infPtBytes)
    assert(MuSigNoncePub.aggregate(Vector(pnonce1, pnonce2)) == expected5)
  }
}
