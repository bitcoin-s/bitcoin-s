package org.bitcoins.crypto

import org.bitcoins.crypto.MuSig2Util._
import org.scalacheck.Gen

class MuSig2UtilTest extends BitcoinSCryptoTest {
  behavior of "MuSig2Util"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it should "work for a single party" in {
    forAll(CryptoGenerators.privateKey, NumberGenerator.bytevector(32)) {
      case (privKey, msg) =>
        val pubKey = privKey.publicKey
        val (noncePub: MultiNoncePub, noncePriv: MultiNoncePriv) =
          genMultiNonce()
        val keySet = KeySet(pubKey.schnorrPublicKey)
        val aggMultiNoncePub = aggNonces(Vector(noncePub))

        assert(aggMultiNoncePub == noncePub)

        val (aggNonce, s) =
          sign(noncePriv, aggMultiNoncePub, privKey, msg, keySet)

        assert(
          partialSigVerify(s,
                           noncePub,
                           aggMultiNoncePub,
                           pubKey.schnorrPublicKey,
                           keySet,
                           msg))

        val sig = signAgg(Vector(s), aggNonce)

        assert(sig == SchnorrDigitalSignature(aggNonce.schnorrNonce, s))

        val aggPub = keySet.aggPubKey

        assert(
          aggPub.schnorrPublicKey == pubKey
            .multiply(keySet.keyAggCoef(pubKey.schnorrPublicKey))
            .schnorrPublicKey)

        assert(aggPub.schnorrPublicKey.verify(msg, sig))
    }
  }

  it should "work for two parties" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32)) { case (priv1, priv2, msg) =>
      val pub1 = priv1.publicKey
      val (noncePub1: MultiNoncePub, noncePriv1: MultiNoncePriv) =
        genMultiNonce()
      val pub2 = priv2.publicKey
      val (noncePub2: MultiNoncePub, noncePriv2: MultiNoncePriv) =
        genMultiNonce()
      val keySet: KeySet = KeySet(pub1.schnorrPublicKey, pub2.schnorrPublicKey)
      val aggMultiNoncePub = aggNonces(Vector(noncePub1, noncePub2))
      val (aggNonce1, s1) =
        sign(noncePriv1, aggMultiNoncePub, priv1, msg, keySet)
      val (aggNonce2, s2) =
        sign(noncePriv2, aggMultiNoncePub, priv2, msg, keySet)

      assert(aggNonce1 == aggNonce2)
      assert(
        partialSigVerify(s1,
                         noncePub1,
                         aggMultiNoncePub,
                         pub1.schnorrPublicKey,
                         keySet,
                         msg))
      assert(
        partialSigVerify(s2,
                         noncePub2,
                         aggMultiNoncePub,
                         pub2.schnorrPublicKey,
                         keySet,
                         msg))

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
      val keySet: KeySet = KeySet(privKeysUnsorted.map(_.schnorrPublicKey))
      val privKeys = keySet.keys.map(pubKey =>
        privKeysUnsorted.find(_.schnorrPublicKey == pubKey).get)
      val nonceData: Vector[(MultiNoncePub, MultiNoncePriv)] =
        privKeys.map(_ => genMultiNonce())
      val aggMultiNoncePub = aggNonces(nonceData.map(_._1))
      val partialSigs: Vector[(ECPublicKey, FieldElement)] =
        privKeys.zipWithIndex.map { case (privKey, i) =>
          sign(nonceData(i)._2, aggMultiNoncePub, privKey, msg, keySet)
        }

      // All aggregate nonces are the same
      assert(partialSigs.map(_._1).forall(_ == partialSigs.head._1))
      // All partial sigs are valid
      assert(partialSigs.map(_._2).zipWithIndex.forall { case (s, i) =>
        partialSigVerify(s,
                         nonceData(i)._1,
                         aggMultiNoncePub,
                         privKeys(i).schnorrPublicKey,
                         keySet,
                         msg)
      })

      val sig = signAgg(partialSigs.map(_._2), partialSigs.head._1)
      val aggPub = keySet.aggPubKey

      assert(aggPub.schnorrPublicKey.verify(msg, sig))
    }
  }

  it must "pass test vectors" in {
    val inputs = Vector(
      "F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9",
      "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      "3590A94E768F8E1815C2F24B4D80A8E3149316C3518CE7B7AD338368D038CA66"
    ).map(SchnorrPublicKey.fromHex)

    val expected =
      Vector(
        "E5830140512195D74C8307E39637CBE5FB730EBEAB80EC514CF88A877CEEEE0B",
        "D70CD69A2647F7390973DF48CBFA2CCC407B8B2D60B08C5F1641185C7998A290",
        "81A8B093912C9E481408D09776CEFB48AEB8B65481B6BAAFB3C5810106717BEB",
        "2EB18851887E7BDC5E830E89B19DDBC28078F1FA88AAD0AD01CA06FE4F80210B"
      ).map(SchnorrPublicKey.fromHex)

    // Vector 1
    assert(UnsortedKeySet(inputs).aggPubKey.schnorrPublicKey == expected(0))
    // Vector 2
    assert(
      UnsortedKeySet(inputs.reverse).aggPubKey.schnorrPublicKey == expected(1))
    // Vector 3
    assert(
      UnsortedKeySet(
        Vector.fill(3)(inputs(0))).aggPubKey.schnorrPublicKey == expected(2))
    // Vector 4
    assert(
      UnsortedKeySet(
        Vector(inputs(0),
               inputs(0),
               inputs(1),
               inputs(1))).aggPubKey.schnorrPublicKey == expected(3))
  }
}
