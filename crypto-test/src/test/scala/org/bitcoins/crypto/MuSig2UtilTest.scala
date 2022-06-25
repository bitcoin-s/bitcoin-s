package org.bitcoins.crypto

import org.bitcoins.crypto.MuSig2Util._
import org.scalacheck.Gen
import scodec.bits.ByteVector

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

  it should "pass key aggregation test vectors" in {
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

    // The following errors must be handled by the caller as we can't even represent them
    // Vector 5
    assertThrows[IllegalArgumentException](
      SchnorrPublicKey.fromHex(
        "0000000000000000000000000000000000000000000000000000000000000005"))
    // Vector 6
    assertThrows[IllegalArgumentException](
      SchnorrPublicKey.fromHex(
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"))

    // TODO Vectors 7 and 8 are tweaking related
  }

  it should "pass nonce generation test vectors" in {
    val rand = ByteVector.fill(32)(0)
    val msg = Some(ByteVector.fill(32)(1))
    val sk = Some(ECPrivateKey(ByteVector.fill(32)(2)))
    val aggpk = Some(ByteVector.fill(32)(3))
    val extraIn = Some(ByteVector.fill(32)(4))

    val expected = Vector(
      "149378B996F012F70C1016C29AE0A9B1D86AAD557CCD29178A24479756205DFD" ++
        "6285697E57E8578FCFDE6FE7FA4B6CA8E3A5AECAB4FB92F84B7F9DF8DDEF4DB8",
      "6613EA4A36A50F34990EAF73612EAA28636ED01325793456C04D8D89DB49372B" ++
        "54E9DB34D09CBD394DB3FE3CCFFBCA6ED016F3AC877938613095893F54FA70DF",
      "7B3B5A002356471AF0E961DE2549C121BD0D48ABCEEDC6E034BDDF86AD3E0A18" ++
        "7ECEE674CEF7364B0BC4BEEFB8B66CAD89F98DE2F8C5A5EAD5D1D1E4BD7D04CD"
    ).map(ByteVector.fromValidHex(_))

    val nonce1 = genMultiNonceInternal(rand, sk, aggpk, msg, extraIn)
    // Vector 1
    assert(nonce1._2.bytes == expected(0))

    val nonce2 = genMultiNonceInternal(rand, sk, aggpk, msgOpt = None, extraIn)
    // Vector 2
    assert(nonce2._2.bytes == expected(1))

    val nonce3 = genMultiNonceInternal(rand)
    // Vector 3
    assert(nonce3._2.bytes == expected(2))
  }

  it should "pass nonce aggregation test vectors" in {
    val pnonce = Vector(
      "020151C80F435648DF67A22B749CD798CE54E0321D034B92B709B567D60A42E666" ++
        "03BA47FBC1834437B3212E89A84D8425E7BF12E0245D98262268EBDCB385D50641",
      "03FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6" ++
        "0248C264CDD57D3C24D79990B0F865674EB62A0F9018277A95011B41BFC193B833"
    ).map { hex =>
      val (p1, p2) = hex.splitAt(66)
      MultiNoncePub(Vector(p1, p2).map(ECPublicKey.fromHex))
    }

    val expected = ByteVector.fromValidHex(
      "035FE1873B4F2967F52FEA4A06AD5A8ECCBE9D0FD73068012C894E2E87CCB5804B" ++
        "024725377345BDE0E9C33AF3C43C0A29A9249F2F2956FA8CFEB55C8573D0262DC8"
    )

    // Vector 1
    assert(aggNonces(pnonce).bytes == expected)

    // The following errors must be handled by the caller as we can't even represent them
    // Vector 2
    assertThrows[IllegalArgumentException](
      ECPublicKey.fromHex(
        "04FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6"))
    // Vector 3
    assertThrows[IllegalArgumentException](
      ECPublicKey.fromHex(
        "0248C264CDD57D3C24D79990B0F865674EB62A0F9018277A95011B41BFC193B831"))
    // Vector 4
    assertThrows[IllegalArgumentException](
      ECPublicKey.fromHex(
        "02FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"))

    // Vector 5 FAILS because MultiNoncePub doesn't support infinity yet
    val g = CryptoParams.getG
    val negG = g.multiply(FieldElement.orderMinusOne)
    val pnonce1 = MultiNoncePub(Vector(pnonce.head.pubNonces.head, g))
    val pnonce2 = MultiNoncePub(Vector(pnonce.last.pubNonces.head, negG))
    val x = aggNonces(Vector(pnonce1, pnonce2)).bytes
    val y = expected.take(33) ++ ByteVector.fill(33)(0)
    assert(x == y, (x, y))
  }
}
