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

  it should "work with tweaks" in {
    val privKeysGen: Gen[Vector[ECPrivateKey]] = Gen
      .choose[Int](2, 20)
      .flatMap(n => Gen.listOfN(n, CryptoGenerators.privateKey))
      .map(_.toVector)

    val tweaksGen: Gen[Vector[Tweak]] = Gen
      .choose[Int](0, 10)
      .flatMap(n =>
        Gen.listOfN(n,
                    CryptoGenerators.fieldElement.flatMap(x =>
                      NumberGenerator.bool.map((x, _)))))
      .map(_.toVector)
      .map(_.map { case (x, b) => Tweak(x, b) })

    forAll(
      privKeysGen,
      NumberGenerator.bytevector(32),
      tweaksGen
    ) { case (privKeysUnsorted, msg, tweaks) =>
      val keySet: KeySet =
        KeySet(privKeysUnsorted.map(_.schnorrPublicKey), tweaks)
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

      val sig = signAgg(partialSigs.map(_._2), aggMultiNoncePub, keySet, msg)
      val aggPub = keySet.aggPubKey

      assert(aggPub.schnorrPublicKey.verify(msg, sig))
    }
  }

  // https://github.com/jonasnick/bips/blob/263a765a77e20efe883ed3b28dc155a0d8c7d61a/bip-musig2/reference.py#L391
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
    // Vector 7
    assertThrows[IllegalArgumentException](
      Tweak(
        FieldElement(
          "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"),
        isXOnlyT = true))
    // Vector 8
    val schnorrG = CryptoParams.getG.schnorrPublicKey
    val keySetNoTweak = KeySet(schnorrG)
    val coeff = keySetNoTweak.keyAggCoef(schnorrG).negate
    val keySet =
      keySetNoTweak.withTweaks(Vector(Tweak(coeff, isXOnlyT = false)))
    assertThrows[Exception](keySet.aggPubKey)
  }

  // https://github.com/jonasnick/bips/blob/263a765a77e20efe883ed3b28dc155a0d8c7d61a/bip-musig2/reference.py#L436
  it should "pass nonce generation test vectors" in {
    val rand = ByteVector.fill(32)(0)
    val msg = Some(ByteVector.fill(32)(1))
    val sk = Some(ECPrivateKey(ByteVector.fill(32)(2)))
    // TODO make this a pubkey once it is changed in the BIP
    val aggpk = Some(ByteVector.fill(32)(3))
    val extraIn = Some(ByteVector.fill(32)(4))

    val expected = Vector(
      "149378B996F012F70C1016C29AE0A9B1D86AAD557CCD29178A24479756205DFD" ++
        "6285697E57E8578FCFDE6FE7FA4B6CA8E3A5AECAB4FB92F84B7F9DF8DDEF4DB8",
      "6613EA4A36A50F34990EAF73612EAA28636ED01325793456C04D8D89DB49372B" ++
        "54E9DB34D09CBD394DB3FE3CCFFBCA6ED016F3AC877938613095893F54FA70DF",
      "7B3B5A002356471AF0E961DE2549C121BD0D48ABCEEDC6E034BDDF86AD3E0A18" ++
        "7ECEE674CEF7364B0BC4BEEFB8B66CAD89F98DE2F8C5A5EAD5D1D1E4BD7D04CD"
    ).map(MultiNoncePriv.fromHex)

    val nonce1 = genMultiNonceInternal(rand, sk, aggpk, msg, extraIn)
    // Vector 1
    assert(nonce1._2 == expected(0))

    val nonce2 = genMultiNonceInternal(rand, sk, aggpk, msgOpt = None, extraIn)
    // Vector 2
    assert(nonce2._2 == expected(1))

    val nonce3 = genMultiNonceInternal(rand)
    // Vector 3
    assert(nonce3._2 == expected(2))
  }

  // https://github.com/jonasnick/bips/blob/263a765a77e20efe883ed3b28dc155a0d8c7d61a/bip-musig2/reference.py#L461
  it should "pass nonce aggregation test vectors" in {
    val pnonce = Vector(
      "020151C80F435648DF67A22B749CD798CE54E0321D034B92B709B567D60A42E666" ++
        "03BA47FBC1834437B3212E89A84D8425E7BF12E0245D98262268EBDCB385D50641",
      "03FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6" ++
        "0248C264CDD57D3C24D79990B0F865674EB62A0F9018277A95011B41BFC193B833"
    ).map(MultiNoncePub.fromHex)

    val expected = MultiNoncePub(
      "035FE1873B4F2967F52FEA4A06AD5A8ECCBE9D0FD73068012C894E2E87CCB5804B" ++
        "024725377345BDE0E9C33AF3C43C0A29A9249F2F2956FA8CFEB55C8573D0262DC8"
    )

    // Vector 1
    assert(aggNonces(pnonce) == expected)

    // The following errors must be handled by the caller as we can't even represent them
    // Vector 2
    assertThrows[IllegalArgumentException](
      MultiNoncePub(
        "04FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6" ++
          "0248C264CDD57D3C24D79990B0F865674EB62A0F9018277A95011B41BFC193B833"))
    // Vector 3
    assertThrows[IllegalArgumentException](
      MultiNoncePub(
        "03FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6" ++
          "0248C264CDD57D3C24D79990B0F865674EB62A0F9018277A95011B41BFC193B831"))
    // Vector 4
    assertThrows[IllegalArgumentException](
      MultiNoncePub(
        "03FF406FFD8ADB9CD29877E4985014F66A59F6CD01C0E88CAA8E5F3166B1F676A6" ++
          "02FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"))

    // Vector 5
    val g = CryptoParams.getG.toPoint
    val negG = g.multiply(FieldElement.orderMinusOne)
    val pnonce1 = MultiNoncePub(Vector(pnonce.head.pubNonces.head, g))
    val pnonce2 = MultiNoncePub(Vector(pnonce.last.pubNonces.head, negG))
    val expected5 =
      MultiNoncePub(expected.bytes.take(33) ++ MultiNoncePub.infPtBytes)
    assert(aggNonces(Vector(pnonce1, pnonce2)) == expected5)
  }

  // https://github.com/jonasnick/bips/blob/263a765a77e20efe883ed3b28dc155a0d8c7d61a/bip-musig2/reference.py#L504
  it should "pass signing and verification test vectors" in {
    val remotePubKeys = Vector(
      "F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9",
      "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659"
    ).map(SchnorrPublicKey.fromHex)

    val privNonce = MultiNoncePriv(
      "508B81A611F100A6B2B6B29656590898AF488BCF2E1F55CF22E5CFB84421FE61" ++
        "FA27FD49B1D50085B481285E1CA205D55C82CC1B31FF5CD54A489829355901F7")

    val pubNonces = Vector(
      "0337C87821AFD50A8644D820A8F3E02E499C931865C2360FB43D0A0D20DAFE07EA" ++
        "0287BF891D2A6DEAEBADC909352AA9405D1428C15F4B75F04DAE642A95C2548480",
      "0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" ++
        "0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798",
      "032DE2662628C90B03F5E720284EB52FF7D71F4284F627B68A853D78C78E1FFE93" ++
        "03E4C5524E83FFE1493B9077CF1CA6BEB2090C93D930321071AD40B2F44E599046",
      "0237C87821AFD50A8644D820A8F3E02E499C931865C2360FB43D0A0D20DAFE07EA" ++
        "0387BF891D2A6DEAEBADC909352AA9405D1428C15F4B75F04DAE642A95C2548480"
    ).map(MultiNoncePub.fromHex)

    assert(privNonce.toPublicNonces == pubNonces.head)

    val aggNonce = MultiNoncePub(
      "028465FCF0BBDBCF443AABCCE533D42B4B5A10966AC09A49655E8C42DAAB8FCD61" ++
        "037496A3CC86926D452CAFCFD55D25972CA1675D549310DE296BFF42F72EEEA8C9")

    assert(aggNonce == aggNonces(pubNonces.take(3)))

    val sk = ECPrivateKey(
      "7FB9E0E687ADA1EEBF7ECFE2F21E73EBDB51A7D450948DFE8D76D7F2D1007671")
    val pk = sk.schnorrPublicKey
    val msg = ByteVector.fromValidHex(
      "F95466D086770E689964664219266FE5ED215C92AE20BAB5C9D79ADDDDF3C0CF")

    val expected = Vector(
      "68537CC5234E505BD14061F8DA9E90C220A181855FD8BDB7F127BB12403B4D3B",
      "2DF67BFFF18E3DE797E13C6475C963048138DAEC5CB20A357CECA7C8424295EA",
      "0D5B651E6DE34A29A12DE7A8B4183B4AE6A7F7FBE15CDCAFA4A3D1BCAABC7517",
      "8D5E0407FB4756EEBCD86264C32D792EE36EEB69E952BBB30B8E41BEBC4D22FA"
    ).map(FieldElement.fromHex)

    // Vector 1
    val keySet1 = UnsortedKeySet(Vector(pk, remotePubKeys(0), remotePubKeys(1)))
    assert(sign(privNonce, aggNonce, sk, msg, keySet1)._2 == expected(0))

    // Vector 2
    val keySet2 = UnsortedKeySet(Vector(remotePubKeys(0), pk, remotePubKeys(1)))
    assert(sign(privNonce, aggNonce, sk, msg, keySet2)._2 == expected(1))

    // Vector 3
    val keySet3 = UnsortedKeySet(Vector(remotePubKeys(0), remotePubKeys(1), pk))
    assert(sign(privNonce, aggNonce, sk, msg, keySet3)._2 == expected(2))

    // Vector 4
    val infAggNonce = aggNonces(Vector(pubNonces(0), pubNonces(3)))
    assert(infAggNonce.pubNonces.forall(_ == SecpPointInfinity))
    val keySet4 = UnsortedKeySet(Vector(pk, remotePubKeys(0)))
    assert(sign(privNonce, infAggNonce, sk, msg, keySet4)._2 == expected(3))

    // The following errors must be handled by the caller as we can't even represent them
    // Vector 5, 17
    assertThrows[IllegalArgumentException](
      SchnorrPublicKey(
        "0000000000000000000000000000000000000000000000000000000000000007"))
    // Vector 6
    assertThrows[IllegalArgumentException](
      MultiNoncePub(
        "048465FCF0BBDBCF443AABCCE533D42B4B5A10966AC09A49655E8C42DAAB8FCD61" ++
          "037496A3CC86926D452CAFCFD55D25972CA1675D549310DE296BFF42F72EEEA8C9"))
    // Vector 7
    assertThrows[IllegalArgumentException](
      MultiNoncePub(
        "028465FCF0BBDBCF443AABCCE533D42B4B5A10966AC09A49655E8C42DAAB8FCD61" ++
          "020000000000000000000000000000000000000000000000000000000000000009"))
    // Vector 8
    assertThrows[IllegalArgumentException](
      MultiNoncePub(
        "028465FCF0BBDBCF443AABCCE533D42B4B5A10966AC09A49655E8C42DAAB8FCD61" ++
          "02FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"))

    // Verification test vectors
    // Vector 9
    assert(
      partialSigVerify(expected(0),
                       pubNonces.take(3),
                       keySet1,
                       msg,
                       signerIndex = 0))
    // Vector 10
    assert(
      partialSigVerify(expected(1),
                       Vector(pubNonces(1), pubNonces(0), pubNonces(2)),
                       keySet2,
                       msg,
                       signerIndex = 1))
    // Vector 11
    assert(
      partialSigVerify(expected(2),
                       Vector(pubNonces(1), pubNonces(2), pubNonces(0)),
                       keySet3,
                       msg,
                       signerIndex = 2))

    // Vector 12
    assert(
      partialSigVerify(expected(3),
                       Vector(pubNonces(0), pubNonces(3)),
                       keySet4,
                       msg,
                       signerIndex = 0))

    // Vector 13
    val badSig = FieldElement(
      "97AC833ADCB1AFA42EBF9E0725616F3C9A0D5B614F6FE283CEAAA37A8FFAF406")
    assert(!partialSigVerify(badSig, pubNonces, keySet1, msg, signerIndex = 0))
    // Vector 14
    assert(
      !partialSigVerify(expected(0),
                        pubNonces.take(3),
                        keySet1,
                        msg,
                        signerIndex = 1))
    // The following errors must be handled by the caller as we can't even represent them
    // Vector 15
    assertThrows[IllegalArgumentException](
      FieldElement(
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"))
    // Vector 16
    assertThrows[IllegalArgumentException](
      MultiNoncePub(
        "020000000000000000000000000000000000000000000000000000000000000009" ++
          pubNonces.head.bytes.drop(33).toHex))
  }

  // https://github.com/jonasnick/bips/blob/musig2/bip-musig2/reference.py#L629
  it should "pass tweak test vectors" in {
    val pubKeys = Vector(
      "F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9",
      "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659"
    ).map(SchnorrPublicKey.fromHex)

    val secnonce = MultiNoncePriv(
      "508B81A611F100A6B2B6B29656590898AF488BCF2E1F55CF22E5CFB84421FE61" ++
        "FA27FD49B1D50085B481285E1CA205D55C82CC1B31FF5CD54A489829355901F7")

    val pnonce = Vector(
      "0337C87821AFD50A8644D820A8F3E02E499C931865C2360FB43D0A0D20DAFE07EA" ++
        "0287BF891D2A6DEAEBADC909352AA9405D1428C15F4B75F04DAE642A95C2548480",
      "0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" ++
        "0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798",
      "032DE2662628C90B03F5E720284EB52FF7D71F4284F627B68A853D78C78E1FFE93" ++
        "03E4C5524E83FFE1493B9077CF1CA6BEB2090C93D930321071AD40B2F44E599046"
    ).map(MultiNoncePub.fromHex)

    val aggnonce = MultiNoncePub(
      "028465FCF0BBDBCF443AABCCE533D42B4B5A10966AC09A49655E8C42DAAB8FCD61" ++
        "037496A3CC86926D452CAFCFD55D25972CA1675D549310DE296BFF42F72EEEA8C9")

    val sk = ECPrivateKey(
      "7FB9E0E687ADA1EEBF7ECFE2F21E73EBDB51A7D450948DFE8D76D7F2D1007671")
    val msg = ByteVector.fromValidHex(
      "F95466D086770E689964664219266FE5ED215C92AE20BAB5C9D79ADDDDF3C0CF")

    val tweaks = Vector(
      "E8F791FF9225A2AF0102AFFF4A9A723D9612A682A25EBE79802B263CDFCD83BB",
      "AE2EA797CC0FE72AC5B97B97F3C6957D7E4199A167A58EB08BCAFFDA70AC0455",
      "F52ECBC565B3D8BEA2DFD5B75A4F457E54369809322E4120831626F290FA87E0",
      "1969AD73CC177FA0B4FCED6DF1F7BF9907E665FDE9BA196A74FED0A3CF5AEF9D"
    ).map(FieldElement.fromHex)

    val expected = Vector(
      "5E24C7496B565DEBC3B9639E6F1304A21597F9603D3AB05B4913641775E1375B",
      "78408DDCAB4813D1394C97D493EF1084195C1D4B52E63ECD7BC5991644E44DDD",
      "C3A829A81480E36EC3AB052964509A94EBF34210403D16B226A6F16EC85B7357",
      "8C4473C6A382BD3C4AD7BE59818DA5ED7CF8CEC4BC21996CFDA08BB4316B8BC7"
    ).map(FieldElement.fromHex)

    val pk = sk.schnorrPublicKey

    val keySet = UnsortedKeySet(Vector(pubKeys(0), pubKeys(1), pk))
    val pnonces = Vector(pnonce(1), pnonce(2), pnonce(0))

    // Vector 1
    val tweaks1 = Vector(Tweak(tweaks(0), isXOnlyT = true))
    val keySet1 = keySet.withTweaks(tweaks1)
    assert(sign(secnonce, aggnonce, sk, msg, keySet1)._2 == expected(0))
    assert(
      partialSigVerify(expected(0), pnonces, keySet1, msg, signerIndex = 2))

    // Vector 2
    val tweaks2 = Vector(Tweak(tweaks(0), isXOnlyT = false))
    val keySet2 = keySet.withTweaks(tweaks2)
    assert(sign(secnonce, aggnonce, sk, msg, keySet2)._2 == expected(1))
    assert(
      partialSigVerify(expected(1), pnonces, keySet2, msg, signerIndex = 2))

    // Vector 3
    val tweaks3 = Vector(Tweak(tweaks(0), isXOnlyT = false),
                         Tweak(tweaks(1), isXOnlyT = true))
    val keySet3 = keySet.withTweaks(tweaks3)
    assert(sign(secnonce, aggnonce, sk, msg, keySet3)._2 == expected(2))
    assert(
      partialSigVerify(expected(2), pnonces, keySet3, msg, signerIndex = 2))

    // Vector 4
    val tweaks4 = Vector(Tweak(tweaks(0), isXOnlyT = true),
                         Tweak(tweaks(1), isXOnlyT = false),
                         Tweak(tweaks(2), isXOnlyT = true),
                         Tweak(tweaks(3), isXOnlyT = false))
    val keySet4 = keySet.withTweaks(tweaks4)
    assert(sign(secnonce, aggnonce, sk, msg, keySet4)._2 == expected(3))
    assert(
      partialSigVerify(expected(3), pnonces, keySet4, msg, signerIndex = 2))

    // The following error must be handled by the caller as we can't even represent it
    // Vector 5
    assertThrows[IllegalArgumentException](
      Tweak(
        FieldElement(
          "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"),
        isXOnlyT = false))
  }

  // https://github.com/jonasnick/bips/blob/263a765a77e20efe883ed3b28dc155a0d8c7d61a/bip-musig2/reference.py#L702
  it should "pass signature aggregation test vectors" in {
    val pubKeys = Vector(
      "487D1B83B41B4CBBD07A111F1BBC7BDC8864CFEF5DBF96E46E51C68399B0BEF6",
      "4795C22501BF534BC478FF619407A7EC9E8D8883646D69BD43A0728944EA802F",
      "0F5BE837F3AB7E7FEFF1FAA44D673C2017206AE836D2C7893CDE4ACB7D55EDEB",
      "0FD453223E444FCA91FB5310990AE8A0C5DAA14D2A4C8944E1C0BC80C30DF682"
    ).map(SchnorrPublicKey.fromHex)

    val aggNonce = Vector(
      "024FA51009A56F0D6DF737131CE1FBBD833797AF3B4FE6BF0D68F4D49F68B0947E" ++
        "0248FB3BB9191F0CFF13806A3A2F1429C23012654FCE4E41F7EC9169EAA6056B21",
      "023B11E63E2460E5E0F1561BB700FEA95B991DD9CA2CBBE92A3960641FA7469F67" ++
        "02CA4CD38375FE8BEB857C770807225BFC7D712F42BA896B83FC71138E56409B21",
      "03F98BEAA32B8A38FE3797C4E813DC9CE05ADBE32200035FB37EB0A030B735E9B6" ++
        "030E6118EC98EA2BA7A358C2E38E7E13E63681EEB683E067061BF7D52DCF08E615",
      "026491FBCFD47148043A0F7310E62EF898C10F2D0376EE6B232EAAD36F3C2E29E3" ++
        "03020CB17D168908E2904DE2EB571CD232CA805A6981D0F86CDBBD2F12BD91F6D0",
      "000000000000000000000000000000000000000000000000000000000000000000" ++
        "000000000000000000000000000000000000000000000000000000000000000000"
    ).map(MultiNoncePub.fromHex)

    val msg = ByteVector.fromValidHex(
      "599C67EA410D005B9DA90817CF03ED3B1C868E4DA4EDF00A5880B0082C237869")

    val tweaks = Vector(
      "B511DA492182A91B0FFB9A98020D55F260AE86D7ECBD0399C7383D59A5F2AF7C",
      "A815FE049EE3C5AAB66310477FBC8BCCCAC2F3395F59F921C364ACD78A2F48DC",
      "75448A87274B056468B977BE06EB1E9F657577B7320B0A3376EA51FD420D18A8"
    ).map(FieldElement.fromHex)

    val psig = Vector(
      "E5C1CBD6E7E89FE9EE30D5F3B6D06B9C218846E4A1DEF4EE851410D51ABBD850",
      "9BC470F7F1C9BC848BDF179B0023282FFEF40908E0EF88459784A4355FC86D0C",
      "D5D8A09929BA264B2F5DF15ACA1CF2DEFA47C048DF0C3232E965FFE2F2831B1D",
      "A915197503C1051EA77DC91F01C3A0E60BFD64473BD536CB613F9645BD61C843",
      "99A144D7076A128022134E036B8BDF33811F7EAED9A1E48549B46D8A63D64DC9",
      "716A72A0C1E531EBB4555C8E29FD35C796F4F231C3B039193D7E8D7AEFBDF5F7",
      "06B6DD04BC0F1EF740916730AD7DAC794255B161221719765BDE9686A26633DC",
      "BF6D85D4930062726EBC6EBB184AFD68DBB3FED159C501989690A62600D6FBAB"
    ).map(FieldElement.fromHex)

    val expected = Vector(
      "4006D4D069F3B51E968762FF8074153E278E5BCD221AABE0743CA001B77E79F5" ++
        "81863CCED9B25C6E7A0FED8EB6F393CD65CD7306D385DCF85CC6567DAA4E041B",
      "98BCD40DFD94B47A3DA37D7B78EB6CCE8ABEACA23C3ADE6F4678902410EB35C6" ++
        "7EEDBA0E2D7B2B69D6DBBA79CBE093C64B9647A96B98C8C28AD3379BDFAEA21F",
      "3741FEDCCDD7508B58DCB9A780FF5D97452EC8C0448D8C97004EA7175C14F200" ++
        "7A54D1DE356EBA6719278436EF111DFA8F1B832368371B9B7A25001709039679",
      "F4B3DA3CF0D0F7CF5C1840593BF1A1A415DA341619AE848F2210696DC8C75125" ++
        "40962C84EF7F0CEC491065F2D577213CF10E8A63D153297361B3B172BE27B61F"
    ).map(SchnorrDigitalSignature.fromHex)

    // Vector 1
    val keySet1 = UnsortedKeySet(pubKeys.take(2))
    val sig1 = signAgg(psig.take(2), aggNonce(0), keySet1, msg)
    assert(sig1 == expected(0))
    assert(keySet1.aggPubKey.schnorrPublicKey.verify(msg, sig1))

    // Vector 2
    val keySet2 = UnsortedKeySet(Vector(pubKeys(0), pubKeys(2)))
    val sig2 = signAgg(psig.slice(2, 4), aggNonce(1), keySet2, msg)
    assert(sig2 == expected(1))
    assert(keySet2.aggPubKey.schnorrPublicKey.verify(msg, sig2))

    // Vector 3
    val tweaks3 = Vector(Tweak(tweaks(0), isXOnlyT = false))
    val keySet3 = UnsortedKeySet(Vector(pubKeys(0), pubKeys(2)), tweaks3)
    val sig3 = signAgg(psig.slice(4, 6), aggNonce(2), keySet3, msg)
    assert(sig3 == expected(2))
    assert(keySet3.aggPubKey.schnorrPublicKey.verify(msg, sig3))

    // Vector 4
    val tweaks4 = Vector(Tweak(tweaks(0), isXOnlyT = true),
                         Tweak(tweaks(1), isXOnlyT = false),
                         Tweak(tweaks(2), isXOnlyT = true))
    val keySet4 = UnsortedKeySet(Vector(pubKeys(0), pubKeys(3)), tweaks4)
    val sig4 = signAgg(psig.slice(6, 8), aggNonce(3), keySet4, msg)
    assert(sig4 == expected(3))
    assert(keySet4.aggPubKey.schnorrPublicKey.verify(msg, sig4))

    // The following error must be handled by the caller as we can't even represent it
    // Vector 5
    assertThrows[IllegalArgumentException](
      FieldElement(
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"))
  }
}
