package org.bitcoins.core.crypto

import ExtKeyVersion._
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.crypto
import org.bitcoins.testkit.core.gen.{
  CryptoGenerators,
  HDGenerators,
  NumberGenerator
}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.HexStringSyntax

import scala.util.{Failure, Success, Try}

class ExtKeyTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "ExtKey"

  it must "derive similar keys for UInt32s and primitive numbers" in {
    forAll(CryptoGenerators.extPrivateKey, NumberGenerator.uInt32s) {
      (priv, index) =>
        val derivedPrivUInt32 = priv.deriveChildPrivKey(index)
        val derivedPrivPrimitive = priv.deriveChildPrivKey(index.toLong)
        assert(Success(derivedPrivUInt32) == derivedPrivPrimitive)

        val pub = priv.extPublicKey
        val derivedPubUInt32: Try[ExtPublicKey] = pub.deriveChildPubKey(index)
        val derivedPubPrimitive: Try[ExtPublicKey] =
          pub.deriveChildPubKey(index.toLong)
        (derivedPubUInt32, derivedPubPrimitive) match {
          case (Success(_), Success(_)) => succeed
          case (Failure(exc1), Failure(exc2)) =>
            assert(exc1.getMessage == exc2.getMessage)
          case _: (Try[ExtPublicKey], Try[ExtPublicKey]) => fail
        }
    }
  }

  it must "fail to make a private key out of a public key" in {
    forAll(CryptoGenerators.extPublicKey) { pub =>
      val attempt = ExtPrivateKey.fromString(pub.toString)
      attempt match {
        case Success(_)   => fail
        case Failure(exc) => assert(exc.getMessage.contains("expected private"))
      }
    }
  }

  it must "derive private keys from a BIP32 path and an xpriv" in {
    forAll(CryptoGenerators.extPrivateKey, HDGenerators.bip32Path) {
      (priv, path) =>
        priv.deriveChildPrivKey(path)
        succeed
    }
  }

  it must "derive public keys from a BIP32 path and an xpriv" in {
    forAll(CryptoGenerators.extPrivateKey, HDGenerators.bip32Path) {
      (priv, path) =>
        val pub = priv.deriveChildPubKey(path)
        pub match {
          case Failure(exc) => fail(exc.getMessage)
          case Success(_)   => succeed
        }
    }
  }

  it must "fail to derive public keys from a hardened public key" in {
    forAll(CryptoGenerators.extPrivateKey, HDGenerators.hardBip32Child) {
      (priv, child) =>
        val pub = priv.extPublicKey
        val derivedPub = pub.deriveChildPubKey(child.toUInt32)
        derivedPub match {
          case Success(_)   => fail
          case Failure(exc) => assert(exc.getMessage.contains("hardened"))
        }
    }
  }

  //https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#test-vectors
  it must "pass the test vectors in BIP32" in {
    //master key
    val seedBytes = hex"000102030405060708090a0b0c0d0e0f"

    val path = BIP32Path.empty

    val masterPriv =
      ExtPrivateKey(LegacyMainNetPriv, Some(seedBytes), path)
    masterPriv.toStringSensitive must be(
      "xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi")

    //master public key
    val masterPub = masterPriv.extPublicKey
    masterPub.toString must be(
      "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8")

    //derive child
    val m0hPath = BIP32Path.fromString("m/0'")
    val m0h = masterPriv.deriveChildPrivKey(m0hPath)
    m0h.toStringSensitive must be(
      "xprv9uHRZZhk6KAJC1avXpDAp4MDc3sQKNxDiPvvkX8Br5ngLNv1TxvUxt4cV1rGL5hj6KCesnDYUhd7oWgT11eZG7XnxHrnYeSvkzY7d2bhkJ7")

    val m0hPub = m0h.extPublicKey
    m0hPub.toString must be(
      "xpub68Gmy5EdvgibQVfPdqkBBCHxA5htiqg55crXYuXoQRKfDBFA1WEjWgP6LHhwBZeNK1VTsfTFUHCdrfp1bgwQ9xv5ski8PX9rL2dZXvgGDnw")

    val m0h1Path = BIP32Path.fromString("m/0'/1")
    val m0h1 = masterPriv.deriveChildPrivKey(m0h1Path)
    m0h1.toStringSensitive must be(
      "xprv9wTYmMFdV23N2TdNG573QoEsfRrWKQgWeibmLntzniatZvR9BmLnvSxqu53Kw1UmYPxLgboyZQaXwTCg8MSY3H2EU4pWcQDnRnrVA1xe8fs")

    val m0h1Pub = m0h1.extPublicKey
    m0h1Pub.toString must be(
      "xpub6ASuArnXKPbfEwhqN6e3mwBcDTgzisQN1wXN9BJcM47sSikHjJf3UFHKkNAWbWMiGj7Wf5uMash7SyYq527Hqck2AxYysAA7xmALppuCkwQ")

    val m0h1P2hath = BIP32Path.fromString("m/0'/1/2'")
    val m0h12h = masterPriv.deriveChildPrivKey(m0h1P2hath)
    m0h12h.toStringSensitive must be(
      "xprv9z4pot5VBttmtdRTWfWQmoH1taj2axGVzFqSb8C9xaxKymcFzXBDptWmT7FwuEzG3ryjH4ktypQSAewRiNMjANTtpgP4mLTj34bhnZX7UiM")

    val m0h12hPub = m0h12h.extPublicKey
    m0h12hPub.toString must be(
      "xpub6D4BDPcP2GT577Vvch3R8wDkScZWzQzMMUm3PWbmWvVJrZwQY4VUNgqFJPMM3No2dFDFGTsxxpG5uJh7n7epu4trkrX7x7DogT5Uv6fcLW5")

    val m0h12h2Path = BIP32Path.fromString("m/0'/1/2'/2")
    val m0h12h2 = masterPriv.deriveChildPrivKey(m0h12h2Path)
    m0h12h2.toStringSensitive must be(
      "xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8RfQMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334")

    val m0h12h2Pub = m0h12h2.extPublicKey
    m0h12h2Pub.toString must be(
      "xpub6FHa3pjLCk84BayeJxFW2SP4XRrFd1JYnxeLeU8EqN3vDfZmbqBqaGJAyiLjTAwm6ZLRQUMv1ZACTj37sR62cfN7fe5JnJ7dh8zL4fiyLHV")

    val m0h12h21000000000Path = BIP32Path.fromString("m/0'/1/2'/2/1000000000")
    val m0h12h21000000000 = masterPriv.deriveChildPrivKey(m0h12h21000000000Path)
    m0h12h21000000000.toStringSensitive must be(
      "xprvA41z7zogVVwxVSgdKUHDy1SKmdb533PjDz7J6N6mV6uS3ze1ai8FHa8kmHScGpWmj4WggLyQjgPie1rFSruoUihUZREPSL39UNdE3BBDu76")

    val m0h12h21000000000Pub = m0h12h21000000000.extPublicKey
    m0h12h21000000000Pub.toString must be(
      "xpub6H1LXWLaKsWFhvm6RVpEL9P4KfRZSW7abD2ttkWP3SSQvnyA8FSVqNTEcYFgJS2UaFcxupHiYkro49S8yGasTvXEYBVPamhGW6cFJodrTHy")
  }

  it must "pass test vector 2 in BIP32" in {
    val seedBytes =
      hex"fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542"

    val masterPriv =
      ExtPrivateKey(LegacyMainNetPriv, Some(seedBytes), BIP32Path.empty)
    masterPriv.toStringSensitive must be(
      "xprv9s21ZrQH143K31xYSDQpPDxsXRTUcvj2iNHm5NUtrGiGG5e2DtALGdso3pGz6ssrdK4PFmM8NSpSBHNqPqm55Qn3LqFtT2emdEXVYsCzC2U")

    val masterPub = masterPriv.extPublicKey
    masterPub.toString must be(
      "xpub661MyMwAqRbcFW31YEwpkMuc5THy2PSt5bDMsktWQcFF8syAmRUapSCGu8ED9W6oDMSgv6Zz8idoc4a6mr8BDzTJY47LJhkJ8UB7WEGuduB")

    val m0Path = BIP32Path.fromString("m/0")
    val m0 = masterPriv.deriveChildPrivKey(m0Path)
    m0.toStringSensitive must be(
      "xprv9vHkqa6EV4sPZHYqZznhT2NPtPCjKuDKGY38FBWLvgaDx45zo9WQRUT3dKYnjwih2yJD9mkrocEZXo1ex8G81dwSM1fwqWpWkeS3v86pgKt")

    val m0Pub = m0.extPublicKey
    m0Pub.toString must be(
      "xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH")

    val m02147483647hPath = BIP32Path.fromString("m/0/2147483647'")
    val m02147483647h =
      masterPriv.deriveChildPrivKey(m02147483647hPath)
    m02147483647h.toStringSensitive must be(
      "xprv9wSp6B7kry3Vj9m1zSnLvN3xH8RdsPP1Mh7fAaR7aRLcQMKTR2vidYEeEg2mUCTAwCd6vnxVrcjfy2kRgVsFawNzmjuHc2YmYRmagcEPdU9")

    val m02147483647hPub = m02147483647h.extPublicKey
    m02147483647hPub.toString must be(
      "xpub6ASAVgeehLbnwdqV6UKMHVzgqAG8Gr6riv3Fxxpj8ksbH9ebxaEyBLZ85ySDhKiLDBrQSARLq1uNRts8RuJiHjaDMBU4Zn9h8LZNnBC5y4a")

    val m02147483647h1Path = BIP32Path.fromString("m/0/2147483647'/1")
    val m02147483647h1 = masterPriv.deriveChildPrivKey(m02147483647h1Path)
    m02147483647h1.toStringSensitive must be(
      "xprv9zFnWC6h2cLgpmSA46vutJzBcfJ8yaJGg8cX1e5StJh45BBciYTRXSd25UEPVuesF9yog62tGAQtHjXajPPdbRCHuWS6T8XA2ECKADdw4Ef")

    val m02147483647h1Pub = m02147483647h1.extPublicKey
    m02147483647h1Pub.toString must be(
      "xpub6DF8uhdarytz3FWdA8TvFSvvAh8dP3283MY7p2V4SeE2wyWmG5mg5EwVvmdMVCQcoNJxGoWaU9DCWh89LojfZ537wTfunKau47EL2dhHKon")

    val m02147483647h12147483646hPath =
      BIP32Path.fromString("m/0/2147483647'/1/2147483646'")
    val m02147483647h12147483646h =
      masterPriv.deriveChildPrivKey(m02147483647h12147483646hPath)
    m02147483647h12147483646h.toStringSensitive must be(
      "xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc")

    val m02147483647h12147483646hPub = m02147483647h12147483646h.extPublicKey
    m02147483647h12147483646hPub.toString must be(
      "xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL")

    val m02147483647h12147483646h2Path =
      BIP32Path.fromString("m/0/2147483647'/1/2147483646'/2")
    val m02147483647h12147483646h2 =
      masterPriv.deriveChildPrivKey(m02147483647h12147483646h2Path)
    m02147483647h12147483646h2.toStringSensitive must be(
      "xprvA2nrNbFZABcdryreWet9Ea4LvTJcGsqrMzxHx98MMrotbir7yrKCEXw7nadnHM8Dq38EGfSh6dqA9QWTyefMLEcBYJUuekgW4BYPJcr9E7j")

    val m02147483647h12147483646h2Pub = m02147483647h12147483646h2.extPublicKey
    m02147483647h12147483646h2Pub.toString must be(
      "xpub6FnCn6nSzZAw5Tw7cgR9bi15UV96gLZhjDstkXXxvCLsUXBGXPdSnLFbdpq8p9HmGsApME5hQTZ3emM2rnY5agb9rXpVGyy3bdW6EEgAtqt")
  }

  it must "pass test vector 3 in BIP32" in {
    val seedBytes =
      hex"4b381541583be4423346c643850da4b320e46a87ae3d2a4e6da11eba819cd4acba45d239319ac14f863b8d5ab5a0d0c64d2e8a1e7d1457df2e5a3c51c73235be"

    val masterPrivKey =
      ExtPrivateKey(LegacyMainNetPriv, Some(seedBytes), BIP32Path.empty)
    masterPrivKey.toStringSensitive must be(
      "xprv9s21ZrQH143K25QhxbucbDDuQ4naNntJRi4KUfWT7xo4EKsHt2QJDu7KXp1A3u7Bi1j8ph3EGsZ9Xvz9dGuVrtHHs7pXeTzjuxBrCmmhgC6")

    val masterPubKey = masterPrivKey.extPublicKey
    masterPubKey.toString must be(
      "xpub661MyMwAqRbcEZVB4dScxMAdx6d4nFc9nvyvH3v4gJL378CSRZiYmhRoP7mBy6gSPSCYk6SzXPTf3ND1cZAceL7SfJ1Z3GC8vBgp2epUt13")

    val m0h = masterPrivKey.deriveChildPrivKey(BIP32Path.fromString("m/0'"))
    m0h.toStringSensitive must be(
      "xprv9uPDJpEQgRQfDcW7BkF7eTya6RPxXeJCqCJGHuCJ4GiRVLzkTXBAJMu2qaMWPrS7AANYqdq6vcBcBUdJCVVFceUvJFjaPdGZ2y9WACViL4L")

    val m0hPub = m0h.extPublicKey
    m0hPub.toString must be(
      "xpub68NZiKmJWnxxS6aaHmn81bvJeTESw724CRDs6HbuccFQN9Ku14VQrADWgqbhhTHBaohPX4CjNLf9fq9MYo6oDaPPLPxSb7gwQN3ih19Zm4Y")
  }

  it must "have derivation symmetry with (1<<31)-1, last i before hardened keys" in {
    //xprv9s21ZrQH143K4QWHDnxmxUbzAQYiDavkg14kQcmZjP2KaSB1PZs5BUsyNGSrWXTzZ9qwyJo5yzvDe3fWybykc8CQPDZMaKupTeVbkfG7osL
    //actual priv key 68e5ed2b2c8fc5a6605107d29d074e3d6ccb119c2811007e32f48305176f814c
    val str =
      "xprv9s21ZrQH143K4LCRq4tUZUt3fiTNZr6QTiep3HGzMxtSwfxKAhBmNJJnsmoyWuYZCPC4DNsiVwToHJbxZtq4iEkozBhMzWNTiCH4tzJNjPi"
    val masterPriv = ExtPrivateKey.fromString(str).get
    val idx = UInt32((1L << 31) - 1)
    val path1 = masterPriv.deriveChildPrivKey(idx).extPublicKey.key
    val path2 = masterPriv.extPublicKey.deriveChildPubKey(idx).get.key
    path1 must be(path2)
  }

  it must "not serialize a ExtPrivateKey to string" in {
    val seedBytes =
      hex"4b381541583be4423346c643850da4b320e46a87ae3d2a4e6da11eba819cd4acba45d239319ac14f863b8d5ab5a0d0c64d2e8a1e7d1457df2e5a3c51c73235be"

    val masterPriv =
      ExtPrivateKey(LegacyMainNetPriv, Some(seedBytes), BIP32Path.empty)
    masterPriv.toString must be(s"Masked(ExtPrivateKeyImpl)")
  }
}
