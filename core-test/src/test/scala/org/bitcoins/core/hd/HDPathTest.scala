package org.bitcoins.core.hd

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.crypto.{ExtKeyVersion, *}
import org.bitcoins.core.protocol.{Bech32Address, Bech32mAddress}
import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
  TaprootScriptPubKey
}
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey, XOnlyPubKey}
import org.bitcoins.testkitcore.gen.{HDGenerators, NumberGenerator}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.*

import scala.util.{Failure, Success}

class HDPathTest extends BitcoinSUnitTest {

  behavior of "HDAccount"

  it must "fail to make accounts with negative indices" in {
    forAll(HDGenerators.hdCoin, NumberGenerator.negativeInts) { (coin, i) =>
      assertThrows[IllegalArgumentException](HDAccount(coin = coin, index = i))
    }
  }

  it must "be convertable to a HDChain" in {
    forAll(HDGenerators.hdAccount, HDGenerators.hdChainType) {
      (account, chainType) =>
        val chain = account.toChain(chainType)
        assert(chain.account == account)
    }
  }

  behavior of "HDChain"

  it must "be convertable to an address" in {
    forAll(HDGenerators.hdChain, NumberGenerator.positiveInts) { (chain, i) =>
      val addr = chain.toHDAddress(i)
      assert(addr.chain == chain)
    }
  }

  behavior of "HDAddress"

  it must "fail to make addresses with neagtives indices" in {
    forAll(HDGenerators.hdChain, NumberGenerator.negativeInts) { (chain, i) =>
      assertThrows[IllegalArgumentException](
        HDAddress(chain = chain, index = i)
      )
    }
  }

  it must "be convertable to a HD path" in {
    forAll(HDGenerators.hdAddress) { addr =>
      val path = addr.toPath
      assert(path.address == addr)

    }
  }

  behavior of "HDChainType"

  it must "correctly represent external and change chains" in {
    HDChainType.fromInt(0) must be(HDChainType.External)
    HDChainType.fromInt(1) must be(HDChainType.Change)

    forAll(NumberGenerator.ints.suchThat(i => i != 1 && i != 0)) { i =>
      assertThrows[IllegalArgumentException](HDChainType.fromInt(i))
    }
  }

  behavior of "HDCoinType"

  it must "correctly represent Bitcoin and Testnet coins" in {
    HDCoinType.fromInt(0) must be(HDCoinType.Bitcoin)
    HDCoinType.fromInt(1) must be(HDCoinType.Testnet)
    forAll(
      NumberGenerator.ints.suchThat(i =>
        !HDCoinType.all.map(_.toInt).contains(i))
    ) { i =>
      HDCoinType.fromInt(i) must be(HDCoinType.UnknownCoinType(i))
      HDCoinType.fromKnown(i) must be(None)
    }
  }

  it must "be convertable to a HDAccount" in {
    forAll(HDGenerators.hdCoin, NumberGenerator.positiveInts) { (coin, index) =>
      val account = coin.toAccount(index)
      assert(account.coin == coin)
    }
  }

  behavior of "HDPath"

  it must "generate the next path" in {
    forAll(HDGenerators.hdPath) { path =>
      val next = path.next
      assert(next != path)
      // all elements except the last one should be the same
      assert(next.path.init == path.path.init)
      assert(next.address.index == path.address.index + 1)
    }
  }

  it must "have toString/fromString symmetry" in {
    forAll(HDGenerators.hdPath) { path =>
      val pathFromString = HDPath.fromStringOpt(path.toString)
      val resultOpt = pathFromString.map {
        case value: LegacyHDPath =>
          assert(value == path.asInstanceOf[LegacyHDPath])
        case value: SegWitHDPath =>
          assert(value == path.asInstanceOf[SegWitHDPath])
        case value: NestedSegWitHDPath =>
          assert(value == path.asInstanceOf[NestedSegWitHDPath])
        case h: HDPath =>
          sys.error(s"Unsupported HDPath=$h")
      }
      resultOpt.getOrElse(
        fail(s"$path did not have toString/fromString symmetry")
      )
    }
  }

  it must "fail to generate HD paths with an invalid length" in {
    forAll(HDGenerators.hdPathWithConstructor) { case (hd, hdApply) =>
      val tooShortPath = hd.path.dropRight(1)
      val attempt = hdApply(tooShortPath)
      attempt match {
        case Success(_) => fail()
        case Failure(exception) =>
          assert(exception.getMessage.contains("must have five elements"))
      }
    }
  }

  it must "fail to generate HD paths with the wrong hardened index types" in {
    forAll(HDGenerators.hdPathWithConstructor) { case (hd, hdApply) =>
      val nonHardenedCoinChildren = hd.path.zipWithIndex.map {
        case (child, index) =>
          if (index == LegacyHDPath.COIN_INDEX) child.copy(hardenedOpt = None)
          else child
      }

      val badCoinAttempt = hdApply(nonHardenedCoinChildren)

      badCoinAttempt match {
        case Success(_) => fail()
        case Failure(exc) =>
          assert(exc.getMessage.contains("coin type child must be hardened"))
      }

      val nonHardenedAccountChildren = hd.path.zipWithIndex.map {
        case (child, index) =>
          if (index == LegacyHDPath.ACCOUNT_INDEX)
            child.copy(hardenedOpt = None)
          else child
      }
      val badAccountAttempt = hdApply(nonHardenedAccountChildren)

      badAccountAttempt match {
        case Success(_) => fail()
        case Failure(exc) =>
          assert(exc.getMessage.contains("account child must be hardened"))
      }

      val hardenedChainChildren = hd.path.zipWithIndex.map {
        case (child, index) =>
          if (index == LegacyHDPath.CHAIN_INDEX)
            child.copy(hardenedOpt = HardenedType.defaultOpt)
          else child
      }
      val badChainAttempt =
        hdApply(hardenedChainChildren)

      badChainAttempt match {
        case Success(_) => fail()
        case Failure(exc) =>
          assert(exc.getMessage.contains("chain child must not be hardened"))
      }

      val hardenedAddressChildren = hd.path.zipWithIndex.map {
        case (child, index) =>
          if (index == LegacyHDPath.ADDRESS_INDEX)
            child.copy(hardenedOpt = HardenedType.defaultOpt)
          else child
      }
      val badAddrAttempt =
        hdApply(hardenedAddressChildren)

      badAddrAttempt match {
        case Success(_) => fail()
        case Failure(exc) =>
          assert(
            exc.getMessage.contains("address index child must not be hardened")
          )
      }
    }
  }

  // https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#examples
  it must "correctly parse the examples from BIP44" in {

    val firstString = " m / 44' / 0' / 0' / 0 / 0 "
    val first = LegacyHDPath.fromString(firstString)
    assert(first.purpose == HDPurpose.Legacy)
    assert(first.coin.coinType == HDCoinType.Bitcoin)
    assert(first.account.index == 0)
    assert(first.chain.chainType == HDChainType.External)
    assert(first.address.index == 0)
    assert(HDPath.fromStringOpt(firstString).contains(first))

    val secondString = " m / 44' / 0' / 0' / 0 / 1 "
    val second = LegacyHDPath.fromString(secondString)
    assert(second.purpose == HDPurpose.Legacy)
    assert(second.coin.coinType == HDCoinType.Bitcoin)
    assert(second.account.index == 0)
    assert(second.chain.chainType == HDChainType.External)
    assert(second.address.index == 1)
    assert(HDPath.fromStringOpt(secondString).contains(second))

    val thirdString = " m / 44' / 0' / 0' / 1 / 0 "
    val third = LegacyHDPath.fromString(thirdString)
    assert(third.purpose == HDPurpose.Legacy)
    assert(third.coin.coinType == HDCoinType.Bitcoin)
    assert(third.account.index == 0)
    assert(third.chain.chainType == HDChainType.Change)
    assert(third.address.index == 0)
    assert(HDPath.fromStringOpt(thirdString).contains(third))

    val fourthString = " m / 44' / 0' / 0' / 1 / 1 "
    val fourth = LegacyHDPath.fromString(fourthString)
    assert(fourth.purpose == HDPurpose.Legacy)
    assert(fourth.coin.coinType == HDCoinType.Bitcoin)
    assert(fourth.account.index == 0)
    assert(fourth.chain.chainType == HDChainType.Change)
    assert(fourth.address.index == 1)
    assert(HDPath.fromStringOpt(fourthString).contains(fourth))

    val fifthString = " m / 44' / 0' / 1' / 0 / 0 "
    val fifth = LegacyHDPath.fromString(fifthString)
    assert(fifth.purpose == HDPurpose.Legacy)
    assert(fifth.coin.coinType == HDCoinType.Bitcoin)
    assert(fifth.account.index == 1)
    assert(fifth.chain.chainType == HDChainType.External)
    assert(fifth.address.index == 0)
    assert(HDPath.fromStringOpt(fifthString).contains(fifth))

    val sixthString = " m / 44' / 0' / 1' / 0 / 1 "
    val sixth = LegacyHDPath.fromString(sixthString)
    assert(sixth.purpose == HDPurpose.Legacy)
    assert(sixth.coin.coinType == HDCoinType.Bitcoin)
    assert(sixth.account.index == 1)
    assert(sixth.chain.chainType == HDChainType.External)
    assert(sixth.address.index == 1)
    assert(HDPath.fromStringOpt(sixthString).contains(sixth))

    val seventhString = " m / 44' / 0' / 1' / 1 / 0 "
    val seventh = LegacyHDPath.fromString(seventhString)
    assert(seventh.purpose == HDPurpose.Legacy)
    assert(seventh.coin.coinType == HDCoinType.Bitcoin)
    assert(seventh.account.index == 1)
    assert(seventh.chain.chainType == HDChainType.Change)
    assert(seventh.address.index == 0)
    assert(HDPath.fromStringOpt(seventhString).contains(seventh))

    val eightString = " m / 44' / 0' / 1' / 1 / 1 "
    val eigth = LegacyHDPath.fromString(eightString)
    assert(eigth.purpose == HDPurpose.Legacy)
    assert(eigth.coin.coinType == HDCoinType.Bitcoin)
    assert(eigth.account.index == 1)
    assert(eigth.chain.chainType == HDChainType.Change)
    assert(eigth.address.index == 1)
    assert(HDPath.fromStringOpt(eightString).contains(eigth))

    val ninthString = " m / 44' / 1' / 0' / 0 / 1 "
    val ninth = LegacyHDPath.fromString(ninthString)
    assert(ninth.purpose == HDPurpose.Legacy)
    assert(ninth.coin.coinType == HDCoinType.Testnet)
    assert(ninth.account.index == 0)
    assert(ninth.chain.chainType == HDChainType.External)
    assert(ninth.address.index == 1)
    assert(HDPath.fromStringOpt(ninthString).contains(ninth))

    val tenthString = " m / 44' / 1' / 0' / 0 / 1 "
    val tenth = LegacyHDPath.fromString(tenthString)
    assert(tenth.purpose == HDPurpose.Legacy)
    assert(tenth.coin.coinType == HDCoinType.Testnet)
    assert(tenth.account.index == 0)
    assert(tenth.chain.chainType == HDChainType.External)
    assert(tenth.address.index == 1)
    assert(HDPath.fromStringOpt(tenthString).contains(tenth))

    val eleventhString = " m / 44' / 1' / 0' / 1 / 0 "
    val eleventh = LegacyHDPath.fromString(eleventhString)
    assert(eleventh.purpose == HDPurpose.Legacy)
    assert(eleventh.coin.coinType == HDCoinType.Testnet)
    assert(eleventh.account.index == 0)
    assert(eleventh.chain.chainType == HDChainType.Change)
    assert(eleventh.address.index == 0)
    assert(HDPath.fromStringOpt(eleventhString).contains(eleventh))

    val twelfthString = " m / 44' / 1' / 0' / 1 / 1 "
    val twelfth = LegacyHDPath.fromString(twelfthString)
    assert(twelfth.purpose == HDPurpose.Legacy)
    assert(twelfth.coin.coinType == HDCoinType.Testnet)
    assert(twelfth.account.index == 0)
    assert(twelfth.chain.chainType == HDChainType.Change)
    assert(twelfth.address.index == 1)
    assert(HDPath.fromStringOpt(twelfthString).contains(twelfth))

    val thirteenthString = " m / 44' / 1' / 1' / 0 / 0 "
    val thirteenth = LegacyHDPath.fromString(thirteenthString)
    assert(thirteenth.purpose == HDPurpose.Legacy)
    assert(thirteenth.coin.coinType == HDCoinType.Testnet)
    assert(thirteenth.account.index == 1)
    assert(thirteenth.chain.chainType == HDChainType.External)
    assert(thirteenth.address.index == 0)
    assert(HDPath.fromStringOpt(thirteenthString).contains(thirteenth))

    val fourteenthString = " m / 44' / 1' / 1' / 0 / 1 "
    val fourteenth = LegacyHDPath.fromString(fourteenthString)
    assert(fourteenth.purpose == HDPurpose.Legacy)
    assert(fourteenth.coin.coinType == HDCoinType.Testnet)
    assert(fourteenth.account.index == 1)
    assert(fourteenth.chain.chainType == HDChainType.External)
    assert(fourteenth.address.index == 1)
    assert(HDPath.fromStringOpt(fourteenthString).contains(fourteenth))

    val fifteenthString = " m / 44' / 1' / 1' / 1 / 0 "
    val fifteenth = LegacyHDPath.fromString(fifteenthString)
    assert(fifteenth.purpose == HDPurpose.Legacy)
    assert(fifteenth.coin.coinType == HDCoinType.Testnet)
    assert(fifteenth.account.index == 1)
    assert(fifteenth.chain.chainType == HDChainType.Change)
    assert(fifteenth.address.index == 0)
    assert(HDPath.fromStringOpt(fifteenthString).contains(fifteenth))

    val sixteenthString = " m / 44' / 1' / 1' / 1 / 1 "
    val sixteenth = LegacyHDPath.fromString(sixteenthString)
    assert(sixteenth.purpose == HDPurpose.Legacy)
    assert(sixteenth.coin.coinType == HDCoinType.Testnet)
    assert(sixteenth.account.index == 1)
    assert(sixteenth.chain.chainType == HDChainType.Change)
    assert(sixteenth.address.index == 1)
    assert(HDPath.fromStringOpt(sixteenthString).contains(sixteenth))

  }

  // https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki#test-vectors
  it must "correctly parse the example from BIP84" in {
    val words = Vector(
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "about"
    )
    val mnemonic = MnemonicCode.fromWords(words)
    val seed = BIP39Seed.fromMnemonic(mnemonic)
    val xpriv = seed.toExtPrivateKey(ExtKeyVersion.SegWitMainNetPriv)
    val xpub = xpriv.extPublicKey

    assert(ExtPrivateKey.fromStringT("zprvAWgYBB").isFailure)
    val expectedXpriv = ExtPrivateKey.fromString(
      "zprvAWgYBBk7JR8Gjrh4UJQ2uJdG1r3WNRRfURiABBE3RvMXYSrRJL62XuezvGdPvG6GFBZduosCc1YP5wixPox7zhZLfiUm8aunE96BBa4Kei5"
    )
    val expectedXpub = ExtPublicKey.fromString(
      "zpub6jftahH18ngZxLmXaKw3GSZzZsszmt9WqedkyZdezFtWRFBZqsQH5hyUmb4pCEeZGmVfQuP5bedXTB8is6fTv19U1GQRyQUKQGUTzyHACMF"
    )

    assert(xpriv == expectedXpriv)
    assert(xpub == expectedXpub)

    {
      val firstAddrPath = SegWitHDPath.fromString("m/84'/0'/0'/0/0")
      val derivedXpriv = xpriv.deriveChildPrivKey(firstAddrPath)
      val derivedPriv = derivedXpriv.key
      val derivedPub = derivedPriv.publicKey
      val spk = P2WPKHWitnessSPKV0(derivedPub)
      val address = Bech32Address(spk, MainNet)

      val expectedPriv = ECPrivateKeyUtil
        .fromWIFToPrivateKey(
          "KyZpNDKnfs94vbrwhJneDi77V6jF64PWPF8x5cdJb8ifgg2DUc9d"
        )
        .toPrivateKey
      val expectedPub = ECPublicKey(
        hex"0330d54fd0dd420a6e5f8d3624f5f3482cae350f79d5f0753bf5beef9c2d91af3c"
      )
      val expectedAddress =
        Bech32Address.fromString("bc1qcr8te4kr609gcawutmrza0j4xv80jy8z306fyu")

      assert(expectedPriv == derivedPriv)
      assert(expectedPub == derivedPub)
      assert(expectedAddress == address)
    }

    {
      val secondAddrPath = SegWitHDPath.fromString("m/84'/0'/0'/0/1")
      val derivedXpriv = xpriv.deriveChildPrivKey(secondAddrPath)
      val derivedPriv = derivedXpriv.key
      val derivedPub = derivedPriv.publicKey
      val spk = P2WPKHWitnessSPKV0(derivedPub)
      val address = Bech32Address(spk, MainNet)

      val expectedPriv = ECPrivateKeyUtil
        .fromWIFToPrivateKey(
          "Kxpf5b8p3qX56DKEe5NqWbNUP9MnqoRFzZwHRtsFqhzuvUJsYZCy"
        )
        .toPrivateKey
      val expectedPub = ECPublicKey(
        hex"03e775fd51f0dfb8cd865d9ff1cca2a158cf651fe997fdc9fee9c1d3b5e995ea77"
      )
      val expectedAddress =
        Bech32Address.fromString("bc1qnjg0jd8228aq7egyzacy8cys3knf9xvrerkf9g")

      assert(expectedPriv == derivedPriv)
      assert(expectedPub == derivedPub)
      assert(expectedAddress == address)
    }

    {
      val firstChangePath = SegWitHDPath.fromString("m/84'/0'/0'/1/0")
      val derivedXpriv = xpriv.deriveChildPrivKey(firstChangePath)
      val derivedPriv = derivedXpriv.key
      val derivedPub = derivedPriv.publicKey
      val spk = P2WPKHWitnessSPKV0(derivedPub)
      val address = Bech32Address(spk, MainNet)

      val expectedPriv = ECPrivateKeyUtil
        .fromWIFToPrivateKey(
          "KxuoxufJL5csa1Wieb2kp29VNdn92Us8CoaUG3aGtPtcF3AzeXvF"
        )
        .toPrivateKey
      val expectedPub = ECPublicKey(
        hex"03025324888e429ab8e3dbaf1f7802648b9cd01e9b418485c5fa4c1b9b5700e1a6"
      )
      val expectedAddress =
        Bech32Address.fromString("bc1q8c6fshw2dlwun7ekn9qwf37cu2rn755upcp6el")

      assert(expectedPriv == derivedPriv)
      assert(expectedPub == derivedPub)
      assert(expectedAddress == address)
    }

  }

  // to reproduce the data used in this test:
  // 1) Run a Trezor T emulator:
  //   1) git clone https://github.com/trezor/trezor-firmware
  //   2) git checkout release/2019-5
  //   3) cd core
  //   4) make vendor
  //   5) sudo apt-get install scons libsdl2-dev libsdl2-image-dev
  //   6) make build_unix
  //   7) ./emu.sh
  // 2) Download udev rules needed for Trezor bridge:
  //   1) wget https://raw.githubusercontent.com/trezor/trezor-common/master/udev/51-trezor.rules /etc/udev/rules.d/
  // 3) Run the Trezor bridge locally
  //   1) git clone https://github.com/trezor/trezord-go
  //   2) go get github.com/trezor/trezord-go
  //   3) go build github.com/trezor/trezord-go
  //   4) ./trezord-go -e 21324
  // 4) Install the trezorctl CLI tool: https://wiki.trezor.io/Using_trezorctl_commands_with_Trezor#Install_python-trezor
  // 5) Go to https://trezor.io/start
  // 6) Restore wallet from seed below
  // 7) Invoke the commands found in core-test/src/test/resources/trezor.txt
  //    and see that the actual output matches the written output
  it must "behave the same way Trezor does with a hard coded mnemonic" in {
    val words = Vector(
      "error",
      "method",
      "sample",
      "almost",
      "peanut",
      "verify",
      "merge",
      "rapid",
      "airport",
      "bundle",
      "execute",
      "fork"
    )

    val mnemonic = MnemonicCode.fromWords(words)
    val seed = BIP39Seed.fromMnemonic(mnemonic)

    // legacy mainnet
    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.LegacyMainNetPriv, seed)
      val path = LegacyHDPath.fromString("m/44'/0'/0'/0/0")
      val xpub = rootXpriv.deriveChildPubKey(path).get
      val expectedXpub = ExtPublicKey.fromString(
        "xpub6FR8LqriB4qyPvdtZwhHW2HQP4daR2qXsYAAsfaiF8DoFJJ5AqGCpiGM3kFC4Z9AZWnReXrzp2nzhp91myPjz96e3wrJoMvgnCyMBjKz8vJ"
      )
      assert(xpub == expectedXpub)
    }

    // legacy testnet
    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.LegacyTestNet3Priv, seed)
      val path = LegacyHDPath.fromString("m/44'/0'/0'/0/0")
      val xpub = rootXpriv.deriveChildPubKey(path).get
      val expectedXpub = ExtPublicKey.fromString(
        "tpubDFnks5gPtLoRfipk28gNhwcmiBjEQRLbRAkUEDdrb2ygzaxnF47Hy9wBHnKyb46QMRKLG7NsM8d3PzddAqEysaYw7YbcUtavNAZkwjM7aqi"
      )
      assert(xpub == expectedXpub)
    }

    // nested segwit mainnet
    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.NestedSegWitMainNetPriv, seed)
      val path = NestedSegWitHDPath.fromString("m/49'/0'/0'/0/0")
      val xpub = rootXpriv.deriveChildPubKey(path).get
      val expectedXpub = ExtPublicKey.fromString(
        "ypub6c6461WnUp9LoRskZCU3bHBJahDvtrPwCCo1WEtuhsrFGZ8Mn2YMNGab2tj5eujgsMx5U1BZz7hA1q87ZdVSXZdArxM9G5Y9iZchQFrov4q"
      )
      assert(xpub == expectedXpub)
    }

    // nested segwit testnet
    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(
          ExtKeyVersion.NestedSegWitTestNet3Priv,
          seed
        )
      val path = NestedSegWitHDPath.fromString("m/49'/0'/0'/0/0")
      val xpub = rootXpriv.deriveChildPubKey(path).get
      val expectedXpub = ExtPublicKey.fromString(
        "upub5JkzsLq7t5yRQF7HDmKYkvoHtpe98NRwXki8NfKNBrLj49sSmPt6t1x2x4tjfH81EoUrU6oL9UGxUgfrgqqPLctmPbZSvSGCdfN7qyMHU7g"
      )
      assert(xpub == expectedXpub)
    }

    // native segwit mainnet
    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.SegWitMainNetPriv, seed)
      val path = SegWitHDPath.fromString("m/84'/0'/0'/0/0")
      val xpub = rootXpriv.deriveChildPubKey(path).get
      val expectedXpub = ExtPublicKey.fromString(
        "zpub6vibtacmZKTajuFATBMJPq629qFzaonkAHzWDBEgpHnuhDBozTVWxbF4zJ1Hm4tdkAMJTg9kUqizEz4JQXGkxyotn3MCxbT92mJ8XVcNN5E"
      )
      assert(xpub == expectedXpub)
    }

    // native segwit testnet
    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.SegWitTestNet3Priv, seed)
      val path = SegWitHDPath.fromString("m/84'/0'/0'/0/0")
      val xpub = rootXpriv.deriveChildPubKey(path).get
      val expectedXpub = ExtPublicKey.fromString(
        "vpub5dPYfuw6xbHfLiUh7kCoZUi1TxgCpKpkVqud5bf9JGHPUovtypqGULcWuUAwmSGx7bt5TmmWeCJnhqc3Xjchn35VJgZWcxBBws3Yy6zYa7G"
      )
      assert(xpub == expectedXpub)
    }

    // multisig mainnet
    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.LegacyMainNetPriv, seed)
      val path = MultisigHDPath.fromString("m/45'/0'/0'/0/0")
      val xpub = rootXpriv.deriveChildPubKey(path).get
      val expectedXpub = ExtPublicKey.fromString(
        "xpub6GqDvL47MZ2baAovXNzG6UuZa7LR37JDG9Qt6nbns4L1q4owu8wnvkiZgTkYgbeyW6EmMjqe5B7TFKb8JvueU9T73pTW4RWf7gEoXFCqMKv"
      )
      assert(xpub == expectedXpub)
    }

    // multisig testnet
    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.LegacyTestNet3Priv, seed)
      val path = MultisigHDPath.fromString("m/45'/0'/0'/0/0")
      val xpub = rootXpriv.deriveChildPubKey(path).get
      val expectedXpub = ExtPublicKey.fromString(
        "tpubDHCrSZso4pz3qxzmyZyMJQEvuES52VoGon1BTLewCy5uaMUeyMnt5CPPvVqLD6cDHzmfyKMWcGwVwW5jhnktMauQ7RCojxAthdqDHJNNVUx"
      )
      assert(xpub == expectedXpub)
    }
  }

  // https://github.com/bitcoin/bips/blob/master/bip-0049.mediawiki#test-vectors
  // Either I'm doing something wrong, or there's multiple versions of BIP49 being
  // implemented. Posted a SO question about this:
  // https://bitcoin.stackexchange.com/questions/87396/whats-the-magic-key-version-bytes-for-bip49
  it must "correctly parse the example from BIP49" in {
    val words = Vector(
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "about"
    )

    val mnemonic = MnemonicCode.fromWords(words)
    val seed = BIP39Seed.fromMnemonic(mnemonic)

    // bip49 does not include custom key version bytes for xpubs
    // other wallets that implement have agreed that this is stupid,
    // and disregard this. we use the same values as are used in other
    // wallets, most notably trezor and samourai. in this test however,
    // we pass in the wrong version bytes to make the test vector
    // from bip49 pass
    val rootXpriv = seed.toExtPrivateKey(ExtKeyVersion.LegacyTestNet3Priv)
    val expectedRootXpriv = ExtPrivateKey.fromString(
      "tprv8ZgxMBicQKsPe5YMU9gHen4Ez3ApihUfykaqUorj9t6FDqy3nP6eoXiAo2ssvpAjoLroQxHqr3R5nE3a5dU3DHTjTgJDd7zrbniJr6nrCzd"
    )

    val path = NestedSegWitHDPath.fromString("m/49'/1'/0'/0/0")

    val firstAccount = path.account
    val accountXpriv = rootXpriv.deriveChildPrivKey(firstAccount)
    val expectedAccountXpriv = ExtPrivateKey.fromString(
      "tprv8gRrNu65W2Msef2BdBSUgFdRTGzC8EwVXnV7UGS3faeXtuMVtGfEdidVeGbThs4ELEoayCAzZQ4uUji9DUiAs7erdVskqju7hrBcDvDsdbY"
    )
    assert(expectedAccountXpriv == accountXpriv)

    val privkeyAtPath = rootXpriv.deriveChildPrivKey(path).key
    val expectedPrivkeyAtPath = ECPrivateKey(
      hex"0xc9bdb49cfbaedca21c4b1f3a7803c34636b1d7dc55a717132443fc3f4c5867e8"
    )
    val expectedPubkeyAtPath = ECPublicKey(
      hex"0x03a1af804ac108a8a51782198c2d034b28bf90c8803f5a53f76276fa69a4eae77f"
    )
    assert(expectedPrivkeyAtPath.publicKey == expectedPubkeyAtPath)
    assert(privkeyAtPath == expectedPrivkeyAtPath)
    assert(privkeyAtPath.publicKey == expectedPubkeyAtPath)

    assert(rootXpriv == expectedRootXpriv)

    /*
     * Yet to implement from the BIP:
     * // Address derivation
     * keyhash = HASH160(account0recvPublickKeyHex) = 0x38971f73930f6c141d977ac4fd4a727c854935b3
     * scriptSig = <0 <keyhash>> = 0x001438971f73930f6c141d977ac4fd4a727c854935b3
     * addressBytes = HASH160(scriptSig) = 0x336caa13e08b96080a32b5d818d59b4ab3b36742
     *
     * // addressBytes base58check encoded for testnet
     * address = base58check(prefix | addressBytes) = 2Mww8dCYPUpKHofjgcXcBCEGmniw9CoaiD2 (testnet)
     */

  }

  // https://github.com/satoshilabs/slips/blob/master/slip-0132.md#bitcoin-test-vectors
  it must "pass the test vector from SLIP132" in {
    val words = Vector(
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "about"
    )
    val mnemonic = MnemonicCode.fromWords(words)
    val seed = BIP39Seed.fromMnemonic(mnemonic)

    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.LegacyMainNetPriv, seed)
      val legacyPathString = "m/44'/0'/0'/0/0"
      val legacyPath = LegacyHDPath.fromString(legacyPathString)
      val legacyPathAccount = legacyPath.account
      val accountXpriv = rootXpriv.deriveChildPrivKey(legacyPathAccount)
      val accountXpub = accountXpriv.extPublicKey

      val expectedAccountXpriv = ExtPrivateKey.fromString(
        "xprv9xpXFhFpqdQK3TmytPBqXtGSwS3DLjojFhTGht8gwAAii8py5X6pxeBnQ6ehJiyJ6nDjWGJfZ95WxByFXVkDxHXrqu53WCRGypk2ttuqncb"
      )
      val expectedAccountXpub = ExtPublicKey.fromString(
        "xpub6BosfCnifzxcFwrSzQiqu2DBVTshkCXacvNsWGYJVVhhawA7d4R5WSWGFNbi8Aw6ZRc1brxMyWMzG3DSSSSoekkudhUd9yLb6qx39T9nMdj"
      )

      assert(expectedAccountXpriv == accountXpriv)
      assert(expectedAccountXpub == accountXpub)
    }

    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.NestedSegWitMainNetPriv, seed)
      val nestedSegwitPathString = "m/49'/0'/0'/0/0"
      val nestedSegwitPath =
        NestedSegWitHDPath.fromString(nestedSegwitPathString)
      val nestedSegwithPathAccount = nestedSegwitPath.account
      val accountXpriv = rootXpriv.deriveChildPrivKey(nestedSegwithPathAccount)
      val accountXpub = accountXpriv.extPublicKey

      val expectedAccountXpriv = ExtPrivateKey.fromString(
        "yprvAHwhK6RbpuS3dgCYHM5jc2ZvEKd7Bi61u9FVhYMpgMSuZS613T1xxQeKTffhrHY79hZ5PsskBjcc6C2V7DrnsMsNaGDaWev3GLRQRgV7hxF"
      )
      val expectedAccountXpub = ExtPublicKey.fromString(
        "ypub6Ww3ibxVfGzLrAH1PNcjyAWenMTbbAosGNB6VvmSEgytSER9azLDWCxoJwW7Ke7icmizBMXrzBx9979FfaHxHcrArf3zbeJJJUZPf663zsP"
      )

      assert(expectedAccountXpriv == accountXpriv)
      assert(expectedAccountXpub == accountXpub)
    }

    {
      val rootXpriv =
        ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.SegWitMainNetPriv, seed)
      val segwitPathString = "m/84'/0'/0'/0/0"
      val segwitPath = SegWitHDPath.fromString(segwitPathString)
      val segwithPathAccount = segwitPath.account
      val accountXpriv = rootXpriv.deriveChildPrivKey(segwithPathAccount)
      val accountXpub = accountXpriv.extPublicKey

      val expectedAccountXpriv = ExtPrivateKey.fromString(
        "zprvAdG4iTXWBoARxkkzNpNh8r6Qag3irQB8PzEMkAFeTRXxHpbF9z4QgEvBRmfvqWvGp42t42nvgGpNgYSJA9iefm1yYNZKEm7z6qUWCroSQnE"
      )
      val expectedAccountXpub = ExtPublicKey.fromString(
        "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
      )

      assert(expectedAccountXpriv == accountXpriv)
      assert(expectedAccountXpub == accountXpub)

    }
  }

  it must "pass examples from BIP86" in {
    val words = Vector(
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "abandon",
      "about"
    )

    val mnemonic = MnemonicCode.fromWords(words)
    val seed = BIP39Seed.fromMnemonic(mnemonic)
    val rootXpriv =
      ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.LegacyMainNetPriv, seed)

    {
      val taprootPathString = "m/86'/0'/0'/0/0"
      val taprootPath = TaprootHDPath.fromString(taprootPathString)
      val taprootPathAccount = taprootPath.account
      val accountXpriv = rootXpriv.deriveChildPrivKey(taprootPathAccount)
      val accountXpub = accountXpriv.extPublicKey

      val expectedAccountXpriv = ExtPrivateKey.fromString(
        "xprv9xgqHN7yz9MwCkxsBPN5qetuNdQSUttZNKw1dcYTV4mkaAFiBVGQziHs3NRSWMkCzvgjEe3n9xV8oYywvM8at9yRqyaZVz6TYYhX98VjsUk"
      )
      val expectedAccountXpub = ExtPublicKey.fromString(
        "xpub6BgBgsespWvERF3LHQu6CnqdvfEvtMcQjYrcRzx53QJjSxarj2afYWcLteoGVky7D3UKDP9QyrLprQ3VCECoY49yfdDEHGCtMMj92pReUsQ"
      )

      assert(expectedAccountXpriv == accountXpriv)
      assert(expectedAccountXpub == accountXpub)

      val first = rootXpriv.deriveChildPrivKey(taprootPath)
      assert(first == ExtPrivateKey.fromString(
        "xprvA449goEeU9okwCzzZaxiy475EQGQzBkc65su82nXEvcwzfSskb2hAt2WymrjyRL6kpbVTGL3cKtp9herYXSjjQ1j4stsXXiRF7kXkCacK3T"))
      val firstXPub = first.extPublicKey
      assert(firstXPub == ExtPublicKey.fromString(
        "xpub6H3W6JmYJXN49h5TfcVjLC3onS6uPeUTTJoVvRC8oG9vsTn2J8LwigLzq5tHbrwAzH9DGo6ThGUdWsqce8dGfwHVBxSbixjDADGGdzF7t2B"))
      val firstInternalKey = first.publicKey
      assert(
        firstInternalKey.toXOnly == XOnlyPubKey.fromHex(
          "cc8a4bc64d897bddc5fbc2f670f7a8ba0b386779106cf1223c6fc5d7cd6fc115"))
      val firstOutputKey =
        TaprootScriptPubKey.fromInternalKey(firstInternalKey.toXOnly)
      assert(
        firstOutputKey.pubKey == XOnlyPubKey.fromHex(
          "a60869f0dbcf1dc659c9cecbaf8050135ea9e8cdc487053f1dc6880949dc684c"))
      val firstSPK = TaprootScriptPubKey(firstOutputKey.pubKey)
      assert(firstSPK == TaprootScriptPubKey.fromAsmHex(
        "5120a60869f0dbcf1dc659c9cecbaf8050135ea9e8cdc487053f1dc6880949dc684c"))
      val firstAddress = Bech32mAddress(firstSPK, MainNet)
      assert(
        firstAddress == Bech32mAddress.fromString(
          "bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr"))
    }
    {
      val secondReceivingPath = TaprootHDPath.fromString("m/86'/0'/0'/0/1")
      val second = rootXpriv.deriveChildPrivKey(secondReceivingPath)
      assert(second == ExtPrivateKey.fromString(
        "xprvA449goEeU9okyiF1LmKiDaTgeXvmh87DVyRd35VPbsSop8n8uALpbtrUhUXByPFKK7C2yuqrB1FrhiDkEMC4RGmA5KTwsE1aB5jRu9zHsuQ"))
      val secondXPub = second.extPublicKey
      assert(secondXPub == ExtPublicKey.fromString(
        "xpub6H3W6JmYJXN4CCKUSnriaiQRCZmG6aq4sCMDqTu1ACyngw7HShf59hAxYjXgKDuuHThVEUzdHrc3aXCr9kfvQvZPit5dnD3K9xVRBzjK3rX"))
      val secondInternalKey = second.publicKey
      assert(
        secondInternalKey.toXOnly == XOnlyPubKey.fromHex(
          "83dfe85a3151d2517290da461fe2815591ef69f2b18a2ce63f01697a8b313145"))
      val secondOutputKey =
        TaprootScriptPubKey.fromInternalKey(secondInternalKey.toXOnly)
      assert(
        secondOutputKey.pubKey == XOnlyPubKey.fromHex(
          "a82f29944d65b86ae6b5e5cc75e294ead6c59391a1edc5e016e3498c67fc7bbb"))
      val secondSPK = TaprootScriptPubKey(secondOutputKey.pubKey)
      assert(secondSPK == TaprootScriptPubKey.fromAsmHex(
        "5120a82f29944d65b86ae6b5e5cc75e294ead6c59391a1edc5e016e3498c67fc7bbb"))
      val secondAddress = Bech32mAddress(secondSPK, MainNet)
      assert(
        secondAddress == Bech32mAddress.fromString(
          "bc1p4qhjn9zdvkux4e44uhx8tc55attvtyu358kutcqkudyccelu0was9fqzwh"))

    }

    {
      val changePath = TaprootHDPath.fromString("m/86'/0'/0'/1/0")
      val changeRootKey = rootXpriv.deriveChildPrivKey(changePath)
      assert(changeRootKey == ExtPrivateKey.fromString(
        "xprvA3Ln3Gt3aphvUgzgEDT8vE2cYqb4PjFfpmbiFKphxLg1FjXQpkAk5M1ZKDY15bmCAHA35jTiawbFuwGtbDZogKF1WfjwxML4gK7WfYW5JRP"))
      val changeXPub = changeRootKey.extPublicKey
      assert(changeXPub == ExtPublicKey.fromString(
        "xpub6GL8SnQwRCGDhB59LEz9HMyM6sRYoByXBzXK3iEKWgCz8XrZNHUzd9L3AUBELW5NzA7dEFvMas1F84TuPH3xqdUA5tumaGWFgihJzWytXe3"))
      val changeInternalKey = changeRootKey.publicKey
      assert(
        changeInternalKey.toXOnly == XOnlyPubKey.fromHex(
          "399f1b2f4393f29a18c937859c5dd8a77350103157eb880f02e8c08214277cef"))
      val changeOutputKey =
        TaprootScriptPubKey.fromInternalKey(changeInternalKey.toXOnly)
      assert(
        changeOutputKey.pubKey == XOnlyPubKey.fromHex(
          "882d74e5d0572d5a816cef0041a96b6c1de832f6f9676d9605c44d5e9a97d3dc "))
      val changeSPK = TaprootScriptPubKey(changeOutputKey.pubKey)
      assert(changeSPK == TaprootScriptPubKey.fromAsmHex(
        "5120882d74e5d0572d5a816cef0041a96b6c1de832f6f9676d9605c44d5e9a97d3dc"))
      val changeAddress = Bech32mAddress(changeSPK, MainNet)
      assert(
        changeAddress == Bech32mAddress.fromString(
          "bc1p3qkhfews2uk44qtvauqyr2ttdsw7svhkl9nkm9s9c3x4ax5h60wqwruhk7"))

    }
  }

}
