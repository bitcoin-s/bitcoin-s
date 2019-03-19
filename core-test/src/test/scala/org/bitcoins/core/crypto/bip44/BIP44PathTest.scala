package org.bitcoins.core.crypto.bip44

import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkit.util.BitcoinSUnitTest

import scala.util.{Failure, Success, Try}

class BIP44PathTest extends BitcoinSUnitTest {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "BIP44Account"

  it must "fail to make accounts with negative indices" in {
    forAll(CryptoGenerators.bip44Coin, NumberGenerator.negativeInts) {
      (coin, i) =>
        assertThrows[IllegalArgumentException](
          BIP44Account(coin = coin, index = i))
    }
  }

  it must "be convertable to a BIP44Chain" in {
    forAll(CryptoGenerators.bip44Account, CryptoGenerators.bip44ChainType) {
      (account, chainType) =>
        val chain = account.toChain(chainType)
        assert(chain.account == account)
    }
  }

  behavior of "BIP44Address"

  it must "fail to make addresses with neagtives indices" in {
    forAll(CryptoGenerators.bip44Chain, NumberGenerator.negativeInts) {
      (chain, i) =>
        assertThrows[IllegalArgumentException](
          BIP44Address(chain = chain, index = i))
    }
  }

  it must "be convertable to a BIP44Path" in {
    forAll(CryptoGenerators.bip44Address) { addr =>
      val path = addr.toPath
      assert(path.address == addr)

    }
  }

  behavior of "BIP44ChainType"

  it must "correctly represent external and change chains" in {
    BIP44ChainType.fromInt(0) must be(BIP44ChainType.External)
    BIP44ChainType.fromInt(1) must be(BIP44ChainType.Change)

    forAll(NumberGenerator.ints.suchThat(i => i != 1 && i != 0)) { i =>
      assertThrows[IllegalArgumentException](BIP44ChainType.fromInt(i))
    }
  }

  behavior of "BIP44Coin"

  it must "correctly represent Bitcoin and Testnet coins" in {
    BIP44Coin.fromInt(0) must be(BIP44Coin.Bitcoin)
    BIP44Coin.fromInt(1) must be(BIP44Coin.Testnet)
    forAll(NumberGenerator.ints.suchThat(i => i != 1 && i != 0)) { i =>
      assertThrows[IllegalArgumentException](BIP44Coin.fromInt(i))
    }
  }

  it must "be convertable to a BIP44Account" in {
    forAll(CryptoGenerators.bip44Coin, NumberGenerator.positiveInts) {
      (coin, index) =>
        val account = coin.toAccount(index)
        assert(account.coin == coin)
    }
  }

  behavior of "BIP44Path"

  it must "have toString/fromString symmetry" in {
    forAll(CryptoGenerators.bip44Path) { path =>
      val fromString = BIP44Path.fromString(path.toString)
      assert(fromString == path)
    }
  }

  it must "fail to generate a BIP44 path with an invalid purpose field" in {
    val badPaths = CryptoGenerators.bip32Path.suchThat { bip32 =>
      bip32.path.nonEmpty &&
      bip32.path.head != BIP44Path.purposeChild
    }

    forAll(badPaths) { badPath =>
      val attempt = Try { BIP44Path.fromString(badPath.toString) }
      attempt match {
        case Failure(exc) =>
          assert(exc.getMessage.contains("first child in a BIP44 path"))
        case Success(_) => fail
      }
    }
  }

  it must "fail to generate BIP44 paths with an invalid length" in {
    forAll(CryptoGenerators.bip44Path) { bip44 =>
      val tooShortPath = bip44.path.dropRight(1)
      val attempt = BIP44Path(tooShortPath)
      attempt match {
        case Success(_) => fail
        case Failure(exception) =>
          assert(exception.getMessage.contains("must have five elements"))
      }
    }
  }

  it must "fail to generate BIP44 paths with the wrong hardened index types" in {
    forAll(CryptoGenerators.bip44Path) { bip44 =>
      val nonHardenedCoinChildren = bip44.path.zipWithIndex.map {
        case (child, index) =>
          if (index == BIP44Path.COIN_INDEX) child.copy(hardened = false)
          else child
      }

      val badCoinAttempt = BIP44Path(nonHardenedCoinChildren)

      badCoinAttempt match {
        case Success(_) => fail
        case Failure(exc) =>
          assert(exc.getMessage.contains("coin type child must be hardened"))
      }

      val nonHardenedAccountChildren = bip44.path.zipWithIndex.map {
        case (child, index) =>
          if (index == BIP44Path.ACCOUNT_INDEX) child.copy(hardened = false)
          else child
      }
      val badAccountAttempt = BIP44Path(nonHardenedAccountChildren)

      badAccountAttempt match {
        case Success(_) => fail
        case Failure(exc) =>
          assert(exc.getMessage.contains("account child must be hardened"))
      }

      val hardenedChainChildren = bip44.path.zipWithIndex.map {
        case (child, index) =>
          if (index == BIP44Path.CHAIN_INDEX) child.copy(hardened = true)
          else child
      }
      val badChainAttempt =
        BIP44Path(hardenedChainChildren)

      badChainAttempt match {
        case Success(_) => fail
        case Failure(exc) =>
          assert(exc.getMessage.contains("chain child must not be hardened"))
      }

      val hardenedAddressChildren = bip44.path.zipWithIndex.map {
        case (child, index) =>
          if (index == BIP44Path.ADDRESS_INDEX) child.copy(hardened = true)
          else child
      }
      val badAddrAttempt =
        BIP44Path(hardenedAddressChildren)

      badAddrAttempt match {
        case Success(_) => fail
        case Failure(exc) =>
          assert(
            exc.getMessage.contains("address index child must not be hardened"))
      }
    }
  }

  // https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#examples
  it must "correctly parse the examples from BIP44" in {
    val first = BIP44Path.fromString(" m / 44' / 0' / 0' / 0 / 0 ")
    assert(first.coin == BIP44Coin.Bitcoin)
    assert(first.account.index == 0)
    assert(first.chain.chainType == BIP44ChainType.External)
    assert(first.address.index == 0)

    val second = BIP44Path.fromString(" m / 44' / 0' / 0' / 0 / 1 ")
    assert(second.coin == BIP44Coin.Bitcoin)
    assert(second.account.index == 0)
    assert(second.chain.chainType == BIP44ChainType.External)
    assert(second.address.index == 1)

    val third = BIP44Path.fromString(" m / 44' / 0' / 0' / 1 / 0 ")
    assert(third.coin == BIP44Coin.Bitcoin)
    assert(third.account.index == 0)
    assert(third.chain.chainType == BIP44ChainType.Change)
    assert(third.address.index == 0)

    val fourth = BIP44Path.fromString(" m / 44' / 0' / 0' / 1 / 1 ")
    assert(fourth.coin == BIP44Coin.Bitcoin)
    assert(fourth.account.index == 0)
    assert(fourth.chain.chainType == BIP44ChainType.Change)
    assert(fourth.address.index == 1)

    val fifth = BIP44Path.fromString(" m / 44' / 0' / 1' / 0 / 0 ")
    assert(fifth.coin == BIP44Coin.Bitcoin)
    assert(fifth.account.index == 1)
    assert(fifth.chain.chainType == BIP44ChainType.External)
    assert(fifth.address.index == 0)

    val sixth = BIP44Path.fromString(" m / 44' / 0' / 1' / 0 / 1 ")
    assert(sixth.coin == BIP44Coin.Bitcoin)
    assert(sixth.account.index == 1)
    assert(sixth.chain.chainType == BIP44ChainType.External)
    assert(sixth.address.index == 1)

    val seventh = BIP44Path.fromString(" m / 44' / 0' / 1' / 1 / 0 ")
    assert(seventh.coin == BIP44Coin.Bitcoin)
    assert(seventh.account.index == 1)
    assert(seventh.chain.chainType == BIP44ChainType.Change)
    assert(seventh.address.index == 0)

    val eigth = BIP44Path.fromString(" m / 44' / 0' / 1' / 1 / 1 ")
    assert(eigth.coin == BIP44Coin.Bitcoin)
    assert(eigth.account.index == 1)
    assert(eigth.chain.chainType == BIP44ChainType.Change)
    assert(eigth.address.index == 1)

    val ninth = BIP44Path.fromString(" m / 44' / 1' / 0' / 0 / 1 ")
    assert(ninth.coin == BIP44Coin.Testnet)
    assert(ninth.account.index == 0)
    assert(ninth.chain.chainType == BIP44ChainType.External)
    assert(ninth.address.index == 1)

    val tenth = BIP44Path.fromString(" m / 44' / 1' / 0' / 0 / 1 ")
    assert(tenth.coin == BIP44Coin.Testnet)
    assert(tenth.account.index == 0)
    assert(tenth.chain.chainType == BIP44ChainType.External)
    assert(tenth.address.index == 1)

    val eleventh = BIP44Path.fromString(" m / 44' / 1' / 0' / 1 / 0 ")
    assert(eleventh.coin == BIP44Coin.Testnet)
    assert(eleventh.account.index == 0)
    assert(eleventh.chain.chainType == BIP44ChainType.Change)
    assert(eleventh.address.index == 0)

    val twelfth = BIP44Path.fromString(" m / 44' / 1' / 0' / 1 / 1 ")
    assert(twelfth.coin == BIP44Coin.Testnet)
    assert(twelfth.account.index == 0)
    assert(twelfth.chain.chainType == BIP44ChainType.Change)
    assert(twelfth.address.index == 1)

    val thirteenth = BIP44Path.fromString(" m / 44' / 1' / 1' / 0 / 0 ")
    assert(thirteenth.coin == BIP44Coin.Testnet)
    assert(thirteenth.account.index == 1)
    assert(thirteenth.chain.chainType == BIP44ChainType.External)
    assert(thirteenth.address.index == 0)

    val fourteenth = BIP44Path.fromString(" m / 44' / 1' / 1' / 0 / 1 ")
    assert(fourteenth.coin == BIP44Coin.Testnet)
    assert(fourteenth.account.index == 1)
    assert(fourteenth.chain.chainType == BIP44ChainType.External)
    assert(fourteenth.address.index == 1)

    val fifteenth = BIP44Path.fromString(" m / 44' / 1' / 1' / 1 / 0 ")
    assert(fifteenth.coin == BIP44Coin.Testnet)
    assert(fifteenth.account.index == 1)
    assert(fifteenth.chain.chainType == BIP44ChainType.Change)
    assert(fifteenth.address.index == 0)

    val sixteenth = BIP44Path.fromString(" m / 44' / 1' / 1' / 1 / 1 ")
    assert(sixteenth.coin == BIP44Coin.Testnet)
    assert(sixteenth.account.index == 1)
    assert(sixteenth.chain.chainType == BIP44ChainType.Change)
    assert(sixteenth.address.index == 1)

  }
}
