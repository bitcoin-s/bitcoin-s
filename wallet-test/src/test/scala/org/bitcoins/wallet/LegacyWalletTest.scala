package org.bitcoins.wallet

import org.bitcoins.core.hd.{AddressType, HDPurposes}
import org.bitcoins.core.protocol.{Bech32Address, P2PKHAddress}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.scalatest.FutureOutcome

class LegacyWalletTest extends BitcoinSWalletTest {

  override type FixtureParam = UnlockedWalletApi

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withLegacyWallet(test)

  it should "generate legacy addresses" in { wallet: UnlockedWalletApi =>
    for {
      addr <- wallet.getNewAddress()
      account <- wallet.getDefaultAccount()
      otherAddr <- wallet.getNewAddress()
      thirdAddr <- wallet.getNewAddress(AddressType.Legacy)
      allAddrs <- wallet.listAddresses()
    } yield {
      assert(account.hdAccount.purpose == HDPurposes.Legacy)
      assert(allAddrs.forall(_.address.isInstanceOf[P2PKHAddress]))
      assert(allAddrs.length == 3)
      assert(allAddrs.exists(_.address == addr))
      assert(allAddrs.exists(_.address == otherAddr))
      assert(allAddrs.exists(_.address == thirdAddr))
    }
  }

  it should "generate segwit addresses" in { wallet =>
    for {
      account <- wallet.getDefaultAccountForType(AddressType.SegWit)
      addr <- wallet.getNewAddress(AddressType.SegWit)
    } yield {
      assert(account.hdAccount.purpose == HDPurposes.SegWit)
      assert(addr.isInstanceOf[Bech32Address])
    }
  }

  it should "generate mixed addresses" in { wallet =>
    for {
      segwit <- wallet.getNewAddress(AddressType.SegWit)
      legacy <- wallet.getNewAddress(AddressType.Legacy)
      // TODO: uncomment this once nested segwit is implemented
      // https://github.com/bitcoin-s/bitcoin-s/issues/407
      // nested <- wallet.getNewAddress(AddressType.NestedSegWit)
    } yield {
      assert(segwit.isInstanceOf[Bech32Address])
      assert(legacy.isInstanceOf[P2PKHAddress])
      // TODO: uncomment this once nested segwit is implemented
      // https://github.com/bitcoin-s/bitcoin-s/issues/407
      // assert(nested.isInstanceOf[P2SHAddress])
    }
  }
}
