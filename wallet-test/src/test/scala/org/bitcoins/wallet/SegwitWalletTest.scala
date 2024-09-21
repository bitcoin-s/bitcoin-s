package org.bitcoins.wallet

import org.bitcoins.core.hd.{AddressType, HDPurpose}
import org.bitcoins.core.protocol.{Bech32Address, P2PKHAddress, P2SHAddress}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome

class SegwitWalletTest extends BitcoinSWalletTest {

  override type FixtureParam = Wallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withSegwitWallet(test)
  }

  it should "generate segwit addresses" in { wallet =>
    for {
      addr <- wallet.getNewAddress()
      account <- wallet.accountHandling.getDefaultAccount()
      otherAddr <- wallet.getNewAddress()
      thirdAddr <- wallet.addressHandling.getNewAddress(AddressType.SegWit)
      allAddrs <- wallet.addressHandling.listAddresses()
    } yield {
      assert(account.hdAccount.purpose == HDPurpose.SegWit)
      assert(allAddrs.forall(_.address.isInstanceOf[Bech32Address]))
      assert(allAddrs.length == 3)
      assert(allAddrs.exists(_.address == addr))
      assert(allAddrs.exists(_.address == otherAddr))
      assert(allAddrs.exists(_.address == thirdAddr))
    }
  }

  it should "generate legacy addresses" in { wallet =>
    for {
      account <- wallet.accountHandling.getDefaultAccountForType(
        AddressType.Legacy)
      addr <- wallet.addressHandling.getNewAddress(AddressType.Legacy)
    } yield {
      assert(account.hdAccount.purpose == HDPurpose.Legacy)
      assert(addr.isInstanceOf[P2PKHAddress])
    }
  }

  it should "generate mixed addresses" in { wallet =>
    for {
      segwit <- wallet.addressHandling.getNewAddress(AddressType.SegWit)
      legacy <- wallet.addressHandling.getNewAddress(AddressType.Legacy)
      nested <- wallet.addressHandling.getNewAddress(AddressType.NestedSegWit)
    } yield {
      assert(segwit.isInstanceOf[Bech32Address])
      assert(legacy.isInstanceOf[P2PKHAddress])
      assert(nested.isInstanceOf[P2SHAddress])
    }
  }
}
