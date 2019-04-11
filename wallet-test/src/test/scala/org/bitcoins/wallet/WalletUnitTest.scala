package org.bitcoins.wallet

import org.bitcoins.wallet.api.{
  InitializeWalletError,
  InitializeWalletSuccess,
  UnlockedWalletApi
}
import org.bitcoins.wallet.util.BitcoinSWalletTest

import scala.concurrent.Future

class WalletUnitTest extends BitcoinSWalletTest {
  behavior of "Wallet - unit test"

  lazy val walletF: Future[UnlockedWalletApi] = Wallet
    .initialize(
    chainParams = chainParams,
      dbConfig = dbConfig
    )
    .map {
      case InitializeWalletSuccess(wallet) => wallet
      case err: InitializeWalletError      => fail(err)
    }

  it should "create a new wallet" in {
    for {
      _ <- walletF
      accounts <- accountDAO.findAll()
      mnemonic <- mnemonicDAO.read()
      addresses <- addressDAO.findAll()
    } yield {
      assert(accounts.length == 1)
      assert(addresses.isEmpty)
      assert(mnemonic.isDefined)
    }
  }

  // eventually this test should NOT succeed, as BIP44
  // requires a limit to addresses being generated when
  // they haven't received any funds
  it should "generate addresses" in {
    for {
      wallet <- walletF
      addr <- wallet.getNewAddress()
      otherAddr <- wallet.getNewAddress()
      allAddrs <- addressDAO.findAll()
    } yield {
      assert(allAddrs.length == 2)
      assert(allAddrs.head.address == addr)
      assert(allAddrs.last.address == otherAddr)
    }
  }

}
