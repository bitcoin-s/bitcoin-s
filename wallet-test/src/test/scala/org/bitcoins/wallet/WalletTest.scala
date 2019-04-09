package org.bitcoins.wallet

import org.bitcoins.wallet.util.BitcoinSWalletTest

class WalletTest extends BitcoinSWalletTest {
  val passphrase = "foobar"

  /*
  it should "create a new wallet" in {

    for {
      _ <- Wallet.initialize(chainParams = chainParams,
                             dbConfig = dbConfig,
                             passphrase = passphrase)
      accounts <- accountDAO.findAll()
      mnemonic <- mnemonicDAO.read()
      addresses <- addressDAO.findAll()
    } yield {
      assert(accounts.length == 1)
      assert(addresses.isEmpty)
      assert(mnemonic.isDefined)
    }
  }
   */

  it should "create addresses" in {
    for {
      wallet <- Wallet.initialize(chainParams = chainParams,
                                  dbConfig = dbConfig,
                                  passphrase = passphrase)
      addr <- wallet.getNewAddress()
      otherAddr <- wallet.getNewAddress()
      allAddrs <- addressDAO.findAll()
    } yield {
      logger.info(s"addr: $addr")
      logger.info(s"otherAddr: $otherAddr")
      assert(allAddrs.length == 2)
      succeed
    }
  }
}
