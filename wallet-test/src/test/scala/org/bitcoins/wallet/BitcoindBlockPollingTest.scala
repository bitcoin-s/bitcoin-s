package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.db.AppConfig
import org.bitcoins.rpc.util.AsyncUtil
import org.bitcoins.server.{BitcoinSAppConfig, BitcoindRpcBackendUtil}
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class BitcoindBlockPollingTest extends BitcoinSAsyncTest with EmbeddedPg {

  /** Wallet config with data directory set to user temp directory */
  implicit protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  implicit protected def walletAppConfig: WalletAppConfig = {
    config.walletConf
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.walletConf)
    super[EmbeddedPg].beforeAll()
  }

  override def afterAll(): Unit = {
    Await.result(config.chainConf.stop(), 1.minute)
    Await.result(config.nodeConf.stop(), 1.minute)
    Await.result(config.walletConf.stop(), 1.minute)
    super[EmbeddedPg].afterAll()
  }

  it must "properly setup and poll blocks from bitcoind" in {
    val amountToSend = Bitcoins.one

    for {
      // Setup bitcoind
      bitcoind <- BitcoinSFixture.createBitcoindWithFunds()
      blockCount <- bitcoind.getBlockCount

      // Setup wallet
      tmpWallet <-
        BitcoinSWalletTest.createDefaultWallet(bitcoind, bitcoind, None)
      wallet =
        BitcoindRpcBackendUtil.createWalletWithBitcoindCallbacks(bitcoind,
                                                                 tmpWallet)
      // Assert wallet is empty
      isEmpty <- wallet.isEmpty()
      _ = assert(isEmpty)

      // Send to wallet
      addr <- wallet.getNewAddress()
      _ <- bitcoind.sendToAddress(addr, amountToSend)
      _ <- bitcoind.generateToAddress(6, addr)

      // assert wallet hasn't seen it yet
      firstBalance <- wallet.getBalance()
      _ = assert(firstBalance == Satoshis.zero)

      // Setup block polling
      _ = BitcoindRpcBackendUtil.startBitcoindBlockPolling(wallet,
                                                           bitcoind,
                                                           blockCount,
                                                           1.second)
      // Wait for it to process
      _ <- AsyncUtil.awaitConditionF(() =>
        wallet.getBalance().map(_ > Satoshis.zero))

      balance <- wallet.getBalance()
    } yield assert(balance == amountToSend)
  }
}
