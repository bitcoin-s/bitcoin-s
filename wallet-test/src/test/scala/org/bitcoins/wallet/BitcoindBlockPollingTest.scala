package org.bitcoins.wallet

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency._
import org.bitcoins.db.AppConfig
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.{BitcoinSAppConfig, BitcoindRpcBackendUtil}
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesFundedCached,
  CachedBitcoindNewest
}
import org.bitcoins.testkit.util.BitcoinSAsyncFixtureTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}

import scala.concurrent.duration.DurationInt

class BitcoindBlockPollingTest
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindNewest
    with EmbeddedPg {

  override type FixtureParam = BitcoindRpcClient

  /** Wallet config with data directory set to user temp directory */
  implicit protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.walletConf)
    super[EmbeddedPg].beforeAll()
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[EmbeddedPg].afterAll()
  }

  it must "properly setup and poll blocks from bitcoind" in { bitcoind =>
    val amountToSend = Bitcoins.one

    for {
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

      // assert wallet hasn't seen it yet
      firstBalance <- wallet.getBalance()
      _ = assert(firstBalance == Satoshis.zero)

      // Setup block polling
      _ = BitcoindRpcBackendUtil.startBitcoindBlockPolling(wallet,
                                                           bitcoind,
                                                           1.second)
      _ <- bitcoind.generateToAddress(6, addr)

      // Wait for it to process
      _ <- AsyncUtil.awaitConditionF(
        () => wallet.getBalance().map(_ > Satoshis.zero),
        1.second)

      balance <- wallet.getBalance()
      //clean up
      _ <- wallet.stop()
    } yield assert(balance == amountToSend)
  }
}
