package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.server.BitcoindRpcBackendUtil
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletAppConfigWithBitcoindNewestFixtures
}
import org.bitcoins.testkitcore.util.TestUtil.bech32Address
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.duration.DurationInt

class BitcoindZMQBackendTest extends WalletAppConfigWithBitcoindNewestFixtures {

  it must "get txs and blocks through zmq" in { walletAppConfigWithBitcoind =>
    val bitcoind = walletAppConfigWithBitcoind.bitcoind
    implicit val walletAppConfig: WalletAppConfig =
      walletAppConfigWithBitcoind.walletAppConfig

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

      addr <- wallet.getNewAddress()

      firstBalance <- wallet.getBalance()
      _ = assert(firstBalance == Satoshis.zero)

      // Setup zmq subscribers
      _ = BitcoindRpcBackendUtil.startZMQWalletCallbacks(
        wallet,
        bitcoind.instance.zmqConfig)

      _ <- bitcoind.sendToAddress(addr, amountToSend)

      // Wait for it to process
      _ <- TestAsyncUtil.awaitConditionF(
        () => wallet.getUnconfirmedBalance().map(_ > Satoshis.zero),
        interval = 1.second,
        maxTries = 100)

      unconfirmed <- wallet.getUnconfirmedBalance()
      _ = assert(unconfirmed == amountToSend)

      confirmed <- wallet.getConfirmedBalance()
      _ = assert(confirmed == Satoshis.zero)

      _ <- bitcoind.generateToAddress(6, bech32Address)

      // Wait for it to process
      _ <- TestAsyncUtil.awaitConditionF(
        () => wallet.getConfirmedBalance().map(_ > Satoshis.zero),
        interval = 1.second,
        maxTries = 100)

      balance <- wallet.getConfirmedBalance()

      // clean up
      _ <- wallet.walletConfig.stop()
    } yield assert(balance == amountToSend)
  }
}
