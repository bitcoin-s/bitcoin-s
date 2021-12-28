package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.server.BitcoindRpcBackendUtil
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.wallet._
import org.bitcoins.testkitcore.util.TestUtil.bech32Address
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal

class BitcoindZMQBackendTest extends WalletAppConfigWithBitcoindNewestFixtures {

  it must "get txs and blocks through zmq" in { walletAppConfigWithBitcoind =>
    val bitcoind = walletAppConfigWithBitcoind.bitcoind
    implicit val walletAppConfig: WalletAppConfig =
      walletAppConfigWithBitcoind.walletAppConfig

    val amountToSend = Bitcoins.one

    def attemptZMQTx(addr: BitcoinAddress, wallet: Wallet): Future[Unit] = {
      for {
        _ <- bitcoind.sendToAddress(addr, amountToSend)
        // Wait for it to process
        _ <- TestAsyncUtil.awaitConditionF(
          () => wallet.getUnconfirmedBalance().map(_ > Satoshis.zero),
          interval = 1.second)
      } yield ()
    }

    def attemptZMQBlock(numBlocks: Int, wallet: Wallet): Future[Unit] = {
      for {
        _ <- bitcoind.generateToAddress(numBlocks, bech32Address)
        // Wait for it to process
        _ <- TestAsyncUtil.awaitConditionF(
          () => wallet.getConfirmedBalance().map(_ > Satoshis.zero),
          interval = 1.second)
      } yield ()
    }

    for {
      // Setup wallet
      tmpWallet <-
        BitcoinSWalletTest.createDefaultWallet(bitcoind, bitcoind, None)
      wallet =
        BitcoindRpcBackendUtil.createWalletWithBitcoindCallbacks(
          bitcoind = bitcoind,
          wallet = tmpWallet,
          chainCallbacksOpt = None)
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

      _ <- attemptZMQTx(addr, wallet)
        .recoverWith { case NonFatal(_) =>
          attemptZMQTx(addr, wallet)
        }

      unconfirmed <- wallet.getUnconfirmedBalance()
      _ = assert(unconfirmed == amountToSend)

      confirmed <- wallet.getConfirmedBalance()
      _ = assert(confirmed == Satoshis.zero)

      _ <- attemptZMQBlock(6, wallet)
        .recoverWith { case NonFatal(_) =>
          attemptZMQBlock(1, wallet)
        }

      balance <- wallet.getConfirmedBalance()

      // clean up
      _ <- wallet.walletConfig.stop()
    } yield {
      // use >= because of multiple attempts
      assert(balance >= amountToSend)
    }
  }
}
