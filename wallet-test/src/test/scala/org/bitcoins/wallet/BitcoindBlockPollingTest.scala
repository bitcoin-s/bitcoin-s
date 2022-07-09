package org.bitcoins.wallet

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.server.BitcoindRpcBackendUtil
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletAppConfigWithBitcoindNewestFixtures
}
import org.bitcoins.testkitcore.util.TestUtil.bech32Address

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.DurationInt

class BitcoindBlockPollingTest
    extends WalletAppConfigWithBitcoindNewestFixtures {

  it must "properly setup and poll blocks from bitcoind" in {
    walletAppConfigWithBitcoind =>
      val bitcoind = walletAppConfigWithBitcoind.bitcoind
      implicit val walletAppConfig = walletAppConfigWithBitcoind.walletAppConfig

      val amountToSend = Bitcoins.one

      for {
        // Setup wallet
        tmpWallet <-
          BitcoinSWalletTest.createDefaultWallet(bitcoind, bitcoind, None)
        wallet =
          BitcoindRpcBackendUtil.createWalletWithBitcoindCallbacks(bitcoind,
                                                                   tmpWallet,
                                                                   None)
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
        _ <- BitcoindRpcBackendUtil.startBitcoindBlockPolling(wallet,
                                                              bitcoind,
          None,
                                                              1.second)
        _ <- bitcoind.generateToAddress(6, bech32Address)

        // Wait for it to process
        _ <- AsyncUtil.awaitConditionF(
          () => wallet.getBalance().map(_ > Satoshis.zero),
          1.second)

        balance <- wallet.getConfirmedBalance()
      } yield assert(balance == amountToSend)
  }

  it must "properly setup and poll mempool transactions from bitcoind" in {
    walletAppConfigWithBitcoind =>
      val bitcoind = walletAppConfigWithBitcoind.bitcoind
      implicit val walletAppConfig = walletAppConfigWithBitcoind.walletAppConfig

      val amountToSend = Bitcoins.one

      val mempoolTxs = ArrayBuffer.empty[Transaction]

      for {
        // Setup wallet
        tmpWallet <-
          BitcoinSWalletTest.createDefaultWallet(bitcoind, bitcoind, None)
        wallet =
          BitcoindRpcBackendUtil.createWalletWithBitcoindCallbacks(bitcoind,
                                                                   tmpWallet,
                                                                   None)

        // populate initial mempool
        addr <- wallet.getNewAddress()
        txid1 <- bitcoind.sendToAddress(addr, amountToSend)

        // Setup block polling
        _ = BitcoindRpcBackendUtil.startBitcoindMempoolPolling(wallet,
                                                               bitcoind,
                                                               1.second) { tx =>
          mempoolTxs += tx
          FutureUtil.unit
        }

        // Send to wallet
        addr <- wallet.getNewAddress()
        txid2 <- bitcoind.sendToAddress(addr, amountToSend)

        // Wait for it to process
        _ <- AsyncUtil.awaitCondition(
          () => {
            mempoolTxs.exists(_.txIdBE == txid1) && mempoolTxs.exists(
              _.txIdBE == txid2)
          },
          1.second)
      } yield succeed
  }
}
