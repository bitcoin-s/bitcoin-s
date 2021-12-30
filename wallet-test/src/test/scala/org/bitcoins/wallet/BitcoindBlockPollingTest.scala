package org.bitcoins.wallet

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency._
import org.bitcoins.server.BitcoindRpcBackendUtil
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletAppConfigWithBitcoindNewestFixtures
}
import org.bitcoins.testkitcore.util.TestUtil.bech32Address

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
                                                              1.second)
        _ <- bitcoind.generateToAddress(6, bech32Address)

        // Wait for it to process
        _ <- AsyncUtil.awaitConditionF(
          () => wallet.getBalance().map(_ > Satoshis.zero),
          1.second)

        balance <- wallet.getConfirmedBalance()
        //clean up
        _ <- wallet.walletConfig.stop()
      } yield assert(balance == amountToSend)
  }
}
