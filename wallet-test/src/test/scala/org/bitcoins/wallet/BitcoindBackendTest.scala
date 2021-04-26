package org.bitcoins.wallet

import org.bitcoins.commons.jsonmodels.wallet.SyncHeightDescriptor
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

class BitcoindBackendTest
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

  it must "correctly catch up to bitcoind" in { bitcoind =>
    val amountToSend = Bitcoins.one

    for {
      header <- bitcoind.getBestBlockHeader()

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

      // Set sync height
      _ <-
        wallet.stateDescriptorDAO.updateSyncHeight(header.hashBE, header.height)

      _ <- BitcoindRpcBackendUtil.syncWalletToBitcoind(bitcoind, wallet)

      balance <- wallet.getBalance()

      height <- bitcoind.getBlockCount
      bestHash <- bitcoind.getBestBlockHash
      syncHeightOpt <- wallet.getSyncDescriptorOpt()
      _ = assert(balance == amountToSend)
      _ = assert(syncHeightOpt.contains(SyncHeightDescriptor(bestHash, height)))
      _ <- wallet.walletConfig.stop()
    } yield succeed
  }
}
