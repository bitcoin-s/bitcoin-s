package org.bitcoins.wallet

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.{ChainCallbacks, OnSyncFlagChanged}
import org.bitcoins.core.api.wallet.SyncHeightDescriptor
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.rpc.client.v22.BitcoindV22RpcClient
import org.bitcoins.server.BitcoindRpcBackendUtil
import org.bitcoins.testkit.wallet._
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.Future

class BitcoindBackendTest extends WalletAppConfigWithBitcoindNewestFixtures {

  it must "correctly catch up to bitcoind" in { walletAppConfigWithBitcoind =>
    val bitcoind = walletAppConfigWithBitcoind.bitcoind
    val amountToSend = Bitcoins.one
    @volatile var syncingValues = Vector.empty[Boolean]
    val callback: OnSyncFlagChanged = { (value: Boolean) =>
      Future {
        synchronized {
          syncingValues = syncingValues :+ value
        }
      }
    }

    val callbacks = ChainCallbacks.onOnSyncFlagChanged(callback)

    for {
      header <- bitcoind.getBestBlockHeader()

      // Setup wallet
      wallet <- createWallet(walletAppConfigWithBitcoind)
      // Assert wallet is empty
      isEmpty <- wallet.isEmpty()
      _ = assert(isEmpty)

      // Send to wallet
      addr <- wallet.getNewAddress()
      _ <- bitcoind.sendToAddress(addr, amountToSend)
      bitcoindAddr <- bitcoind.getNewAddress
      _ <- bitcoind.generateToAddress(6, bitcoindAddr)

      // assert wallet hasn't seen it yet
      firstBalance <- wallet.getBalance()
      _ = assert(firstBalance == Satoshis.zero)

      // Set sync height
      _ <-
        wallet.stateDescriptorDAO.updateSyncHeight(header.hashBE, header.height)

      syncing <- bitcoind.isSyncing()
      _ = assert(!syncing)
      _ <- BitcoindRpcBackendUtil.syncWalletToBitcoind(bitcoind,
                                                       wallet,
                                                       Some(callbacks))
      _ <- AsyncUtil.awaitConditionF { () => bitcoind.isSyncing().map(!_) }

      balance <- wallet.getBalance()

      height <- bitcoind.getBlockCount
      bestHash <- bitcoind.getBestBlockHash
      syncHeightOpt <- wallet.getSyncDescriptorOpt()
    } yield {
      assert(balance == amountToSend)
      assert(syncHeightOpt.contains(SyncHeightDescriptor(bestHash, height)))
      synchronized {
        assert(syncingValues == Vector(true, false))
      }
    }
  }

  it must "confirm a utxo" in { walletAppConfigWithBitcoind =>
    val bitcoind = walletAppConfigWithBitcoind.bitcoind

    val amountToSend = Bitcoins.one
    for {
      header <- bitcoind.getBestBlockHeader()

      // Setup wallet
      wallet <- createWallet(walletAppConfigWithBitcoind)

      // Assert wallet is empty
      isEmpty <- wallet.isEmpty()
      _ = assert(isEmpty)

      // Send to wallet
      addr <- wallet.getNewAddress()
      _ <- bitcoind.sendToAddress(addr, amountToSend)
      bitcoindAddr <- bitcoind.getNewAddress
      _ <- bitcoind.generateToAddress(wallet.walletConfig.requiredConfirmations,
                                      bitcoindAddr)

      // assert wallet hasn't seen it yet
      firstBalance <- wallet.getBalance()
      _ = assert(firstBalance == Satoshis.zero)

      // Set sync height
      _ <- wallet.stateDescriptorDAO.updateSyncHeight(header.hashBE,
                                                      header.height)

      _ <- BitcoindRpcBackendUtil.syncWalletToBitcoind(bitcoind, wallet, None)

      utxos <- wallet.listUtxos(TxoState.ConfirmedReceived)
    } yield {
      assert(utxos.size == 1)
      val utxo = utxos.head
      assert(utxo.state == TxoState.ConfirmedReceived)
      assert(utxo.output.value == amountToSend)
    }
  }

  it must "sync a filter" in { walletAppConfigWithBitcoind =>
    val bitcoind =
      walletAppConfigWithBitcoind.bitcoind.asInstanceOf[BitcoindV22RpcClient]

    val amountToSend = Bitcoins.one
    for {
      // Setup wallet
      wallet <- createWallet(walletAppConfigWithBitcoind)

      // Assert wallet is empty
      isEmpty <- wallet.isEmpty()
      _ = assert(isEmpty)

      // Send to wallet
      addr <- wallet.getNewAddress()
      _ <- bitcoind.sendToAddress(addr, amountToSend)
      bitcoindAddr <- bitcoind.getNewAddress
      _ <- bitcoind.generateToAddress(1, bitcoindAddr)

      // assert wallet hasn't seen it yet
      firstBalance <- wallet.getBalance()
      _ = assert(firstBalance == Satoshis.zero)

      header <- bitcoind.getBestBlockHeader()

      filterResult <- bitcoind.getBlockFilter(header.hashBE, FilterType.Basic)
      filter = filterResult.filter
      _ <- wallet.processCompactFilter(header.hash, filter)

      balance <- wallet.getBalance()
    } yield {
      assert(balance == amountToSend)
    }
  }

  it must "sync a filter and update utxos to confirmed" in {
    walletAppConfigWithBitcoind =>
      val bitcoind =
        walletAppConfigWithBitcoind.bitcoind.asInstanceOf[BitcoindV22RpcClient]

      val amountToSend = Bitcoins.one
      for {
        // Setup wallet
        wallet <- createWallet(walletAppConfigWithBitcoind)

        // Assert wallet is empty
        isEmpty <- wallet.isEmpty()
        _ = assert(isEmpty)

        // Send to wallet
        addr <- wallet.getNewAddress()
        _ <- bitcoind.sendToAddress(addr, amountToSend)
        bitcoindAddr <- bitcoind.getNewAddress
        _ <- bitcoind.generateToAddress(1, bitcoindAddr)

        // assert wallet hasn't seen it yet
        firstBalance <- wallet.getBalance()
        _ = assert(firstBalance == Satoshis.zero)

        header <- bitcoind.getBestBlockHeader()

        filterResult <- bitcoind.getBlockFilter(header.hashBE, FilterType.Basic)
        filter = filterResult.filter
        _ <- wallet.processCompactFilter(header.hash, filter)

        unconfirmedBalance <- wallet.getUnconfirmedBalance()
        confirmedBalance <- wallet.getConfirmedBalance()
        _ = assert(unconfirmedBalance == amountToSend)
        _ = assert(confirmedBalance == Satoshis.zero)

        // confirm utxos
        _ <- bitcoind.generateToAddress(
          wallet.walletConfig.requiredConfirmations,
          bitcoindAddr)

        // sync wallet
        _ <- BitcoindRpcBackendUtil.syncWalletToBitcoind(bitcoind, wallet, None)

        unconfirmedBalance <- wallet.getUnconfirmedBalance()
        confirmedBalance <- wallet.getConfirmedBalance()
      } yield {
        assert(confirmedBalance == amountToSend)
        assert(unconfirmedBalance == Satoshis.zero)
      }
  }

  private def createWallet(
      params: WalletAppConfigWithBitcoindRpc): Future[Wallet] = {
    val bitcoind = params.bitcoind
    implicit val walletAppConfig: WalletAppConfig = params.walletAppConfig

    for {
      tmpWallet <- BitcoinSWalletTest.createDefaultWallet(nodeApi = bitcoind,
                                                          chainQueryApi =
                                                            bitcoind)
    } yield {
      BitcoindRpcBackendUtil.createWalletWithBitcoindCallbacks(bitcoind,
                                                               tmpWallet,
                                                               None)
    }
  }
}
