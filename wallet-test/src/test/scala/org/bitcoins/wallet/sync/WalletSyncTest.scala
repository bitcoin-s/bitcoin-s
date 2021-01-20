package org.bitcoins.wallet.sync

import org.bitcoins.testkit.chain.SyncUtil
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletWithBitcoindV19}
import org.scalatest.FutureOutcome

class WalletSyncTest extends BitcoinSWalletTest {

  behavior of "WalletSync"

  override type FixtureParam = WalletWithBitcoindV19

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoindV19(test, getBIP39PasswordOpt())

  it must "sync a wallet with bitcoind" in { param =>
    val wallet = param.wallet
    val bitcoind = param.bitcoind
    //first we need to implement the 'getBestBlockHashFunc' and 'getBlockHeaderFunc' functions
    val getBestBlockHashFunc = SyncUtil.getBestBlockHashFunc(bitcoind)

    val getBlockHeaderFunc = SyncUtil.getBlockHeaderFunc(bitcoind)

    val getBlockFunc = SyncUtil.getBlockFunc(bitcoind)
    val syncedWalletF = WalletSync.syncFullBlocks(wallet,
                                                  getBlockHeaderFunc,
                                                  getBestBlockHashFunc,
                                                  getBlockFunc)

    val bitcoindBestHeaderF = bitcoind.getBestBlockHeader()
    for {
      syncedWallet <- syncedWalletF
      descriptorOpt <- syncedWallet.getSyncDescriptorOpt()
      bitcoindBestHeader <- bitcoindBestHeaderF
    } yield {
      descriptorOpt match {
        case Some(descriptor) =>
          assert(descriptor.bestHash == bitcoindBestHeader.hashBE)
          assert(descriptor.height == bitcoindBestHeader.height)
        case None =>
          fail(s"Could not sync wallet with bitcoind, got no descriptor!")
      }
    }
  }
}
