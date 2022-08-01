package org.bitcoins.wallet.sync

import grizzled.slf4j.Logging
import org.bitcoins.core.api.wallet.WalletApi
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.{ExecutionContext, Future}

trait WalletSync extends Logging {

  /** Synchronizes the bitcoin-s' wallet by retrieving each block and then calling
    * [[Wallet.processBlock()]] on the block retrieved
    *
    * WARNING: This should not be used on resource constrained devices
    * as fetching full blocks will use a lot of bandwidth on live networks
    */
  def syncFullBlocks(
      wallet: WalletApi,
      getBlockHeaderFunc: DoubleSha256DigestBE => Future[BlockHeader],
      getBestBlockHashFunc: () => Future[DoubleSha256DigestBE],
      getBlockFunc: DoubleSha256DigestBE => Future[Block],
      genesisBlockHashBE: DoubleSha256DigestBE)(implicit
      ec: ExecutionContext): Future[WalletApi] = {
    val bestBlockHashF = getBestBlockHashFunc()
    val bestBlockHeaderF = for {
      bestBlockHash <- bestBlockHashF
      bestheader <- getBlockHeaderFunc(bestBlockHash)
    } yield bestheader

    val blocksToSyncF = for {
      bestHeader <- bestBlockHeaderF
      blocksToSync <- getBlocksToSync(wallet = wallet,
                                      currentTipBlockHashBE = bestHeader.hashBE,
                                      accum = Vector.empty,
                                      getBlock = getBlockFunc,
                                      genesisBlockHashBE = genesisBlockHashBE)
    } yield blocksToSync

    val syncedWalletF = for {
      blocksToSync <- blocksToSyncF
      syncedWallet <- FutureUtil.foldLeftAsync(wallet, blocksToSync) {
        case (wallet, nextBlock) =>
          wallet.processBlock(nextBlock)
      }
    } yield syncedWallet

    syncedWalletF
  }

  /** Syncs the wallet by walking backwards from the currentTip until we reach our wallet's best blockHash */
  private def getBlocksToSync(
      wallet: WalletApi,
      currentTipBlockHashBE: DoubleSha256DigestBE,
      accum: Vector[Block],
      getBlock: DoubleSha256DigestBE => Future[Block],
      genesisBlockHashBE: DoubleSha256DigestBE)(implicit
      ec: ExecutionContext): Future[Vector[Block]] = {
    val initSyncDescriptorOptF = wallet.getSyncDescriptorOpt()
    for {
      syncDescriptorOpt <- initSyncDescriptorOptF
      walletBestHash = syncDescriptorOpt match {
        case Some(descriptor) => descriptor.bestHash
        case None             => genesisBlockHashBE
      }
      currentBlockOpt <- {
        if (
          walletBestHash == currentTipBlockHashBE || currentTipBlockHashBE == genesisBlockHashBE
        ) {
          Future.successful(None) // done syncing!
        } else {
          getBlock(currentTipBlockHashBE)
            .map(Some(_))
        }
      }
      blocks <- {
        currentBlockOpt match {
          case Some(currentBlock) =>
            //loop again as we need to keep syncing
            getBlocksToSync(
              wallet = wallet,
              currentTipBlockHashBE =
                currentBlock.blockHeader.previousBlockHashBE,
              accum = currentBlock +: accum,
              getBlock = getBlock,
              genesisBlockHashBE = genesisBlockHashBE
            )
          case None =>
            //yay! Done syncing, return all blocks our wallet needs to be synced with
            Future.successful(accum)
        }
      }
    } yield {
      blocks
    }
  }
}

object WalletSync extends WalletSync
