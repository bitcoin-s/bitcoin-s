package org.bitcoins.dlc

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.rpc.client.common.BitcoindRpcClient

import scala.concurrent.{ExecutionContext, Future}

/** DLC client uses this to communicate transactions to the network and receive confirmations. */
trait BitcoinP2PMessenger {

  /** Sends the input Transaction to the Bitcoin network. */
  def sendTransaction(tx: Transaction): Future[Unit]

  /** Returns a Future that completes when the specified number of blocks have been mined. */
  def waitForConfirmations(blocks: Int): Future[Unit]

  /** Returns a Future that completes once a specified number of blocks have been mined. */
  def waitUntilBlockHeight(blockHeight: Int): Future[Unit]
}

/** For use in testing DLC client with Regtest bitcoind node. */
case class BitcoindRpcMessengerRegtest(client: BitcoindRpcClient)(
    implicit ec: ExecutionContext)
    extends BitcoinP2PMessenger {
  private val addressForMiningF: Future[BitcoinAddress] = client.getNewAddress

  override def sendTransaction(tx: Transaction): Future[Unit] = {
    client.sendRawTransaction(tx).map(_ => ())
  }

  override def waitForConfirmations(blocks: Int): Future[Unit] = {
    addressForMiningF.flatMap { addressForMining =>
      client.generateToAddress(blocks, addressForMining).map(_ => ())
    }
  }

  override def waitUntilBlockHeight(blockHeight: Int): Future[Unit] = {
    for {
      currentCount <- client.getBlockCount
      blocksToMine = blockHeight - currentCount
      addressForMining <- addressForMiningF
      _ <- client.generateToAddress(blocksToMine, addressForMining)
    } yield ()
  }
}
