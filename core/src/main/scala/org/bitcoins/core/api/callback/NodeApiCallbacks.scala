package org.bitcoins.core.api.callback

import org.bitcoins.core.api.{Callback, CallbackHandler}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction

import scala.concurrent.{ExecutionContext, Future}

/** Callback for handling a received block */
trait OnBlockReceived extends Callback[Block]
trait OnTxReceived extends Callback[Transaction]

trait NodeApiCallbacks[T <: ModuleCallbacks[T]] extends ModuleCallbacks[T] {
  def onBlockReceived: CallbackHandler[Block, OnBlockReceived]
  def executeOnBlockReceivedCallbacks(
      block: Block
  )(implicit ec: ExecutionContext): Future[Unit] = {
    onBlockReceived.execute(block)
  }

  def onTxReceived: CallbackHandler[Transaction, OnTxReceived]
  def executeOnTxReceivedCallbacks(
      tx: Transaction
  )(implicit ec: ExecutionContext): Future[Unit] = {
    onTxReceived.execute(tx)
  }

}
