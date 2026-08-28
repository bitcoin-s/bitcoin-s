package org.bitcoins.server

import org.apache.pekko.stream.scaladsl.SourceQueueWithComplete
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.StartStop
import org.bitcoins.zmq.ZMQSubscriber

import java.util.concurrent.atomic.AtomicBoolean

case class WalletZmqSubscribers(
    rawTxSubscriberOpt: Option[ZMQSubscriber],
    rawBlockSubscriberOpt: Option[ZMQSubscriber],
    rawTxQueueOpt: Option[SourceQueueWithComplete[Transaction]],
    rawBlockQueueOpt: Option[SourceQueueWithComplete[Block]]
) extends StartStop[Unit] {
  private val isStarted: AtomicBoolean = new AtomicBoolean(false)

  override def start(): Unit = {
    if (isStarted.get()) {
      ()
    } else {
      rawTxSubscriberOpt.foreach(_.start())
      rawBlockSubscriberOpt.foreach(_.start())
      isStarted.set(true)
    }
  }

  override def stop(): Unit = {
    if (isStarted.get()) {
      rawTxSubscriberOpt.foreach(_.stop())
      rawBlockSubscriberOpt.foreach(_.stop())
      rawTxQueueOpt.foreach(_.complete())
      rawBlockQueueOpt.foreach(_.complete())
      isStarted.set(false)
    } else {
      ()
    }

  }
}
