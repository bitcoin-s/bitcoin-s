package org.bitcoins.server

import org.bitcoins.core.util.StartStop
import org.bitcoins.zmq.ZMQSubscriber

import java.util.concurrent.atomic.AtomicBoolean

case class WalletZmqSubscribers(
    rawTxSubscriberOpt: Option[ZMQSubscriber],
    rawBlockSubscriberOpt: Option[ZMQSubscriber]
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
      isStarted.set(false)
    } else {
      ()
    }

  }
}
