package org.bitcoins.zmq

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.StartStop
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.zeromq.{SocketType, ZMQ, ZMQException, ZMsg}
import scodec.bits.ByteVector

import java.net.InetSocketAddress

/** This class is designed to consume a zmq stream from a cryptocurrency's
  * daemon. An example of this is bitcoind. For information on how to setup your
  * coin's conf file to be able to consume a zmq stream please see
  * [[https://github.com/bitcoin/bitcoin/blob/master/doc/zmq.md#usage]]
  * [[http://zguide.zeromq.org/java:psenvsub]]
  * @param socket
  * @param hashTxListener
  * @param hashBlockListener
  * @param rawTxListener
  * @param rawBlockListener
  */
class ZMQSubscriber(
    socket: InetSocketAddress,
    hashTxListener: Option[DoubleSha256DigestBE => Unit],
    hashBlockListener: Option[DoubleSha256DigestBE => Unit],
    rawTxListener: Option[Transaction => Unit],
    rawBlockListener: Option[Block => Unit]
) extends BitcoinSLogger
    with StartStop[Unit] {

  private var isConnected = false
  private val context = ZMQ.context(1)

  private val subscriber: ZMQ.Socket = context.socket(SocketType.SUB)

  private val uri = socket.getHostString + ":" + socket.getPort

  private case object SubscriberRunnable extends Runnable {

    override def run(): Unit = {
      while (isConnected && !subscriberThread.isInterrupted) {
        try {
          val zmsg = ZMsg.recvMsg(subscriber)
          if (zmsg != null) {
            val notificationTypeStr = zmsg.pop().getString(ZMQ.CHARSET)
            val body = zmsg.pop().getData
            processMsg(notificationTypeStr, body)
          } else {
            ()
          }
        } catch {
          case e: ZMQException if e.getErrorCode == ZMQ.Error.ETERM.getCode =>
            context.term()
            logger.info(s"Done terminating zmq context msg=${e.getMessage}")
          case scala.util.control.NonFatal(e) =>
            context.term()
            logger.error(
              s"Failed to terminate zmq context gracefully msg=${e.getMessage}",
              e
            )
        }
      }

    }
  }

  private val subscriberThread = new Thread(SubscriberRunnable)
  subscriberThread.setName(s"ZMQSubscriber-thread-${System
      .currentTimeMillis()}")
  subscriberThread.setDaemon(true)

  override def start(): Unit = {
    logger.info(s"ZmqSubscriber connecting to uri=$uri")

    isConnected = subscriber.connect(s"tcp://$uri")
    subscriberThread.start()
    if (isConnected) {
      hashTxListener.foreach { _ =>
        val result = subscriber.subscribe(HashTx.topic.getBytes(ZMQ.CHARSET))
        if (result) logger.debug("subscribed to hashtxs stream from zmq")
        else logger.error(s"Failed to subscribe to ${HashTx.topic}")
      }

      rawTxListener.foreach { _ =>
        val result = subscriber.subscribe(RawTx.topic.getBytes(ZMQ.CHARSET))
        if (result) logger.debug("subscribed to rawtxs stream from zmq")
        else logger.error(s"Failed to subscribe to ${RawTx.topic}")
      }

      hashBlockListener.foreach { _ =>
        val result = subscriber.subscribe(HashBlock.topic.getBytes(ZMQ.CHARSET))
        if (result) logger.debug("subscribed to hashblock stream from zmq")
        else logger.error(s"Failed to subscribe to ${HashBlock.topic}")
      }

      rawBlockListener.foreach { _ =>
        val result = subscriber.subscribe(RawBlock.topic.getBytes(ZMQ.CHARSET))
        if (result) logger.debug("subscribed to raw block stream from zmq")
        else logger.error(s"Failed to subscribe to ${RawBlock.topic}")
      }
    } else {
      logger.error(s"Failed to connect to zmq socket ${uri}")
      throw new RuntimeException(s"Failed to connect to zmq socket ${uri}")
    }
  }

  /** Stops running the zmq subscriber and cleans up after zmq
    * http://zguide.zeromq.org/java:psenvsub
    */
  override def stop(): Unit = {
    logger.info(s"Stopping zmq")
    // i think this could technically not work, because currently we are blocking
    // on Zmsg.recvMsg in our while loop. If we don't get another message we won't
    // be able toe evaluate the while loop again. Moving forward with this for now.
    isConnected = false
    subscriber.close()
    context.term()
    subscriberThread.interrupt()
    ()
  }

  /** Processes a message that we received the from the cryptocurrency daemon
    * and then applies the appropriate listener to that message.
    */
  private def processMsg(topic: String, body: Array[Byte]): Unit = {
    val notification = ZMQNotification.fromString(topic)
    notification.foreach {
      case HashTx =>
        hashTxListener.foreach { f =>
          val hash = DoubleSha256DigestBE(ByteVector(body))
          f(hash)
        }
      case RawTx =>
        rawTxListener.foreach { f =>
          val tx = Transaction(ByteVector(body))
          f(tx)
        }
      case HashBlock =>
        hashBlockListener.foreach { f =>
          val hash = DoubleSha256DigestBE(ByteVector(body))
          f(hash)
        }
      case RawBlock =>
        rawBlockListener.foreach { f =>
          val block = Block(ByteVector(body))
          f(block)
        }
    }

  }
}
