package org.bitcoins.zmq

import java.net.InetSocketAddress

import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.zeromq.{SocketType, ZMQ, ZMQException, ZMsg}
import scodec.bits.ByteVector

/** This class is designed to consume a zmq stream from a cryptocurrency's daemon.
  * An example of this is  bitcoind. For information on how to setup your coin's  conf
  * file to be able to consume a zmq stream please see
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
    rawBlockListener: Option[Block => Unit])
    extends BitcoinSLogger {

  private var running = true
  private val context = ZMQ.context(1)

  private val subscriber: ZMQ.Socket = context.socket(SocketType.SUB)

  private val uri = socket.getHostString + ":" + socket.getPort

  private case object SubscriberRunnable extends Runnable {

    override def run(): Unit = {
      logger.info(s"ZmqSubscriber connecting to uri=${uri}")
      subscriber.setLinger(2000)
      val isConnected = subscriber.connect(uri)

      if (isConnected) {
        hashTxListener.foreach { _ =>
          subscriber.subscribe(HashTx.topic.getBytes(ZMQ.CHARSET))
          logger.debug("subscribed to the transaction hashes from zmq")
        }

        rawTxListener.foreach { _ =>
          subscriber.subscribe(RawTx.topic.getBytes(ZMQ.CHARSET))
          logger.debug("subscribed to raw transactions from zmq")
        }

        hashBlockListener.foreach { _ =>
          subscriber.subscribe(HashBlock.topic.getBytes(ZMQ.CHARSET))
          logger.debug("subscribed to the hashblock stream from zmq")
        }

        rawBlockListener.foreach { _ =>
          subscriber.subscribe(RawBlock.topic.getBytes(ZMQ.CHARSET))
          logger.debug("subscribed to raw block stream from zmq")
        }

        while (running && !subscriberThread.isInterrupted) {
          try {
            val zmsg = ZMsg.recvMsg(subscriber, ZMQ.NOBLOCK)
            if (zmsg != null) {
              val notificationTypeStr = zmsg.pop().getString(ZMQ.CHARSET)
              val body = zmsg.pop().getData
              processMsg(notificationTypeStr, body)
            } else {
              Thread.sleep(100)
            }
          } catch {
            case e: ZMQException if e.getErrorCode == ZMQ.Error.ETERM.getCode =>
              context.term()
              logger.info(s"Done terminating zmq context msg=${e.getMessage}")
            case e: Exception =>
              context.term()
              logger.info(s"Done terminating zmq context msg=${e.getMessage}")
          }
        }
        logger.info(s"Terminated")
      } else {
        logger.error(s"Failed to connect to zmq socket ${uri}")
        throw new RuntimeException(s"Failed to connect to zmq socket ${uri}")
      }

    }
  }

  private val subscriberThread = new Thread(SubscriberRunnable)
  subscriberThread.setName(
    s"ZMQSubscriber-thread-${System.currentTimeMillis()}")
  subscriberThread.setDaemon(true)

  def start(): Unit = {
    logger.info("starting zmq")
    subscriberThread.start()
  }

  /** Stops running the zmq subscriber and cleans up after zmq
    * http://zguide.zeromq.org/java:psenvsub
    */
  def stop(): Unit = {
    logger.info(s"Stopping zmq")
    //i think this could technically not work, because currently we are blocking
    //on Zmsg.recvMsg in our while loop. If we don't get another message we won't
    //be able toe evaluate the while loop again. Moving forward with this for now.
    running = false
    subscriber.close()
    logger.info("Attempting to terminate context")
    context.term()
    logger.info(s"Done with closing zmq")
    ()
  }

  /** Processes a message that we received the from the cryptocurrency daemon and then
    * applies the appropriate listener to that message.
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
