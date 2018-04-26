package org.bitcoins.zmq

import java.net.{ InetSocketAddress, Socket }

import org.bitcoins.core.crypto.{ DoubleSha256Digest, HashDigest }
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.BitcoinSLogger
import org.zeromq.{ ZMQ, ZMsg }

import scala.concurrent.{ ExecutionContext, Future }

/**
 * This class is designed to consume a zmq stream from a cryptocurrency's daemon.
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
  hashTxListener: Option[Seq[Byte] => Future[Unit]],
  hashBlockListener: Option[Seq[Byte] => Future[Unit]],
  rawTxListener: Option[Seq[Byte] => Future[Unit]],
  rawBlockListener: Option[Seq[Byte] => Future[Unit]]) {
  private val logger = BitcoinSLogger.logger

  private var run = false
  private val context = ZMQ.context(1)

  private val subscriber = context.socket(ZMQ.SUB)
  private val uri = socket.getHostString + ":" + socket.getPort

  def start()(implicit ec: ExecutionContext): Future[Unit] = Future {
    logger.info("starting zmq")
    subscriber.connect(uri)
    logger.info("Connection to zmq client successful")
    //subscribe to the appropriate feed
    hashTxListener.map { _ =>
      subscriber.subscribe(HashTx.topic.getBytes(ZMQ.CHARSET))
      logger.debug("subscribed to the transaction hashes from zmq")
    }

    rawTxListener.map { _ =>
      subscriber.subscribe(RawTx.topic.getBytes(ZMQ.CHARSET))
      logger.debug("subscribed to raw transactions from zmq")
    }

    hashBlockListener.map { _ =>
      subscriber.subscribe(HashBlock.topic.getBytes(ZMQ.CHARSET))
      logger.debug("subscribed to the hashblock stream from zmq")
    }

    rawBlockListener.map { _ =>
      subscriber.subscribe(RawBlock.topic.getBytes(ZMQ.CHARSET))
      logger.debug("subscribed to raw block stream from zmq")
    }

    run = true
    while (run) {
      val zmsg = ZMsg.recvMsg(subscriber)
      val notificationTypeStr = zmsg.pop().getString(ZMQ.CHARSET)
      val body = zmsg.pop().getData
      val processedMsg = processMsg(notificationTypeStr, body)
      processedMsg.onFailure {
        case err =>
          logger.error(err.getMessage)
      }
    }
  }

  /**
   * Stops running the zmq subscriber and cleans up after zmq
   * http://zguide.zeromq.org/java:psenvsub
   */
  def stop: Unit = {
    //i think this could technically not work, because currently we are blocking
    //on Zmsg.recvMsg in our while loop. If we don't get another message we won't
    //be able toe evaluate the while loop again. Moving forward with this for now.
    run = false
    subscriber.close()
    context.term()
  }

  /** Processes a message that we received the from the cryptocurrency daemon and then
    * applies the appropriate listener to that message.
    */
  private def processMsg(topic: String, body: Seq[Byte])(implicit ec: ExecutionContext): Future[Unit] = {
    val notification = ZMQNotification.fromString(topic)
    val res: Option[Future[Unit]] = notification.flatMap {
      case HashTx =>
        hashTxListener.map { f =>
          f(body)
        }
      case RawTx =>
        rawTxListener.map { f =>
          f(body)
        }
      case HashBlock =>
        hashBlockListener.map { f =>
          f(body)
        }
      case RawBlock =>
        rawBlockListener.map { f =>
          f(body)
        }
    }

    res match {
      case Some(f) => f
      case None => Future.successful(Unit)
    }
  }
}
