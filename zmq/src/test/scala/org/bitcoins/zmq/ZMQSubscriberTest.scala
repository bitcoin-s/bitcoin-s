package org.bitcoins.zmq

import java.net.InetSocketAddress

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.scalatest.{ FlatSpec, MustMatchers }
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, Future }
class ZMQSubscriberTest extends FlatSpec with MustMatchers {
  private val logger = LoggerFactory.getLogger(this.getClass().toString)
  "ZMQSubscriber" must "connect to a regtest instance of a daemon and stream txs/blocks from it" in {
    //note for this unit test to pass, you need to setup a bitcoind instance yourself
    //and set the bitcoin.conf file to allow for
    //zmq connections
    //see: https://github.com/bitcoin/bitcoin/blob/master/doc/zmq.md
    val socket = new InetSocketAddress("tcp://127.0.0.1", 28332)

    val zmqSub = new ZMQSubscriber(socket, None, None, rawTxListener, rawBlockListener)
    //stupid, doesn't test anything, for now. You need to look at log output to verify this is working
    Await.result(zmqSub.start(), 100.seconds)
    zmqSub.stop
  }

  val rawBlockListener: Option[Seq[Byte] => Future[Unit]] = Some {
    { bytes: Seq[Byte] =>
      val block = Future(Block.fromBytes(bytes))
      block.map { b =>
        logger.debug(s"received block $b")
        Future.successful(Unit)
      }
    }
  }

  val hashBlockListener: Option[Seq[Byte] => Future[Unit]] = Some {
    { bytes: Seq[Byte] =>
      val hash = Future(DoubleSha256Digest.fromBytes(bytes))
      hash.map { h =>
        logger.debug(s"received block hash $h")
        Future.successful(Unit)
      }

    }
  }

  val rawTxListener: Option[Seq[Byte] => Future[Unit]] = Some {
    { bytes: Seq[Byte] =>
      val txFuture = Future(Transaction.fromBytes(bytes))
      txFuture.map { tx =>
        logger.debug(s"received tx ${tx}")
        Future.successful(Unit)
      }
    }
  }
}
