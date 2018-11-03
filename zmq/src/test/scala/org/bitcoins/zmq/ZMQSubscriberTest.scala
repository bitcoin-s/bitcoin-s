package org.bitcoins.zmq

import java.net.InetSocketAddress

import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{ AsyncFlatSpec, MustMatchers }
import org.slf4j.LoggerFactory
import org.zeromq.{ ZFrame, ZMQ, ZMsg }
import scodec.bits.ByteVector

import scala.concurrent.Promise
class ZMQSubscriberTest extends AsyncFlatSpec with MustMatchers {
  private val logger = LoggerFactory.getLogger(this.getClass().toString)

  behavior of "ZMQSubscriber"

  it must "connect to a regtest instance of a daemon and stream txs/blocks from it" in {
    //note for this unit test to pass, you need to setup a bitcoind instance yourself
    //and set the bitcoin.conf file to allow for
    //zmq connections
    //see: https://github.com/bitcoin/bitcoin/blob/master/doc/zmq.md
    val socket = new InetSocketAddress("tcp://127.0.0.1", 29000)

    val zmqSub = new ZMQSubscriber(socket, None, None, rawTxListener, rawBlockListener)
    //stupid, doesn't test anything, for now. You need to look at log output to verify this is working
    // TODO: In the future this could use the testkit to verify the subscriber by calling generate(1)
    zmqSub.start()
    Thread.sleep(10000) // 10 seconds
    zmqSub.stop

    succeed
  }

  it must "be able to subscribe to a publisher and read a value" in {
    val port = Math.abs(scala.util.Random.nextInt % 14000) + 1000
    val socket = new InetSocketAddress("tcp://127.0.0.1", port)

    val context = ZMQ.context(1)
    val publisher = context.socket(ZMQ.PUB)

    val uri = socket.getHostString + ":" + socket.getPort
    publisher.bind(uri)

    val valuePromise = Promise[String]()
    val fakeBlockListener: Option[ByteVector => Unit] = Some {
      bytes =>
        val str = new String(bytes.toArray)
        valuePromise.success(str)
        ()
    }

    val sub = new ZMQSubscriber(socket, None, None, None, fakeBlockListener)
    sub.start()
    Thread.sleep(1000)

    val testValue = "sweet, sweet satoshis"

    val msg = new ZMsg()
    msg.add(new ZFrame(RawBlock.topic))
    msg.add(new ZFrame(testValue))

    val sent = msg.send(publisher)
    assert(sent)

    valuePromise.future.map { str =>
      sub.stop
      publisher.close()
      context.term()

      assert(str == testValue)
    }
  }

  val rawBlockListener: Option[ByteVector => Unit] = Some {
    { bytes: ByteVector =>
      val hex = BitcoinSUtil.encodeHex(bytes)
      logger.debug(s"received raw block ${hex}")
    }
  }

  val hashBlockListener: Option[ByteVector => Unit] = Some {
    { bytes: ByteVector =>
      val hex = BitcoinSUtil.encodeHex(bytes)
      logger.debug(s"received raw block hash ${hex}")

    }
  }

  val rawTxListener: Option[ByteVector => Unit] = Some {
    { bytes: ByteVector =>
      val hex = BitcoinSUtil.encodeHex(bytes)
      logger.debug(s"received raw tx ${hex}")
    }
  }
}
