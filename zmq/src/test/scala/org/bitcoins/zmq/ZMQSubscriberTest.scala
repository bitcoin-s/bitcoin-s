package org.bitcoins.zmq

import java.net.InetSocketAddress

import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{ FlatSpec, MustMatchers }
import org.slf4j.LoggerFactory
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
    zmqSub.start()
    Thread.sleep(100000) // 100 seconds
    zmqSub.stop
  }

  val rawBlockListener: Option[scodec.bits.ByteVector => Unit] = Some {
    { bytes: scodec.bits.ByteVector =>
      val hex = BitcoinSUtil.encodeHex(bytes)
      logger.debug(s"received raw block ${hex}")
    }
  }

  val hashBlockListener: Option[scodec.bits.ByteVector => Unit] = Some {
    { bytes: scodec.bits.ByteVector =>
      val hex = BitcoinSUtil.encodeHex(bytes)
      logger.debug(s"received raw block hash ${hex}")

    }
  }

  val rawTxListener: Option[scodec.bits.ByteVector => Unit] = Some {
    { bytes: scodec.bits.ByteVector =>
      val hex = BitcoinSUtil.encodeHex(bytes)
      logger.debug(s"received raw tx ${hex}")
    }
  }
}
