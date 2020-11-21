package org.bitcoins.zmq

import java.net.InetSocketAddress

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.scalatest.flatspec.AsyncFlatSpec
import org.zeromq.{SocketType, ZFrame, ZMQ, ZMsg}

import scala.concurrent.Promise

class ZMQSubscriberTest extends AsyncFlatSpec with BitcoinSLogger {

  behavior of "ZMQSubscriber"

  val rawBlockListener: Option[Block => Unit] = Some {
    { block: Block =>
      logger.debug(s"received raw block ${block.hex}")
    }
  }

  val hashBlockListener: Option[DoubleSha256DigestBE => Unit] = Some {
    { hash: DoubleSha256DigestBE =>
      logger.debug(s"received raw block hash ${hash.hex}")
    }
  }

  val rawTxListener: Option[Transaction => Unit] = Some {
    { tx: Transaction =>
      logger.debug(s"received raw tx ${tx.hex}")
    }
  }

  it must "connect to a regtest instance of a daemon and stream txs/blocks from it" in {
    //note for this unit test to pass, you need to setup a bitcoind instance yourself
    //and set the bitcoin.conf file to allow for
    //zmq connections
    //see: https://github.com/bitcoin/bitcoin/blob/master/doc/zmq.md
    val socket = new InetSocketAddress("tcp://127.0.0.1", 29000)

    val zmqSub =
      new ZMQSubscriber(socket, None, None, rawTxListener, rawBlockListener)
    //stupid, doesn't test anything, for now. You need to look at log output to verify this is working
    // TODO: In the future this could use the testkit to verify the subscriber by calling generate(1)
    zmqSub.start()
    Thread.sleep(10000) // 10 seconds
    zmqSub.stop()

    succeed
  }

  it must "be able to subscribe to a publisher and read a value" in {
    val port = Math.abs(scala.util.Random.nextInt() % 14000) + 1024
    val socket = new InetSocketAddress("tcp://127.0.0.1", port)

    val context = ZMQ.context(1)
    val publisher = context.socket(SocketType.PUB)

    val uri = socket.getHostString + ":" + socket.getPort
    publisher.bind(uri)

    val valuePromise = Promise[Array[Byte]]()
    val fakeBlockListener: Option[Block => Unit] = Some { block =>
      val bytes = block.bytes.toArray
      valuePromise.success(bytes)
      ()
    }

    val sub = new ZMQSubscriber(socket, None, None, None, fakeBlockListener)
    sub.start()
    Thread.sleep(1000)

    val testValue = MainNet.chainParams.genesisBlock.bytes.toArray

    val msg = new ZMsg()
    msg.add(new ZFrame(RawBlock.topic))
    msg.add(new ZFrame(testValue))

    val sent = msg.send(publisher)
    assert(sent)

    valuePromise.future.map { bytes =>
      sub.stop()
      publisher.close()
      context.term()

      assert(bytes sameElements testValue)
    }
  }
}
