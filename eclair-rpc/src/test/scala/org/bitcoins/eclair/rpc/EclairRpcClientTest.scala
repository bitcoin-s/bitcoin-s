package org.bitcoins.eclair.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.currency.{ CurrencyUnit, Satoshis }
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.ln.channel.{ ChannelId, ChannelState }
import org.bitcoins.core.protocol.ln.{ NanoBitcoins, PicoBitcoins }
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.json._
import org.bitcoins.eclair.rpc.network.NodeId
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.BitcoindRpcClient
import org.scalatest.{ Assertion, AsyncFlatSpec, BeforeAndAfterAll }

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, Future }

class EclairRpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {

  implicit val system = ActorSystem("EclairRpcClient")
  implicit val m = ActorMaterializer.create(system)
  implicit val ec = m.executionContext
  implicit val bitcoinNp = EclairTestUtil.network

  val logger = BitcoinSLogger.logger

  val bitcoindRpcClient = BitcoindRpcTestUtil.startedBitcoindRpcClient()
  val (client, otherClient) = EclairTestUtil.createNodePair(Some(bitcoindRpcClient))

  behavior of "RpcClient"

  it should "be able to open a channel" in {
    val result: Future[Assertion] = {
      otherClient.getInfo.flatMap { info =>
        val amt = Satoshis(Int64(100000))
        val openedChanF = client.open(info.nodeId, amt)
        openedChanF.flatMap(channelId => hasChannel(client, channelId))
      }
    }
    result
  }

  it should "be able to list an existing peer and isConnected must be true" in {
    //test assumes that a connection to a peer was made in `beforeAll`
    val otherClientNodeIdF = otherClient.getInfo.map(_.nodeId)
    otherClientNodeIdF.flatMap(nid => hasConnection(client, nid))
  }

  it should "be able to generate a payment invoice and then check that invoice" in {
    val amt = PicoBitcoins(1000) //1 satoshi
    val description = "bitcoin-s test case"
    val expiry = (System.currentTimeMillis() / 1000)

    val invoiceF = client.receive(
      description = description,
      amountMsat = amt,
      expirySeconds = expiry)

    val paymentRequestF: Future[PaymentRequest] = invoiceF.flatMap(i => client.checkInvoice(i))

    paymentRequestF.map { paymentRequest =>
      assert(paymentRequest.amount == Some(amt.toLong))
      assert(paymentRequest.timestamp == expiry)
      assert(paymentRequest.description == description)
    }
  }

  it should "open a channel, send a payment, and close the channel" in {
    val openChannelIdF = openAndConfirmChannel(client, otherClient)

    val paymentAmount = NanoBitcoins(100000)
    val invoiceF = openChannelIdF.flatMap(_ => otherClient.receive(paymentAmount))

    val paymentF = invoiceF.flatMap(i => client.send(i))

    val isCorrectAmountF = paymentF.map { p =>
      assert(p.isInstanceOf[PaymentSucceeded])

      val pSucceed = p.asInstanceOf[PaymentSucceeded]

      assert(pSucceed.amountMsat == paymentAmount)

    }

    val closedChannelF: Future[Assertion] = isCorrectAmountF.flatMap { _ =>
      openChannelIdF.flatMap { cid =>

        val closedF = client.close(cid)

        EclairTestUtil.awaitUntilChannelClosing(otherClient, cid)

        val otherStateF = closedF.flatMap(_ => otherClient.channel(cid))

        val isClosed = otherStateF.map { channel =>
          assert(channel.state == ChannelState.CLOSING)
        }
        isClosed
      }
    }

    isCorrectAmountF.flatMap { _ =>
      closedChannelF.map { _ =>
        succeed
      }
    }

  }

  it should "check a payment" in {
    val openChannelIdF = openAndConfirmChannel(client, otherClient)

    val paymentAmount = NanoBitcoins(100000)
    val invoiceF = openChannelIdF.flatMap(_ => otherClient.receive(paymentAmount))

    val isPaid1F = invoiceF.flatMap(i => otherClient.checkPayment(Left(i)))

    val isNotPaidAssertF = isPaid1F.map(isPaid => assert(!isPaid))

    //send the payment now
    val paidF: Future[PaymentResult] = invoiceF.flatMap(i => client.send(i))

    val isPaid2F: Future[Boolean] = paidF.flatMap { p =>
      val succeed = p.asInstanceOf[PaymentSucceeded]

      otherClient.checkPayment(Right(succeed.paymentHash))
    }

    val isPaidAssertF = isPaid2F.map(isPaid => assert(isPaid))

    isNotPaidAssertF.flatMap { isNotPaid =>
      isPaidAssertF.map { isPaid =>
        succeed

      }
    }
  }

  it should "be able to send payments in both directions" in {
    val openChannelIdF = openAndConfirmChannel(client, otherClient)

    val paymentAmount = NanoBitcoins(100000)
    val invoiceF = openChannelIdF.flatMap(_ => otherClient.receive(paymentAmount))
    //send the payment now
    val paidF: Future[PaymentResult] = invoiceF.flatMap(i => client.send(i))

    val isPaidF: Future[Boolean] = paidF.flatMap { p =>
      val succeed = p.asInstanceOf[PaymentSucceeded]
      otherClient.checkPayment(Right(succeed.paymentHash))
    }

    val isPaidAssertF = isPaidF.map(isPaid => assert(isPaid))

    isPaidAssertF.flatMap { isPaid =>
      val invoice2F = openChannelIdF.flatMap(_ => client.receive(paymentAmount))
      //send the payment now
      val paid2F: Future[PaymentResult] = invoice2F.flatMap((i => otherClient.send(i)))

      val isPaid2F: Future[Boolean] = paid2F.flatMap { p =>
        assert(p.isInstanceOf[PaymentSucceeded])
        val succeed = p.asInstanceOf[PaymentSucceeded]
        client.checkPayment(Right(succeed.paymentHash))
      }

      isPaid2F.map(isPaid => assert(isPaid))
    }
  }

  private def hasConnection(client: EclairRpcClient, nodeId: NodeId): Future[Assertion] = {

    val hasPeersF = client.getPeers.map(_.nonEmpty)

    val hasPeersAssertF = hasPeersF.map(h => assert(h))

    val isConnectedF = client.isConnected(nodeId)

    val isConnectedAssertF = isConnectedF.map(isConnected => assert(isConnected))

    hasPeersAssertF.flatMap(hasPeers => isConnectedAssertF.map(isConn => isConn))
  }

  /** Checks that the given [[org.bitcoins.eclair.rpc.client.EclairRpcClient]] has the given chanId */
  private def hasChannel(client: EclairRpcClient, chanId: ChannelId): Future[Assertion] = {
    val recognizedOpenChannel: Future[Assertion] = {

      val chanResultF: Future[ChannelResult] = client.channel(chanId)

      chanResultF.map(c => assert(c.channelId == chanId))

    }

    recognizedOpenChannel
  }

  private def openAndConfirmChannel(
    client1: EclairRpcClient,
    client2: EclairRpcClient,
    amount: CurrencyUnit = Satoshis(Int64(1000000))): Future[ChannelId] = {

    val bitcoindRpc = EclairTestUtil.getBitcoindRpc(client1)

    val nodeId2F: Future[NodeId] = client2.getInfo.map(_.nodeId)

    val channelIdF: Future[ChannelId] = nodeId2F.flatMap(nid2 => client1.open(nid2, amount))

    //confirm the funding tx
    val genF = channelIdF.flatMap(_ => bitcoindRpc.generate(6))

    channelIdF.flatMap { cid =>
      genF.map { _ =>

        //wait until our peer has put the channel in the
        //NORMAL state so we can route payments to them
        EclairTestUtil.awaitUntilChannelNormal(client2, cid)

        cid

      }
    }
  }

  override def afterAll(): Unit = {
    val s1 = EclairTestUtil.shutdown(client)
    val s2 = otherClient.stop()
    Await.result(system.terminate(), 10.seconds)
  }
}
