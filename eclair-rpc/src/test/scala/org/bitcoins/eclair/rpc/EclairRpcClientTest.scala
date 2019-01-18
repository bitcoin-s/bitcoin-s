package org.bitcoins.eclair.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.ln.channel.{ChannelId, ChannelState}
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.config.{EclairAuthCredentials, EclairInstance}
import org.bitcoins.eclair.rpc.json._
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.BitcoindRpcClient
import org.bitcoins.rpc.util.AsyncUtil
import org.scalatest.{Assertion, AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.Logger

import scala.collection.immutable.VectorBuilder
import scala.concurrent._
import scala.concurrent.duration.DurationInt

class EclairRpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {

  implicit val system: ActorSystem = ActorSystem("EclairRpcClient")
  implicit val m: ActorMaterializer = ActorMaterializer.create(system)
  implicit val ec: ExecutionContext = m.executionContext
  implicit val bitcoinNp: RegTest.type = EclairRpcTestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  val bitcoindRpcClient: BitcoindRpcClient =
    BitcoindRpcTestUtil.startedBitcoindRpcClient()

  lazy val (client, otherClient) = {
    val (c1, c2) = EclairRpcTestUtil.createNodePair(Some(bitcoindRpcClient))
    clients += c1
    clients += c2
    (c1, c2)
  }

  private val clients =
    Vector.newBuilder[EclairRpcClient]

  override def beforeAll(): Unit = {
    // make sure we have enough money open channels
    bitcoindRpcClient.generate(200)
  }

  behavior of "RpcClient"

  it should "be able to open and close a channel" in {

    val changeAddrF = bitcoindRpcClient.getNewAddress()
    val result: Future[Assertion] = {
      val isOpenedF: Future[(ChannelId, Assertion)] = {
        otherClient.getInfo.flatMap { info =>
          val amt = Satoshis(Int64(100000))
          val openedChanF = client.open(info.nodeId, amt)

          openedChanF.flatMap { channelId =>
            val exists = hasChannel(client, channelId)
            exists.map(e => (channelId, e))
          }
        }
      }

      val isConfirmedF: Future[(ChannelId, Assertion)] = {
        isOpenedF.map {
          case (chanId, assertion) =>
            val _ = bitcoindRpcClient.generate(6)
            EclairRpcTestUtil.awaitUntilChannelNormal(
              client = client,
              chanId = chanId
            )

            (chanId, assertion)
        }
      }

      val isClosedF = {
        isConfirmedF.flatMap {
          case (chanId, assertion) =>
            val closedF = changeAddrF.flatMap { addr =>
              client.close(chanId, addr.scriptPubKey)
            }

            closedF.flatMap { _ =>
              EclairRpcTestUtil.awaitUntilChannelClosing(client, chanId)
              val chanF = client.channel(chanId)
              chanF.map { chan =>
                assert(chan.state == ChannelState.CLOSING)
              }
            }
        }
      }

      val closedOnChainF = {
        isClosedF.flatMap { _ =>
          changeAddrF.flatMap { addr =>
            val amountF =
              bitcoindRpcClient.getReceivedByAddress(address = addr,
                                                     minConfirmations = 0)

            amountF.map(amt => assert(amt > CurrencyUnits.zero))

          }
        }
      }

      closedOnChainF
    }

    result
  }

  it should "fail to authenticate on bad password" in {
    val goodCredentials = client.instance.authCredentials
    val badCredentials = EclairAuthCredentials("bad_password",
                                               goodCredentials.bitcoinAuthOpt,
                                               goodCredentials.port)
    val badInstance = EclairInstance(client.instance.network,
                                     client.instance.uri,
                                     client.instance.rpcUri,
                                     badCredentials)
    val badClient = new EclairRpcClient(badInstance)

    recoverToSucceededIf[RuntimeException](badClient.getInfo)
  }

  it should "be able to list an existing peer and isConnected must be true" in {
    //test assumes that a connection to a peer was made in `beforeAll`
    val otherClientNodeIdF = otherClient.getInfo.map(_.nodeId)

    otherClientNodeIdF.flatMap(nid => hasConnection(client, nid))
  }

  it should "be able to generate an invoice and get the same amount back" in {
    val amt = PicoBitcoins(10) //this is the smallest unit we can use, 1 msat
    val description = "bitcoin-s test case"
    val expiry = (System.currentTimeMillis() / 1000)

    val invoiceF = client.receive(description = description,
                                  amountMsat = amt,
                                  expirySeconds = expiry)

    val assert0: Future[Assertion] = {
      invoiceF.map { i =>
        assert(i.amount.get == amt)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry)
      }
    }

    val amt1 = NanoBitcoins.one
    val invoice1F = client.receive(description = description,
                                   amountMsat = amt1,
                                   expirySeconds = expiry)

    val assert1 = {
      invoice1F.map { i =>
        assert(i.amount.get == amt1)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry)
      }
    }

    val amt2 = MicroBitcoins.one
    val invoice2F = client.receive(description = description,
                                   amountMsat = amt2,
                                   expirySeconds = expiry)

    val assert2 = {
      invoice2F.map { i =>
        assert(i.amount.get == amt2)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry)

      }
    }

    val amt3 = MilliBitcoins.one

    val invoice3F = client.receive(description = description,
                                   amountMsat = amt3,
                                   expirySeconds = expiry)

    val assert3 = {
      invoice3F.map { i =>
        assert(i.amount.get == amt3)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry)
      }
    }

    assert0.flatMap { _ =>
      assert1.flatMap { _ =>
        assert2.flatMap(_ => assert3)
      }
    }
  }
  it should "be able to generate a payment invoice and then check that invoice" in {
    val amt = PicoBitcoins(1000) //1 satoshi
    val description = "bitcoin-s test case"
    val expiry = (System.currentTimeMillis() / 1000)

    val invoiceF = client.receive(description = description,
                                  amountMsat = amt,
                                  expirySeconds = expiry)

    val paymentRequestF: Future[PaymentRequest] = invoiceF.flatMap { i =>
      client.checkInvoice(i)
    }

    paymentRequestF.map { paymentRequest =>
      assert(paymentRequest.amount.get == amt.toMSat)
      assert(paymentRequest.timestamp == expiry)
    }
  }

  it should "open a channel, send a payment, and close the channel" in {
    val openChannelIdF = openAndConfirmChannel(client, otherClient)

    val paymentAmount = NanoBitcoins(100000)
    val invoiceF =
      openChannelIdF.flatMap(_ => otherClient.receive(paymentAmount))

    val paymentF = invoiceF.flatMap(i => client.send(i))

    val isCorrectAmountF = paymentF.map { p =>
      assert(p.isInstanceOf[PaymentSucceeded])

      val pSucceed = p.asInstanceOf[PaymentSucceeded]

      assert(pSucceed.amountMsat == paymentAmount)

    }

    val closedChannelF: Future[Assertion] = isCorrectAmountF.flatMap { _ =>
      openChannelIdF.flatMap { cid =>
        val closedF = client.close(cid)

        EclairRpcTestUtil.awaitUntilChannelClosing(otherClient, cid)

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
    val invoiceF =
      openChannelIdF.flatMap(_ => otherClient.receive(paymentAmount))

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
    val invoiceF =
      openChannelIdF.flatMap(_ => otherClient.receive(paymentAmount))
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
      val paid2F: Future[PaymentResult] =
        invoice2F.flatMap((i => otherClient.send(i)))

      val isPaid2F: Future[Boolean] = paid2F.flatMap { p =>
        assert(p.isInstanceOf[PaymentSucceeded])
        val succeed = p.asInstanceOf[PaymentSucceeded]
        client.checkPayment(Right(succeed.paymentHash))
      }

      isPaid2F.map(isPaid => assert(isPaid))
    }
  }

  it should "get all channels" in {
    client.allChannels().flatMap(_ => succeed)
  }

  it should "get all channel updates" in {
    client.allUpdates().flatMap { _ =>
      succeed
    }
  }

  it should "get a route to a node ID" in {
    val EclairNodes4(first, second, third, fourth) =
      EclairRpcTestUtil.createNodeLink(bitcoindRpcClient)
    clients ++= List(first, second, third, fourth)

    val hasRoute = () => {
      fourth.getInfo
        .flatMap(info => first.findRoute(info.nodeId))
        .map(route => route.length == 4)
        .recover {
          case err: RuntimeException
              if err.getMessage.contains("route not found") =>
            false
        }
    }

    // Eclair is a bit slow in propagating channel changes
    AsyncUtil.awaitConditionF(hasRoute, duration = 1000.millis, maxTries = 10)

    succeed
  }

  it should "get a route to an invoice" in {
    val EclairNodes4(first, second, third, fourth) =
      EclairRpcTestUtil.createNodeLink(bitcoindRpcClient)
    clients ++= List(first, second, third, fourth)

    val hasRoute = () => {
      fourth
        .receive("foo")
        .flatMap(invoice => first.findRoute(invoice))
        .map(route => route.length == 4)
        .recover {
          case err: RuntimeException
              if err.getMessage.contains("route not found") =>
            false
        }
    }

    // Eclair is a bit slow in propagating channel changes
    AsyncUtil.awaitConditionF(hasRoute, duration = 1000.millis, maxTries = 10)

    succeed
  }

  // We spawn fresh clients in this test because the test
  // needs nodes with activity both related and not related
  // to them
  it should "get all channel updates for a given node ID" in {
    val (firstFreshClient, secondFreshClient) =
      EclairRpcTestUtil.createNodePair(Some(bitcoindRpcClient))
    val (thirdFreshClient, fourthFreshClient) =
      EclairRpcTestUtil.createNodePair(Some(bitcoindRpcClient))

    clients ++= List(firstFreshClient,
                     secondFreshClient,
                     thirdFreshClient,
                     fourthFreshClient)

    EclairRpcTestUtil.connectLNNodes(firstFreshClient, thirdFreshClient)
    EclairRpcTestUtil.connectLNNodes(firstFreshClient, fourthFreshClient)

    def block[T](fut: Future[T]): T = Await.result(fut, 60.seconds)

    def openChannel(c1: EclairRpcClient, c2: EclairRpcClient) = {
      block(
        EclairRpcTestUtil
          .openChannel(c1, c2, Satoshis(Int64(500000)), MilliSatoshis(500000)))
    }

    openChannel(firstFreshClient, secondFreshClient)
    openChannel(thirdFreshClient, fourthFreshClient)

    block(bitcoindRpcClient.generate(10))

    block(EclairRpcTestUtil.sendPayments(firstFreshClient, secondFreshClient))
    block(EclairRpcTestUtil.sendPayments(thirdFreshClient, fourthFreshClient))

    def updateIsInChannels(channels: Seq[OpenChannelInfo])(
        update: ChannelUpdate): Boolean = {
      channels.exists(_.shortChannelId == update.shortChannelId)
    }

    def updateIsNotInChannels(channels: Seq[OpenChannelInfo])(
        update: ChannelUpdate): Boolean =
      !updateIsInChannels(channels)(update)

    for {
      nodeId <- secondFreshClient.getInfo.map(_.nodeId)
      ourOpenChannels <- firstFreshClient
        .channels(nodeId)
        .map(_.collect {
          case open: OpenChannelInfo => open
        })

      ourChannelUpdates <- firstFreshClient.allUpdates(nodeId)
      allChannelUpdates <- firstFreshClient.allUpdates()
    } yield {
      assert(ourChannelUpdates.forall(updateIsInChannels(ourOpenChannels)))
      assert(allChannelUpdates.exists(updateIsNotInChannels(ourOpenChannels)))

      succeed
    }
  }

  private def hasConnection(
      client: EclairRpcClient,
      nodeId: NodeId): Future[Assertion] = {

    val hasPeersF = client.getPeers.map(_.nonEmpty)

    val hasPeersAssertF = hasPeersF.map(h => assert(h))

    val isConnectedF = client.isConnected(nodeId)

    val isConnectedAssertF =
      isConnectedF.map(isConnected => assert(isConnected))

    hasPeersAssertF.flatMap(hasPeers =>
      isConnectedAssertF.map(isConn => isConn))
  }

  /** Checks that the given [[org.bitcoins.eclair.rpc.client.EclairRpcClient]] has the given chanId */
  private def hasChannel(
      client: EclairRpcClient,
      chanId: ChannelId): Future[Assertion] = {
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

    val bitcoindRpc = EclairRpcTestUtil.getBitcoindRpc(client1)

    val nodeId2F: Future[NodeId] = client2.getInfo.map(_.nodeId)

    val channelIdF: Future[ChannelId] =
      nodeId2F.flatMap(nid2 => client1.open(nid2, amount))

    //confirm the funding tx
    val genF = channelIdF.flatMap(_ => bitcoindRpc.generate(6))

    channelIdF.flatMap { cid =>
      genF.map { _ =>
        //wait until our peer has put the channel in the
        //NORMAL state so we can route payments to them
        EclairRpcTestUtil.awaitUntilChannelNormal(client2, cid)

        cid

      }
    }
  }

  override def afterAll(): Unit = {
    clients.result().foreach(EclairRpcTestUtil.shutdown)
    TestKit.shutdownActorSystem(system)
  }
}
