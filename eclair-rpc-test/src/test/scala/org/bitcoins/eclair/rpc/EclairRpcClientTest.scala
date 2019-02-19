package org.bitcoins.eclair.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.ln.LnParams.LnBitcoinRegTest
import org.bitcoins.core.protocol.ln.channel.{
  ChannelId,
  ChannelState,
  FundedChannelId
}
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

import scala.concurrent._
import scala.concurrent.duration.DurationInt

class EclairRpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {

  implicit val system: ActorSystem = ActorSystem("EclairRpcClient")
  implicit val m: ActorMaterializer = ActorMaterializer.create(system)
  implicit val ec: ExecutionContext = m.executionContext
  implicit val bitcoinNp: RegTest.type = EclairRpcTestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  val bitcoindRpcClientF: Future[BitcoindRpcClient] = {
    val cliF = BitcoindRpcTestUtil.startedBitcoindRpcClient()
    // make sure we have enough money open channels
    //not async safe
    val blocksF = cliF.flatMap(_.generate(200))

    blocksF.flatMap(_ => cliF)
  }

  val eclairNodesF: Future[EclairNodes4] = {
    bitcoindRpcClientF.flatMap { bitcoindRpcClient =>
      val nodesF = EclairRpcTestUtil.createNodeLink(bitcoindRpcClient)

      val addedF = nodesF.map { nodes =>
        clients ++= List(nodes.c1, nodes.c2, nodes.c3, nodes.c4)
      }

      addedF.flatMap(_ => nodesF)
    }
  }

  lazy val firstClientF = eclairNodesF.map(_.c1)

  lazy val secondClientF = eclairNodesF.map(_.c2)

  lazy val thirdClientF = eclairNodesF.map(_.c3)

  lazy val fourthClientF = eclairNodesF.map(_.c4)

  /** There is specific cases where we just need two clients,
    * so this is a helper val that pairs two connected
    * clients together with an open channel
    */
  lazy val clientOtherClientF = {

    //use second and third client above since they
    //aren't really being used in the tests that use eclairNodesF
    secondClientF.flatMap(s => thirdClientF.map(t => (s, t)))
  }

  lazy val clientF = clientOtherClientF.map(_._1)
  lazy val otherClientF = clientOtherClientF.map(_._2)

  private val clients =
    Vector.newBuilder[EclairRpcClient]

  /** Executes a test with the default clients defined at the
    * top of this file
    * @param test
    * @tparam T
    * @return
    */
  def executeWithClientOtherClient[T](
      test: (EclairRpcClient, EclairRpcClient) => Future[T]): Future[T] = {

    executeSpecificClients(clientF = clientF,
                           otherClientF = otherClientF,
                           test = test)
  }

  /** Executes the test with the clients passed as a parameter */
  def executeSpecificClients[T](
      clientF: Future[EclairRpcClient],
      otherClientF: Future[EclairRpcClient],
      test: (EclairRpcClient, EclairRpcClient) => Future[T]): Future[T] = {
    clientF.flatMap { c1 =>
      otherClientF.flatMap { other =>
        test(c1, other)
      }
    }
  }

  behavior of "RpcClient"

  it should "be able to open and close a channel" in {

    val changeAddrF = bitcoindRpcClientF.flatMap(_.getNewAddress())
    val result: Future[Assertion] = {
      val isOpenedF: Future[(ChannelId, Assertion)] = {
        val getChannelId =
          (client: EclairRpcClient, otherClient: EclairRpcClient) => {
            otherClient.getInfo.flatMap { info =>
              val amt = Satoshis(Int64(100000))
              val openedChanF = clientF.flatMap(_.open(info.nodeId, amt))

              openedChanF.flatMap { channelId =>
                val exists = hasChannel(client, channelId)
                exists.map(e => (channelId, e))
              }
            }
          }
        executeWithClientOtherClient[(ChannelId, Assertion)](getChannelId)
      }

      val isConfirmedF: Future[(ChannelId, Assertion)] = {
        val getIsConfirmed = { (client: EclairRpcClient, _: EclairRpcClient) =>
          isOpenedF.flatMap {
            case (chanId, assertion) =>
              val generatedF = bitcoindRpcClientF.flatMap(_.generate(6))
              val normalF = generatedF.flatMap { _ =>
                EclairRpcTestUtil.awaitUntilChannelNormal(
                  client = client,
                  chanId = chanId
                )
              }

              normalF.map(_ => (chanId, assertion))
          }
        }

        executeWithClientOtherClient(getIsConfirmed)
      }

      val isClosedF = {
        val getIsClosed = { (client: EclairRpcClient, _: EclairRpcClient) =>
          isConfirmedF.flatMap {
            case (chanId, assertion) =>
              val closedF = changeAddrF.flatMap { addr =>
                val closedF = client.close(chanId, addr.scriptPubKey)

                closedF.flatMap { _ =>
                  EclairRpcTestUtil.awaitUntilChannelClosing(client, chanId)
                }
              }

              closedF.flatMap { _ =>
                val chanF = client.channel(chanId)
                chanF.map { chan =>
                  assert(chan.state == ChannelState.CLOSING)
                }
              }
          }
        }

        executeWithClientOtherClient(getIsClosed)
      }

      val closedOnChainF = {
        isClosedF.flatMap { _ =>
          changeAddrF.flatMap { addr =>
            val amountF =
              bitcoindRpcClientF.flatMap { bitcoindRpcClient =>
                bitcoindRpcClient.getReceivedByAddress(address = addr,
                                                       minConfirmations = 0)
              }
            amountF.map(amt => assert(amt > CurrencyUnits.zero))
          }
        }
      }

      closedOnChainF
    }

    result
  }

  it should "fail to authenticate on bad password" in {
    val goodCredentialsF = {
      val getAuthCredentials = {
        (client: EclairRpcClient, _: EclairRpcClient) =>
          Future.successful(client.instance.authCredentials)
      }
      executeWithClientOtherClient[EclairAuthCredentials](getAuthCredentials)
    }

    val badCredentialsF = goodCredentialsF.map { good =>
      EclairAuthCredentials("bad_password", good.bitcoinAuthOpt, good.port)
    }

    val badInstanceF = badCredentialsF.flatMap { badCredentials =>
      val getBadInstance = (client: EclairRpcClient, _: EclairRpcClient) => {
        val instance = EclairInstance(network = client.instance.network,
                                      uri = client.instance.uri,
                                      rpcUri = client.instance.rpcUri,
                                      authCredentials = badCredentials)

        Future.successful(instance)
      }

      executeWithClientOtherClient(getBadInstance)
    }

    val badClientF = badInstanceF.map(new EclairRpcClient(_))

    badClientF.flatMap { badClient =>
      recoverToSucceededIf[RuntimeException](badClient.getInfo)
    }
  }

  it should "be able to list an existing peer and isConnected must be true" in {
    //test assumes that a connection to a peer was made in `beforeAll`
    val otherClientNodeIdF = {
      val getOtherClientNodeId = {
        (_: EclairRpcClient, otherClient: EclairRpcClient) =>
          otherClient.getInfo.map(_.nodeId)
      }

      executeWithClientOtherClient(getOtherClientNodeId)
    }

    otherClientNodeIdF.flatMap(nid => hasConnection(clientF, nid))
  }

  it should "ble able to pay to a hash" in {
    val amt = MilliSatoshis(50)
    val getPayment = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        {
          for {
            channelId <- openAndConfirmChannel(clientF, otherClientF)
            otherClientNodeId <- otherClient.getInfo.map(_.nodeId)
            channels <- client.channels(otherClientNodeId)
            // without this we've been getting "route not found"
            // probably an async issue, this is more elegant than Thread.sleep
            _ = assert(channels.exists(_.state == ChannelState.NORMAL),
                       "Nodes did not have open channel!")
            invoice <- otherClient.receive(amt.toLnCurrencyUnit)
            payment <- client.send(amt.toLnCurrencyUnit,
                                   invoice.lnTags.paymentHash.hash,
                                   otherClientNodeId)
            _ <- client.close(channelId)
            _ <- bitcoindRpcClientF.flatMap(_.generate(6))
          } yield {
            assert(payment.isInstanceOf[PaymentSucceeded])
            val succeeded = payment.asInstanceOf[PaymentSucceeded]
            assert(succeeded.amountMsat == amt)
          }
        }
    }

    executeWithClientOtherClient(getPayment)

  }

  it should "be able to generate an invoice with amount and pay it" in {
    val amt = MilliSatoshis(50)

    val getPaymentWithAmount = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        {
          for {
            channelId <- openAndConfirmChannel(clientF, otherClientF)
            invoice <- otherClient.receive(amt.toLnCurrencyUnit)
            payment <- client.send(invoice)
            _ <- client.close(channelId)
            _ <- bitcoindRpcClientF.flatMap(_.generate(6))
          } yield {
            assert(payment.isInstanceOf[PaymentSucceeded])
            val succeeded = payment.asInstanceOf[PaymentSucceeded]
            assert(succeeded.amountMsat == amt)
          }
        }
    }

    executeWithClientOtherClient(getPaymentWithAmount)
  }

  it should "be able to generate an invoice without amount and pay it" in {
    val amt = MilliSatoshis(50)
    val getPaymentNoAmount = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        {
          for {
            channelId <- openAndConfirmChannel(clientF, otherClientF)
            invoice <- otherClient.receive("no amount")
            payment <- client.send(invoice, amt.toLnCurrencyUnit)
            _ <- client.close(channelId)
            _ <- bitcoindRpcClientF.flatMap(_.generate(6))
          } yield {
            assert(payment.isInstanceOf[PaymentSucceeded])
            val succeeded = payment.asInstanceOf[PaymentSucceeded]
            assert(succeeded.amountMsat == amt)
          }
        }
    }

    executeWithClientOtherClient(getPaymentNoAmount)
  }

  it should "be able to generate an invoice and get the same amount back" in {
    val amt = PicoBitcoins(10) //this is the smallest unit we can use, 1 msat
    val description = "bitcoin-s test case"
    val expiry = (System.currentTimeMillis() / 1000)

    val invoiceF = clientF.flatMap(
      _.receive(description = description,
                amountMsat = amt,
                expirySeconds = expiry))

    val assert0: Future[Assertion] = {
      invoiceF.map { i =>
        assert(i.amount.get == amt)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry)
      }
    }

    val amt1 = NanoBitcoins.one
    val invoice1F = clientF.flatMap(
      _.receive(description = description,
                amountMsat = amt1,
                expirySeconds = expiry))

    val assert1 = {
      invoice1F.map { i =>
        assert(i.amount.get == amt1)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry)
      }
    }

    val amt2 = MicroBitcoins.one
    val invoice2F = clientF.flatMap(
      _.receive(description = description,
                amountMsat = amt2,
                expirySeconds = expiry))

    val assert2 = {
      invoice2F.map { i =>
        assert(i.amount.get == amt2)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry)

      }
    }

    val amt3 = MilliBitcoins.one

    val invoice3F = clientF.flatMap(
      _.receive(description = description,
                amountMsat = amt3,
                expirySeconds = expiry))

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

    val invoiceF = clientF.flatMap(
      _.receive(description = description,
                amountMsat = amt,
                expirySeconds = expiry))

    val paymentRequestF: Future[PaymentRequest] = invoiceF.flatMap { i =>
      clientF.flatMap(_.checkInvoice(i))
    }

    paymentRequestF.map { paymentRequest =>
      assert(paymentRequest.amount.get == amt.toMSat)
      assert(paymentRequest.timestamp == expiry)
    }
  }

  it should "open a channel, send a payment, and close the channel" in {
    val openChannelIdF = openAndConfirmChannel(clientF, otherClientF)

    val paymentAmount = NanoBitcoins(100000)
    val invoiceF =
      openChannelIdF.flatMap(_ =>
        otherClientF.flatMap(_.receive(paymentAmount)))

    val paymentF = invoiceF.flatMap(i => clientF.flatMap(_.send(i)))

    val isCorrectAmountF = paymentF.map { p =>
      assert(p.isInstanceOf[PaymentSucceeded])

      val pSucceed = p.asInstanceOf[PaymentSucceeded]

      assert(pSucceed.amountMsat == paymentAmount)

    }

    val closedChannelF: Future[Assertion] = isCorrectAmountF.flatMap { _ =>
      openChannelIdF.flatMap { cid =>
        val closedF = clientF.flatMap(_.close(cid))

        val otherClientClosedF = otherClientF.flatMap { otherClient =>
          closedF.flatMap { _ =>
            EclairRpcTestUtil.awaitUntilChannelClosing(otherClient, cid)
          }
        }

        val otherStateF = otherClientClosedF.flatMap { _ =>
          otherClientF.flatMap(_.channel(cid))
        }

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
    val openChannelIdF = openAndConfirmChannel(clientF, otherClientF)

    val paymentAmount = NanoBitcoins(100000)
    val invoiceF =
      openChannelIdF.flatMap(_ =>
        otherClientF.flatMap(_.receive(paymentAmount)))

    val isPaid1F =
      invoiceF.flatMap(i => otherClientF.flatMap(_.checkPayment(Left(i))))

    val isNotPaidAssertF = isPaid1F.map(isPaid => assert(!isPaid))

    //send the payment now
    val paidF: Future[PaymentResult] =
      invoiceF.flatMap(i => clientF.flatMap(_.send(i)))

    val isPaid2F: Future[Boolean] = paidF.flatMap { p =>
      val succeed = p.asInstanceOf[PaymentSucceeded]

      otherClientF.flatMap(_.checkPayment(Right(succeed.paymentHash)))
    }

    val isPaidAssertF = isPaid2F.map(isPaid => assert(isPaid))

    isNotPaidAssertF.flatMap { isNotPaid =>
      isPaidAssertF.map { isPaid =>
        succeed

      }
    }
  }

  it should "be able to send payments in both directions" in {
    val openChannelIdF = openAndConfirmChannel(clientF, otherClientF)

    val paymentAmount = NanoBitcoins(100000)
    val invoiceF =
      openChannelIdF.flatMap(_ =>
        otherClientF.flatMap(_.receive(paymentAmount)))
    //send the payment now
    val paidF: Future[PaymentResult] =
      invoiceF.flatMap(i => clientF.flatMap(_.send(i)))

    val isPaidF: Future[Boolean] = paidF.flatMap { p =>
      val succeed = p.asInstanceOf[PaymentSucceeded]
      otherClientF.flatMap(_.checkPayment(Right(succeed.paymentHash)))
    }

    val isPaidAssertF = isPaidF.map(isPaid => assert(isPaid))

    isPaidAssertF.flatMap { isPaid =>
      val invoice2F =
        openChannelIdF.flatMap(_ => clientF.flatMap(_.receive(paymentAmount)))
      //send the payment now
      val paid2F: Future[PaymentResult] =
        invoice2F.flatMap((i => otherClientF.flatMap(_.send(i))))

      val isPaid2F: Future[Boolean] = paid2F.flatMap { p =>
        assert(p.isInstanceOf[PaymentSucceeded])
        val succeed = p.asInstanceOf[PaymentSucceeded]
        clientF.flatMap(_.checkPayment(Right(succeed.paymentHash)))
      }

      isPaid2F.map(isPaid => assert(isPaid))
    }
  }

  it should "update the relay fee of a channel" in {
    val channelAndFeeF = for {
      channel <- openAndConfirmChannel(clientF, otherClientF)
      feeOpt <- clientF.flatMap(_.channel(channel).map(_.feeBaseMsat))
    } yield {
      assert(feeOpt.isDefined)
      assert(feeOpt.get > MilliSatoshis.zero)
      (channel, feeOpt.get)
    }

    for {
      (channel, oldFee) <- channelAndFeeF
      _ <- clientF.flatMap(
        _.updateRelayFee(channel, MilliSatoshis(oldFee.toLong * 2), 1))
      newFeeOpt <- clientF.flatMap(_.channel(channel).map(_.feeBaseMsat))
    } yield {
      assert(newFeeOpt.isDefined)
      assert(newFeeOpt.get == MilliSatoshis(oldFee.toLong * 2))
    }
  }

  it should "get all channels" in {
    clientF.flatMap(_.allChannels().flatMap(_ => succeed))
  }

  it should "get all channel updates" in {
    clientF.flatMap(_.allUpdates().flatMap { _ =>
      succeed
    })
  }

  it should "get a route to a node ID" in {
    val hasRoute = () => {
      fourthClientF
        .flatMap(_.getInfo)
        .flatMap(info => firstClientF.flatMap(_.findRoute(info.nodeId)))
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
    val hasRoute = () => {
      fourthClientF.flatMap { fourthClient =>
        fourthClient
          .receive("foo")
          .flatMap { invoice =>
            firstClientF.flatMap(_.findRoute(invoice))
          }
          .map(route => route.length == 4)
          .recover {
            case err: RuntimeException
                if err.getMessage.contains("route not found") =>
              false
          }
      }

    }

    // Eclair is a bit slow in propagating channel changes
    AsyncUtil.awaitConditionF(hasRoute, duration = 1000.millis, maxTries = 10)

    succeed
  }

  it should "send some payments and get the audit info" in {
    for {
      invoice <- fourthClientF.flatMap(
        _.receive(MilliSatoshis(50000).toLnCurrencyUnit))
      _ <- firstClientF.flatMap(
        _.send(invoice)
          .map(payment => assert(payment.isInstanceOf[PaymentSucceeded])))

      received <- fourthClientF
        .flatMap(_.audit())
        .map(_.received) // check for received payments
      relayed <- secondClientF
        .flatMap(_.audit())
        .map(_.relayed) // check for relayed payments
      sent <- firstClientF
        .flatMap(_.audit())
        .map(_.sent) // check for sent payments
    } yield {
      assert(received.nonEmpty)
      assert(relayed.nonEmpty)
      assert(sent.nonEmpty)
    }
  }

  // We spawn fresh clients in this test because the test
  // needs nodes with activity both related and not related
  // to them
  it should "get all channel updates for a given node ID" in {
    val freshClients1F = bitcoindRpcClientF.flatMap { bitcoindRpcClient =>
      EclairRpcTestUtil.createNodePair(Some(bitcoindRpcClient))
    }

    val freshClients2F = bitcoindRpcClientF.flatMap { _ =>
      thirdClientF.flatMap(t => fourthClientF.map(f => (t, f)))
    }

    val connectedClientsF: Future[EclairNodes4] = {
      freshClients1F.flatMap {
        case (freshClient1, freshClient2) =>
          freshClients2F.flatMap {
            case (freshClient3, freshClient4) =>
              clients ++= List(freshClient1,
                               freshClient2,
                               freshClient3,
                               freshClient4)

              val connect1And3 =
                EclairRpcTestUtil.connectLNNodes(freshClient1, freshClient3)
              val connect1And4 = connect1And3.flatMap(_ =>
                EclairRpcTestUtil.connectLNNodes(freshClient1, freshClient4))
              connect1And3.flatMap { _ =>
                connect1And4.map { _ =>
                  EclairNodes4(freshClient1,
                               freshClient2,
                               freshClient3,
                               freshClient4)
                }
              }
          }
      }
    }

    def openChannel(
        c1: EclairRpcClient,
        c2: EclairRpcClient): Future[FundedChannelId] = {
      EclairRpcTestUtil
        .openChannel(c1, c2, Satoshis(Int64(500000)), MilliSatoshis(500000))
    }

    val openedChannelsF: Future[(ChannelId, ChannelId)] = {
      connectedClientsF.flatMap {
        case nodes4: EclairNodes4 =>
          val chan1F = openChannel(nodes4.c1, nodes4.c2)
          val chan2F = openChannel(nodes4.c3, nodes4.c4)
          chan1F.flatMap(chanId1 => chan2F.map(chanId2 => (chanId1, chanId2)))
      }
    }

    val gen10F =
      openedChannelsF.flatMap(_ => bitcoindRpcClientF.flatMap(_.generate(10)))

    val nodesReadyForPayments = gen10F.flatMap(_ => connectedClientsF)

    val sendPaymentsF = nodesReadyForPayments.flatMap { n4 =>
      val p1F =
        EclairRpcTestUtil.sendPayments(c1 = n4.c1, c2 = n4.c2, numPayments = 2)
      val p2F =
        EclairRpcTestUtil.sendPayments(c1 = n4.c3, c2 = n4.c4, numPayments = 2)

      p1F.flatMap(_ => p2F)
    }

    val getChannelUpdates = {
      (firstFreshClient: EclairRpcClient, secondFreshClient: EclairRpcClient) =>
        for {
          nodeId <- secondFreshClient.getInfo.map(_.nodeId)
          ourOpenChannels <- firstFreshClient
            .channels(nodeId)
            .map(_.collect {
              case open: OpenChannelInfo => open
            })

          ourChannelUpdates <- firstFreshClient.allUpdates(nodeId)
        } yield {
          assert(ourChannelUpdates.forall(updateIsInChannels(ourOpenChannels)))

          succeed
        }
    }

    val client1F = connectedClientsF.map(_.c1)
    val client2F = connectedClientsF.map(_.c2)

    sendPaymentsF.flatMap { _ =>
      executeSpecificClients(clientF = client1F,
                             otherClientF = client2F,
                             test = getChannelUpdates)
    }

  }

  it must "receive gossip messages about channel updates for nodes we do not have a direct channel with" in {
    //make sure we see payments outside of our immediate peers
    //this is important because these gossip messages contain
    //information about channel fees, so we need to get updates
    //about the channels for future fee calculations
    val sentPaymentF = secondClientF.flatMap { c1 =>
      thirdClientF.flatMap { c2 =>
        EclairRpcTestUtil.sendPayments(c1, c2)
      }
    }

    val gossipFromPeerWithNoChannel = {
      (client: EclairRpcClient, nonPeer: EclairRpcClient) =>
        sentPaymentF.flatMap { _ =>
          val nodeIdF = client.getNodeURI.map(_.nodeId)
          def ourUpdates = nodeIdF.flatMap(nonPeer.allUpdates(_))
          def allUpdates = nonPeer.allUpdates()
          def checkUpdates(): Future[Boolean] = {
            ourUpdates.flatMap(our =>
              allUpdates.map { all =>
                our != all
            })
          }

          val checkedUpatesF: Future[Unit] =
            AsyncUtil.retryUntilSatisfiedF(checkUpdates,
                                           duration = 5.seconds,
                                           maxTries = 15)

          val hasUpdateP = Promise[Assertion]()
          checkedUpatesF.onComplete { t =>
            hasUpdateP.success(assert(t.isSuccess))
          }

          hasUpdateP.future
        }
    }

    //the second client and fourth client aren't directly connected
    //which is why i am choosing to use them for this test
    executeSpecificClients(
      clientF = secondClientF,
      otherClientF = fourthClientF,
      test = gossipFromPeerWithNoChannel
    )
  }

  it should "detect what network we are on" in {

    clientF.map(c => assert(c.network == LnBitcoinRegTest))
  }

  private def hasConnection(
      client: Future[EclairRpcClient],
      nodeId: NodeId): Future[Assertion] = {

    val hasPeersF = clientF.flatMap(_.getPeers.map(_.nonEmpty))

    val hasPeersAssertF = hasPeersF.map(h => assert(h))

    val isConnectedF = clientF.flatMap(_.isConnected(nodeId))

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
      client1F: Future[EclairRpcClient],
      client2F: Future[EclairRpcClient],
      amount: CurrencyUnit = Satoshis(Int64(1000000))): Future[ChannelId] = {

    val bitcoindRpcF = client1F.map(EclairRpcTestUtil.getBitcoindRpc(_))

    val nodeId2F: Future[NodeId] = client2F.flatMap(_.getInfo.map(_.nodeId))

    val channelIdF: Future[ChannelId] = {
      nodeId2F.flatMap { nid2 =>
        client1F.flatMap(_.open(nid2, amount))
      }
    }

    //confirm the funding tx
    val genF = channelIdF.flatMap { _ =>
      bitcoindRpcF.flatMap(_.generate(6))
    }

    channelIdF.flatMap { cid =>
      genF.flatMap { _ =>
        //wait until our peer has put the channel in the
        //NORMAL state so we can route payments to them
        val normalF = client2F.flatMap(c2 =>
          EclairRpcTestUtil.awaitUntilChannelNormal(c2, cid))

        normalF.map(_ => cid)

      }
    }
  }

  private def updateIsInChannels(channels: Seq[OpenChannelInfo])(
      update: ChannelUpdate): Boolean = {
    channels.exists(_.shortChannelId == update.shortChannelId)
  }

  override def afterAll(): Unit = {
    clients.result().foreach(EclairRpcTestUtil.shutdown)
    TestKit.shutdownActorSystem(system)
  }
}
