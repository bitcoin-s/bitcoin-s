package org.bitcoins.eclair.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int64, UInt64}
import org.bitcoins.core.protocol.ln.LnParams.LnBitcoinRegTest
import org.bitcoins.core.protocol.ln.channel.{ChannelId, ChannelState, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.config.{EclairAuthCredentials, EclairInstance}
import org.bitcoins.eclair.rpc.json._
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.util.AsyncUtil
import org.bitcoins.testkit.eclair.rpc.{EclairNodes4, EclairRpcTestUtil}
import org.scalatest.{Assertion, AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.Logger

import scala.concurrent._
import scala.concurrent.duration.DurationInt
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import akka.stream.StreamTcpException
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ln.{LnHumanReadablePart, LnInvoice, PaymentPreimage}
import org.bitcoins.testkit.async.TestAsyncUtil

import scala.concurrent.duration._

class EclairRpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {

  implicit val system: ActorSystem =
    ActorSystem("EclairRpcClient", BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val m: ActorMaterializer = ActorMaterializer.create(system)
  implicit val ec: ExecutionContext = m.executionContext
  implicit val bitcoinNp: RegTest.type = EclairRpcTestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  lazy val bitcoindRpcClientF: Future[BitcoindRpcClient] = {
    val cliF = EclairRpcTestUtil.startedBitcoindRpcClient()
    // make sure we have enough money open channels
    //not async safe
    val blocksF = cliF.flatMap(_.generate(200))

    blocksF.flatMap(_ => cliF)
  }

  lazy val eclairNodesF: Future[EclairNodes4] = {
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
    *
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

  /**
    * Please keep this test the very first. All other tests rely on the propagated gossip messages.
    */
  it should "wait for all gossip messages get propagated throughout the network and get a route to an invoice" in {
    val invoiceF = fourthClientF.flatMap(_.createInvoice("foo", 1000.msats))
    val hasRoute = () => {
      invoiceF.flatMap { invoice =>
        firstClientF
          .flatMap(_.findRoute(invoice, None))
          .map(route => route.length == 4)
          .recover {
            case err: RuntimeException
                if err.getMessage.contains("route not found") =>
              false
          }
      }

    }

    // Eclair is a bit slow in propagating channel changes
    AsyncUtil
      .awaitConditionF(hasRoute, duration = 10.seconds, maxTries = 20)
      .map(_ => succeed)
  }

  it should "send some payments and get the audit info" in {
    for {
      client1 <- firstClientF
      client2 <- secondClientF
      client4 <- fourthClientF
      invoice <- client4.createInvoice("test", 1000.msats)
      paymentId <- client1.payInvoice(invoice)
      _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client1,
                                                        paymentId,
                                                        duration = 5.seconds)
      received <- client4.audit()
      relayed <- client2.audit()
      sent <- client1.audit()
    } yield {
      assert(sent.sent.nonEmpty)
      assert(received.received.nonEmpty)
      assert(relayed.relayed.nonEmpty)
    }
  }

  it should "get a route to a node ID" in {
    val hasRoute = () => {
      fourthClientF
        .flatMap(_.getInfo)
        .flatMap(info =>
          firstClientF.flatMap(_.findRoute(info.nodeId, MilliSatoshis(100))))
        .map(route => route.length == 4)
        .recover {
          case err: RuntimeException
              if err.getMessage.contains("route not found") =>
            false
        }
    }

    // Eclair is a bit slow in propagating channel changes
    AsyncUtil
      .awaitConditionF(hasRoute, duration = 10.seconds, maxTries = 20)
      .map(_ => succeed)
  }

  it should "pay an invoice and monitor the payment" in {
    val checkPayment = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        for {
          _ <- openAndConfirmChannel(clientF, otherClientF)
          invoice <- otherClient.createInvoice("abc", 50.msats)
          paymentResult <- client.payAndMonitorInvoice(invoice, 1.second, 10)
        } yield {
          assert(paymentResult.amountMsat == 50.msats)
        }
    }

    executeWithClientOtherClient(checkPayment)
  }

  it should "check a payment" in {
    val checkPayment = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        for {
          _ <- openAndConfirmChannel(clientF, otherClientF)
          invoice <- otherClient.createInvoice("abc", 50.msats)
          infos <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
          _ = assert(infos.isEmpty)
          paymentId <- client.payInvoice(invoice)
          _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
          sentInfo <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
        } yield {
          assert(sentInfo.head.amountMsat == 50.msats)
        }
    }

    executeWithClientOtherClient(checkPayment)
  }

  it should "be able to create an empty invoice" in {
    for {
      c <- clientF
      invoice <- c.createInvoice("test")
    } yield {
      assert(invoice.hrp.toString == LnHumanReadablePart.lnbcrt.toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount.isEmpty)
      assert(invoice.isValidSignature)
      assert(invoice.timestamp > UInt64(1561063731)) // this is when I wrote this code
      assert(invoice.timestamp <= UInt64(System.currentTimeMillis() / 1000))
    }
  }

  it should "be able to create an invoice with amount" in {
    for {
      c <- clientF
      invoice <- c.createInvoice(description = "test", amountMsat = 12345.msats)
    } yield {
      assert(
        invoice.hrp.toString == LnHumanReadablePart
          .lnbcrt(Some(12345.msats.toLnCurrencyUnit))
          .toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount == Some(123450.pBTC))
      assert(invoice.isValidSignature)
      assert(invoice.timestamp > UInt64(1561063731)) // this is when I wrote this code
      assert(invoice.timestamp <= UInt64(System.currentTimeMillis() / 1000))
    }
  }

  it should "be able to create an invoice with amount and expiry time" in {
    for {
      c <- clientF
      invoice <- c.createInvoice(description = "test",
                                 amountMsat = 12345.msats,
                                 expireIn = 67890.seconds)
    } yield {
      assert(
        invoice.hrp.toString == LnHumanReadablePart
          .lnbcrt(Some(12345.msats.toLnCurrencyUnit))
          .toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount == Some(123450.pBTC))
      assert(invoice.isValidSignature)
      assert(invoice.timestamp > UInt64(1561063731)) // this is when I wrote this code
      assert(invoice.timestamp <= UInt64(System.currentTimeMillis() / 1000))
    }
  }

  val testBitcoinAddress = BitcoinAddress("n3p1ct69ao3qxWvEvzLhLtWG2zJGTjN3EV").get

  it should "be able to create an invoice with amount, expiry time, and fallbackAddress" in {
    for {
      c <- clientF
      invoice <- c.createInvoice(description = "test",
                                 amountMsat = Some(12345.msats),
                                 expireIn = Some(67890.seconds),
                                 fallbackAddress = Some(testBitcoinAddress),
                                 paymentPreimage = None)
    } yield {
      assert(
        invoice.hrp.toString == LnHumanReadablePart
          .lnbcrt(Some(123450.pBTC))
          .toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount == Some(123450.pBTC))
      assert(invoice.isValidSignature)
      assert(invoice.timestamp > UInt64(1561063731)) // this is when I wrote this code
      assert(invoice.timestamp <= UInt64(System.currentTimeMillis() / 1000))
    }
  }

  val testPaymentPreimage = PaymentPreimage.fromHex(
    "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff")

  it should "be able to create an invoice with amount, expiry time, fallbackAddress, and preimage" in {
    for {
      c <- clientF
      invoice <- c.createInvoice(
        description = "test",
        amountMsat = Some(12345.msats),
        expireIn = Some(67890.seconds),
        fallbackAddress = Some(testBitcoinAddress),
        paymentPreimage = Some(testPaymentPreimage)
      )
    } yield {
      assert(
        invoice.hrp.toString == LnHumanReadablePart
          .lnbcrt(Some(12345.msats.toLnCurrencyUnit))
          .toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount == Some(123450.pBTC))
      assert(invoice.isValidSignature)
      assert(invoice.timestamp > UInt64(1561063731)) // this is when I wrote this code
      assert(invoice.timestamp <= UInt64(System.currentTimeMillis() / 1000))
    }
  }

  it should "be able to start and shutdown a node" in {
    for {
      bitcoind <- EclairRpcTestUtil.startedBitcoindRpcClient()
      eclair <- {
        val server = EclairRpcTestUtil.eclairInstance(bitcoind)
        val eclair = new EclairRpcClient(server)
        eclair.start().map(_ => eclair)
      }
      _ <- TestAsyncUtil.retryUntilSatisfiedF(conditionF = () => eclair.isStarted(),
        duration = 1.second,
        maxTries = 60)
      _ = EclairRpcTestUtil.shutdown(eclair)
      _ <- TestAsyncUtil.retryUntilSatisfiedF(conditionF = () => eclair.isStarted().map(!_),
        duration = 1.second,
        maxTries = 60)
    } yield succeed
  }
  it should "be able to open and close a channel" in {

    val changeAddrF = bitcoindRpcClientF.flatMap(_.getNewAddress)
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
            case (chanId, _) =>
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
      EclairAuthCredentials("bad_password",
                            good.bitcoinAuthOpt,
                            rpcPort = good.rpcPort,
                            bitcoindRpcUri = good.bitcoindRpcUri)
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

  it should "be able to pay to a route" in {
    val amt = 50.msats
    val getPayment = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        {
          for {
            otherClientNodeId <- otherClient.getInfo.map(_.nodeId)
            invoice <- otherClient.createInvoice("foo", amt)
            route <- client.findRoute(otherClientNodeId, amt)
            paymentId <- client.sendToRoute(route,
                                            amt,
                                            invoice.lnTags.paymentHash.hash,
                                            144)
            _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
            succeeded <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
          } yield {
            assert(succeeded.nonEmpty)

            val succeededPayment = succeeded.head
            assert(succeededPayment.status == PaymentStatus.SUCCEEDED)
            assert(succeededPayment.amountMsat == amt)
            assert(succeededPayment.preimage.nonEmpty)
          }
        }
    }

    executeWithClientOtherClient(getPayment)

  }

  it should "be able to pay to a hash" in {
    val amt = 50.msats
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
            invoice <- otherClient.createInvoice("foo", amt)
            paymentId <- client.sendToNode(otherClientNodeId,
                                           amt,
                                           invoice.lnTags.paymentHash.hash,
                                           None,
                                           None,
                                           None)
            _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
            succeeded <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
            _ <- client.close(channelId)
            _ <- bitcoindRpcClientF.flatMap(_.generate(6))
          } yield {
            assert(succeeded.nonEmpty)

            val succeededPayment = succeeded.head
            assert(succeededPayment.status == PaymentStatus.SUCCEEDED)
            assert(succeededPayment.amountMsat == amt)
            assert(succeededPayment.preimage.nonEmpty)
          }
        }
    }

    executeWithClientOtherClient(getPayment)

  }

  it should "be able to generate an invoice with amount and pay it and close the channel" in {
    val amt = 50.msats

    val getPaymentWithAmount = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        {
          for {
            channelId <- openAndConfirmChannel(clientF, otherClientF)
            invoice <- otherClient.createInvoice("test", amt)
            paymentId <- client.payInvoice(invoice)
            _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
            succeeded <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
            received <- otherClient.getReceivedInfo(invoice)
            _ <- client.close(channelId)
            _ <- EclairRpcTestUtil.awaitUntilChannelClosing(otherClient,
                                                            channelId)
            channel <- otherClient.channel(channelId)
            _ <- bitcoindRpcClientF.flatMap(_.generate(6))
          } yield {
            assert(succeeded.nonEmpty)

            assert(received.get.paymentHash == invoice.lnTags.paymentHash.hash)
            assert(received.get.amountMsat == amt)

            val succeededPayment = succeeded.head
            assert(succeededPayment.status == PaymentStatus.SUCCEEDED)
            assert(succeededPayment.amountMsat == amt)
            assert(succeededPayment.preimage.nonEmpty)

            assert(channel.state == ChannelState.CLOSING)
          }
        }
    }

    executeWithClientOtherClient(getPaymentWithAmount)
  }

  it should "be able to generate an invoice without amount and pay it" in {
    val amt = 50.msats
    val getPaymentNoAmount = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        {
          for {
            channelId <- openAndConfirmChannel(clientF, otherClientF)
            invoice <- otherClient.createInvoice("no amount")
            paymentId <- client.payInvoice(invoice, amt)
            _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
            succeeded <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
            _ <- client.close(channelId)
            _ <- bitcoindRpcClientF.flatMap(_.generate(6))
          } yield {
            assert(succeeded.nonEmpty)

            val succeededPayment = succeeded.head
            assert(succeededPayment.status == PaymentStatus.SUCCEEDED)
            assert(succeededPayment.amountMsat == amt)
            assert(succeededPayment.preimage.nonEmpty)
          }
        }
    }

    executeWithClientOtherClient(getPaymentNoAmount)
  }

  it should "be able to generate an invoice and get the same amount back" in {
    val amt = PicoBitcoins(10) //this is the smallest unit we can use, 1 msat
    val description = "bitcoin-s test case"
    val expiry = System.currentTimeMillis().millis

    val invoiceF = clientF.flatMap(
      _.createInvoice(description = description,
                      amountMsat = amt.toMSat,
                      expireIn = expiry))

    val assert0: Future[Assertion] = {
      invoiceF.map { i =>
        assert(i.amount.get == amt)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry.toSeconds)
      }
    }

    val amt1 = NanoBitcoins.one
    val invoice1F = clientF.flatMap(
      _.createInvoice(description = description,
                      amountMsat = Some(amt1.toMSat),
                      expireIn = Some(expiry),
                      None,
                      None))

    val assert1 = {
      invoice1F.map { i =>
        assert(i.amount.get == amt1)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry.toSeconds)
      }
    }

    val amt2 = MicroBitcoins.one
    val invoice2F = clientF.flatMap(
      _.createInvoice(description = description,
                      amountMsat = amt2.toMSat,
                      expireIn = expiry))

    val assert2 = {
      invoice2F.map { i =>
        assert(i.amount.get == amt2)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry.toSeconds)

      }
    }

    val amt3 = MilliBitcoins.one

    val invoice3F = clientF.flatMap(
      _.createInvoice(description = description,
                      amountMsat = amt3.toMSat,
                      expireIn = expiry))

    val assert3 = {
      invoice3F.map { i =>
        assert(i.amount.get == amt3)
        assert(i.lnTags.description.get.string == description)
        assert(i.lnTags.expiryTime.get.u32.toLong == expiry.toSeconds)
      }
    }

    assert0.flatMap { _ =>
      assert1.flatMap { _ =>
        assert2.flatMap(_ => assert3)
      }
    }
  }

  it should "be able to generate a payment invoice and then check that invoice" in {
    val amt = 1000.msats
    val description = "bitcoin-s test case"
    val expiry = (System.currentTimeMillis() / 1000).seconds

    val invoiceF = clientF.flatMap(
      _.createInvoice(description = description,
                      amountMsat = amt,
                      expireIn = expiry))

    val paymentRequestF: Future[InvoiceResult] = invoiceF.flatMap { i =>
      clientF.flatMap(_.parseInvoice(i))
    }

    paymentRequestF.map { paymentRequest =>
      val i = LnInvoice.fromString(paymentRequest.serialized).get
      assert(i.amount.get.toMSat == amt)
      assert(paymentRequest.timestamp == expiry)
    }
  }

  it should "fail to get received info about an invoice that hasn't been paid too, and then sucessfully get the info after the payment happened" in {
    val amt = 1000.msat
    for {
      c1 <- clientF
      c2 <- otherClientF
      invoice <- c2.createInvoice(s"invoice-payment")
      receiveOpt <- c2.getReceivedInfo(invoice)
      _ = assert(receiveOpt.isEmpty)
      _ <- c1.payInvoice(invoice, amt)
      _ <- AsyncUtil.retryUntilSatisfiedF(
        () => c2.getReceivedInfo(invoice).map(_.isDefined),
        1.seconds)
      receivedAgainOpt <- c2.getReceivedInfo(invoice)
    } yield {
      assert(receivedAgainOpt.isDefined)
      assert(receivedAgainOpt.get.amountMsat == amt)
      assert(
        receivedAgainOpt.get.paymentHash == invoice.lnTags.paymentHash.hash)
    }
  }

  it should "monitor an invoice" in {
    val amt = 1000.msat
    val test = (client: EclairRpcClient, otherClient: EclairRpcClient) => {
      val invoiceF = otherClient.createInvoice("monitor an invoice", amt)
      val paidF = invoiceF.flatMap(i => client.payInvoice(i))
      val monitorF = invoiceF.flatMap(i => otherClient.monitorInvoice(i))
      for {
        paid <- paidF
        invoice <- invoiceF
        received <- monitorF
      } yield {
        assert(received.amountMsat == amt)
        assert(received.paymentHash == invoice.lnTags.paymentHash.hash)
      }
    }
    executeWithClientOtherClient(test)
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
        EclairRpcTestUtil.sendPayments(c1 = n4.c1, c2 = n4.c2, numPayments = 1)
      val p2F =
        EclairRpcTestUtil.sendPayments(c1 = n4.c3, c2 = n4.c4, numPayments = 1)

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

  it should "be able to send payments in both directions" in {
    val paymentAmount = MilliSatoshis(5000)
    val paymentAmount2 = MilliSatoshis(500)

    val sendInBothDiercions = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        for {
          _ <- openAndConfirmChannel(clientF, otherClientF)

          invoice <- otherClient.createInvoice("test", paymentAmount)
          paymentId <- client.payInvoice(invoice)
          _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
          sentInfo <- client.getSentInfo(invoice.lnTags.paymentHash.hash)

          invoice2 <- client.createInvoice("test", paymentAmount2)
          paymentId2 <- otherClient.payInvoice(invoice2)
          _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(otherClient,
                                                            paymentId2)
          sentInfo2 <- otherClient.getSentInfo(invoice2.lnTags.paymentHash.hash)
        } yield {
          assert(sentInfo.head.amountMsat == paymentAmount)
          assert(sentInfo2.head.amountMsat == paymentAmount2)
        }
    }

    executeWithClientOtherClient(sendInBothDiercions)
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
      _ <- clientF.flatMap(_.updateRelayFee(channel, oldFee * 2, 1))
      newFeeOpt <- clientF.flatMap(_.channel(channel).map(_.feeBaseMsat))
    } yield {
      assert(newFeeOpt.isDefined)
      assert(newFeeOpt.get == oldFee * 2)
    }
  }

  it should "update the relay fee of a channel with short channel id" in {
    val channelAndFeeF = for {
      channelId <- openAndConfirmChannel(clientF, otherClientF)
      client <- clientF
      channel <- client.channel(channelId)
    } yield {
      assert(channel.feeBaseMsat.isDefined)
      assert(channel.feeBaseMsat.get > MilliSatoshis.zero)
      assert(channel.shortChannelId.isDefined)
      (channel.channelId, channel.shortChannelId.get, channel.feeBaseMsat.get)
    }

    for {
      (channelId, shortChannelId, oldFee) <- channelAndFeeF
      _ <- clientF.flatMap(_.updateRelayFee(shortChannelId, oldFee * 4, 1))
      newFeeOpt <- clientF.flatMap(_.channel(channelId).map(_.feeBaseMsat))
    } yield {
      assert(newFeeOpt.isDefined)
      assert(newFeeOpt.get == oldFee * 4)
    }
  }

  it should "get channels" in {
    clientF.flatMap(_.channels().flatMap(channels => assert(channels.nonEmpty)))
  }

  it should "get all channels" in {
    clientF.flatMap(
      _.allChannels().flatMap(channels => assert(channels.nonEmpty)))
  }

  it should "get all channel updates" in {
    clientF.flatMap(_.allUpdates().flatMap { updates =>
      assert(updates.nonEmpty)
    })
  }

  it should "get updates for a single node" in {
    for {
      client <- clientF
      nodeInfo <- client.getInfo
      updates <- client.allUpdates(nodeInfo.nodeId)
    } yield {
      assert(updates.nonEmpty)
    }
  }

  it should "get all nodes" in {
    clientF.flatMap(_.allNodes().flatMap(nodes => assert(nodes.nonEmpty)))
  }

  it must "receive gossip messages about channel updates for nodes we do not have a direct channel with" in {
    //make sure we see payments outside of our immediate peers
    //this is important because these gossip messages contain
    //information about channel fees, so we need to get updates
    //about the channels for future fee calculations
    val sentPaymentF = secondClientF.flatMap { c1 =>
      thirdClientF.flatMap { c2 =>
        EclairRpcTestUtil.sendPayments(c1, c2, numPayments = 1)
      }
    }

    val gossipFromPeerWithNoChannel = {
      (client: EclairRpcClient, nonPeer: EclairRpcClient) =>
        sentPaymentF.flatMap { _ =>
          val nodeIdF = client.getNodeURI.map(_.nodeId)

          def ourUpdates = nodeIdF.flatMap(nonPeer.allUpdates)

          def allUpdates = nonPeer.allUpdates()

          def checkUpdates(): Future[Boolean] = {
            ourUpdates.flatMap(our =>
              allUpdates.map { all =>
                our != all
            })
          }

          val checkedUpatesF: Future[Unit] =
            AsyncUtil.retryUntilSatisfiedF((() => checkUpdates()),
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

  it should "force close a channel" in {
    for {
      c <- clientF
      channels <- c.channels()
      _ <- c.forceClose(channels.head.channelId)
      channel <- c.channel(channels.head.channelId)
    } yield {
      assert(channel.state == ChannelState.CLOSING)
    }
  }

  it should "get channel stats" in {
    for {
      c <- clientF
      res <- c.channelStats()
    } yield {
      assert(res.nonEmpty)
    }
  }

  it should "get network fees" in {
    for {
      c <- clientF
      res <- c.networkFees(from = None, to = None)
    } yield {
      assert(res.nonEmpty)
    }
  }

  it should "get invoice" in {
    for {
      c <- clientF
      i <- c.createInvoice("test", 12345789.msats)
      res <- c.getInvoice(i.lnTags.paymentHash.hash)
    } yield {
      assert(res.amount == i.amount)
    }
  }

  it should "list invoice" in {
    for {
      c <- clientF
      res <- c.listInvoices(from = None, to = None)
    } yield {
      assert(res.nonEmpty)
    }
  }

  it should "get usable balances" in {
    for {
      c <- clientF
      res <- c.usableBalances()
    } yield {
      assert(res.nonEmpty)
    }
  }

  it should "disconnect node" in {
    for {
      c1 <- clientF
      c2 <- otherClientF
      nodeInfo2 <- c2.getInfo
      _ <- c1.disconnect(nodeInfo2.nodeId)
    } yield {
      succeed
    }
  }

  private def hasConnection(
      client: Future[EclairRpcClient],
      nodeId: NodeId): Future[Assertion] = {

    val hasPeersF = client.flatMap(_.getPeers.map(_.nonEmpty))

    val hasPeersAssertF = hasPeersF.map(h => assert(h))

    val isConnectedF = client.flatMap(_.isConnected(nodeId))

    val isConnectedAssertF =
      isConnectedF.map(isConnected => assert(isConnected))

    hasPeersAssertF.flatMap(_ => isConnectedAssertF.map(isConn => isConn))
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
