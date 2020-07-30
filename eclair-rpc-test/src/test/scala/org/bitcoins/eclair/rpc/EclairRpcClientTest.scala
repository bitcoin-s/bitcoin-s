package org.bitcoins.eclair.rpc

import java.nio.file.Files
import java.time.Instant

import org.bitcoins.commons.jsonmodels.eclair._
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ln.LnParams.LnBitcoinRegTest
import org.bitcoins.core.protocol.ln.channel.{
  ChannelId,
  ChannelState,
  FundedChannelId
}
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.ln.{
  LnHumanReadablePart,
  LnInvoice,
  PaymentPreimage
}
import org.bitcoins.eclair.rpc.api._
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.config.{EclairAuthCredentials, EclairInstance}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.util.AsyncUtil
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.eclair.rpc.{EclairNodes4, EclairRpcTestUtil}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalatest.Assertion

import scala.concurrent._
import scala.concurrent.duration.{DurationInt, _}

class EclairRpcClientTest extends BitcoinSAsyncTest {

  private val dirExists = Files.exists(EclairRpcTestUtil.binaryDirectory)

  private val hasContents = dirExists && Files
    .list(EclairRpcTestUtil.binaryDirectory)
    .toArray()
    .nonEmpty

  if (!hasContents) {
    import System.err.{println => printerr}
    printerr()
    printerr(s"Run 'sbt downloadEclair' to fetch needed binaries")
    sys.error {
      val msg =
        s""""Eclair binary directory (${BitcoindRpcTestUtil.binaryDirectory}) is empty. 
           |Run 'sbt downloadEclair' to fetch needed binaries""".stripMargin
      msg
    }
  }

  lazy val bitcoindRpcClientF: Future[BitcoindRpcClient] = {
    for {
      cli <- EclairRpcTestUtil.startedBitcoindRpcClient()
      // make sure we have enough money to open channels
      address <- cli.getNewAddress
      _ <- cli.generateToAddress(200, address)
    } yield cli
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

  behavior of "EclairRpcClient"

  it must "have our EclairRpcClient work with .hashCode() and equals" in {
    val f = (client1: EclairApi, client2: EclairApi) => {
      Future {
        assert(client1 != client2)
        assert(client1.hashCode() != client2.hashCode())
      }
    }

    executeWithClientOtherClient(f)
  }

  it should "perform on-chain operations" in {
    for {
      c <- clientF
      address <- c.getNewAddress()
      balance <- c.onChainBalance()
      txid <- c.sendOnChain(address, Satoshis(5000), 1)
      balance1 <- c.onChainBalance()
      transactions <- c.onChainTransactions()
    } yield {
      assert(balance.confirmed > Satoshis(0))
      assert(balance.unconfirmed == Satoshis(0))
      // we sent 5000 sats to ourselves and paid some sats in fee
      assert(balance1.confirmed < balance.confirmed)
      assert(transactions.exists(_.txid == txid))
    }
  }

  /**
    * Please keep this test the very first. All other tests rely on the propagated gossip messages.
    */
  it should "wait for all gossip messages get propagated throughout the network and get a route to an invoice" in {
    val hasRoute = () => {
      (for {
        client1 <- firstClientF
        client4 <- fourthClientF
        bitcoind <- bitcoindRpcClientF
        _ <- EclairRpcTestUtil.awaitEclairInSync(client4, bitcoind)
        _ <- EclairRpcTestUtil.awaitEclairInSync(client1, bitcoind)
        invoice <- client4.createInvoice("foo", 1000.msats)
        info <- client4.getInfo
        _ = assert(info.nodeId == invoice.nodeId)
        _ = assert(
          info.publicAddresses
            .map(_.getHostString)
            .exists(_.endsWith(".onion")))
        route <- client1.findRoute(invoice, None)
      } yield {
        route.size == 4
      }).recover {
        case err: RuntimeException
            if err.getMessage.contains("route not found") =>
          false
      }
    }

    AsyncUtil
      .awaitConditionF(hasRoute, duration = 1.second, maxTries = 60)
      .map(_ => succeed)
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

    AsyncUtil
      .awaitConditionF(hasRoute, duration = 1.second, maxTries = 60)
      .map(_ => succeed)
  }

  it should "send some payments and get the audit info" in {
    for {
      client1 <- firstClientF
      client2 <- secondClientF
      client4 <- fourthClientF
      bitcoind <- bitcoindRpcClientF
      _ <- EclairRpcTestUtil.awaitEclairInSync(client4, bitcoind)
      _ <- EclairRpcTestUtil.awaitEclairInSync(client1, bitcoind)
      invoice <- client4.createInvoice("test", 1000.msats)
      info <- client4.getInfo
      _ = assert(info.nodeId == invoice.nodeId)
      paymentId <- client1.payInvoice(invoice)
      _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client1,
                                                        paymentId,
                                                        duration = 1.second)
      received <- client4.audit()
      relayed <- client2.audit()
      sent <- client1.audit()
    } yield {
      assert(sent.sent.nonEmpty)
      assert(received.received.nonEmpty)
      assert(relayed.relayed.nonEmpty)
    }
  }

  it should "pay an invoice and monitor the payment" in {
    val checkPayment = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        for {
          bitcoind <- bitcoindRpcClientF
          _ <- EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)
          _ <- EclairRpcTestUtil.awaitEclairInSync(otherClient, bitcoind)
          _ <- EclairRpcTestUtil.awaitEclairInSync(client, bitcoind)
          invoice <- otherClient.createInvoice("abc", 50.msats)
          info <- otherClient.getInfo
          _ = assert(info.nodeId == invoice.nodeId)
          paymentResult <-
            client.payAndMonitorInvoice(invoice, Some("ext_id"), 3.second, 60)
        } yield {
          assert(paymentResult.amount == 50.msats)
          assert(paymentResult.externalId.contains("ext_id"))
        }
    }

    executeWithClientOtherClient(checkPayment)
  }

  it should "check a payment" in {
    val checkPayment = {
      (client: EclairRpcClient, otherClient: EclairRpcClient) =>
        for {
          _ <- EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)
          invoice <- otherClient.createInvoice("abc", 50.msats)
          info <- otherClient.getInfo
          _ = assert(info.nodeId == invoice.nodeId)
          infos <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
          _ = assert(infos.isEmpty)
          paymentId <- client.payInvoice(invoice)
          _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
          sentInfo <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
        } yield {
          assert(sentInfo.head.amount == 50.msats)
        }
    }

    executeWithClientOtherClient(checkPayment)
  }

  it should "be able to create an empty invoice" in {
    for {
      c <- clientF
      invoice <- c.createInvoice("test")
      info <- c.getInfo
    } yield {
      assert(invoice.nodeId == info.nodeId)
      assert(invoice.hrp.toString == LnHumanReadablePart.lnbcrt.toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount.isEmpty)
      assert(invoice.isValidSignature)
      assert(
        invoice.timestamp > UInt64(1561063731)
      ) // this is when I wrote this code
      assert(invoice.timestamp <= UInt64(System.currentTimeMillis() / 1000))
    }
  }

  it should "be able to create an invoice with amount" in {
    for {
      c <- clientF
      invoice <- c.createInvoice(description = "test", amountMsat = 12345.msats)
      info <- c.getInfo
    } yield {
      assert(invoice.nodeId == info.nodeId)
      assert(
        invoice.hrp.toString == LnHumanReadablePart
          .lnbcrt(Some(12345.msats.toLnCurrencyUnit))
          .toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount == Some(123450.pBTC))
      assert(invoice.isValidSignature)
      assert(
        invoice.timestamp > UInt64(1561063731)
      ) // this is when I wrote this code
      assert(invoice.timestamp <= UInt64(System.currentTimeMillis() / 1000))
    }
  }

  it should "be able to create an invoice with amount and expiry time" in {
    for {
      c <- clientF
      invoice <- c.createInvoice(description = "test",
                                 amountMsat = 12345.msats,
                                 expireIn = 67890.seconds)
      info <- c.getInfo
    } yield {
      assert(invoice.nodeId == info.nodeId)
      assert(
        invoice.hrp.toString == LnHumanReadablePart
          .lnbcrt(Some(12345.msats.toLnCurrencyUnit))
          .toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount == Some(123450.pBTC))
      assert(invoice.isValidSignature)
      assert(
        invoice.timestamp > UInt64(1561063731)
      ) // this is when I wrote this code
      assert(invoice.timestamp <= UInt64(System.currentTimeMillis() / 1000))
    }
  }

  val testBitcoinAddress = BitcoinAddress("n3p1ct69ao3qxWvEvzLhLtWG2zJGTjN3EV")

  it should "be able to create an invoice with amount, expiry time, and fallbackAddress" in {
    for {
      c <- clientF
      invoice <- c.createInvoice(description = "test",
                                 amountMsat = Some(12345.msats),
                                 expireIn = Some(67890.seconds),
                                 fallbackAddress = Some(testBitcoinAddress),
                                 paymentPreimage = None)
      info <- c.getInfo
    } yield {
      assert(invoice.nodeId == info.nodeId)
      assert(
        invoice.hrp.toString == LnHumanReadablePart
          .lnbcrt(Some(123450.pBTC))
          .toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount == Some(123450.pBTC))
      assert(invoice.isValidSignature)
      assert(
        invoice.timestamp > UInt64(1561063731)
      ) // this is when I wrote this code
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
      info <- c.getInfo
    } yield {
      assert(invoice.nodeId == info.nodeId)
      assert(
        invoice.hrp.toString == LnHumanReadablePart
          .lnbcrt(Some(12345.msats.toLnCurrencyUnit))
          .toString)
      assert(invoice.network == LnBitcoinRegTest)
      assert(invoice.amount == Some(123450.pBTC))
      assert(invoice.isValidSignature)
      assert(
        invoice.timestamp > UInt64(1561063731)
      ) // this is when I wrote this code
      assert(invoice.timestamp <= UInt64(System.currentTimeMillis() / 1000))
    }
  }

  it should "be able to start and shutdown a node" in {
    for {
      bitcoind <- EclairRpcTestUtil.startedBitcoindRpcClient()
      eclair <- {
        val server = EclairRpcTestUtil.eclairInstance(bitcoind)
        val eclair =
          new EclairRpcClient(
            server,
            EclairRpcClient.getEclairBinary(EclairRpcTestUtil.binaryDirectory))
        eclair.start().map(_ => eclair)
      }
      _ <- TestAsyncUtil.retryUntilSatisfiedF(conditionF =
                                                () => eclair.isStarted(),
                                              duration = 1.second,
                                              maxTries = 60)
      _ = EclairRpcTestUtil.shutdown(eclair)
      _ <-
        TestAsyncUtil.retryUntilSatisfiedF(conditionF =
                                             () => eclair.isStarted().map(!_),
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
              val amt = Satoshis(100000)
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
              for {
                bitcoind <- bitcoindRpcClientF
                address <- bitcoind.getNewAddress
                _ <- bitcoind.generateToAddress(6, address)
                _ <- EclairRpcTestUtil.awaitUntilChannelNormal(
                  client = client,
                  chanId = chanId
                )

              } yield (chanId, assertion)
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
                                      authCredentials = badCredentials,
                                      logbackXmlPath = None)

        Future.successful(instance)
      }

      executeWithClientOtherClient(getBadInstance)
    }

    val badClientF =
      badInstanceF.map(
        new EclairRpcClient(
          _,
          EclairRpcClient.getEclairBinary(EclairRpcTestUtil.binaryDirectory)))

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
            otherClientInfo <- otherClient.getInfo
            otherClientNodeId = otherClientInfo.nodeId
            preimage = PaymentPreimage.random
            invoice <- otherClient.createInvoice("foo", amt, preimage)
            route <- client.findRoute(otherClientNodeId, amt)
            result <- client.sendToRoute(invoice,
                                         route,
                                         amt,
                                         invoice.lnTags.paymentHash.hash,
                                         finalCltvExpiry = 144,
                                         recipientAmountMsat = None,
                                         parentId = None,
                                         externalId = Some("ext_id"))
            _ <-
              EclairRpcTestUtil
                .awaitUntilIncomingPaymentStatus[
                  IncomingPaymentStatus.Received](
                  otherClient,
                  invoice.lnTags.paymentHash.hash)
            _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client,
                                                              result.parentId)
            succeeded <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
          } yield {
            assert(otherClientNodeId == invoice.nodeId)
            assert(succeeded.nonEmpty)

            val succeededPayment = succeeded.head
            assert(succeededPayment.amount == amt)
            assert(succeededPayment.externalId.contains("ext_id"))
            succeededPayment.status match {
              case sent: OutgoingPaymentStatus.Succeeded =>
                assert(sent.paymentPreimage == preimage)
              case s: OutgoingPaymentStatus =>
                fail(s"Unexpected payment status ${s}")
            }
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
            channelId <-
              EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)
            otherClientNodeId <- otherClient.getInfo.map(_.nodeId)
            channels <- client.channels(otherClientNodeId)
            // without this we've been getting "route not found"
            // probably an async issue, this is more elegant than Thread.sleep
            _ = assert(channels.exists(_.state == ChannelState.NORMAL),
                       "Nodes did not have open channel!")
            preimage = PaymentPreimage.random
            wsEventP = Promise[WebSocketEvent]
            _ <- client.connectToWebSocket { event =>
              if (!wsEventP.isCompleted) {
                wsEventP.success(event)
              }
            }
            invoice <- otherClient.createInvoice("foo", amt, preimage)
            paymentId <- client.sendToNode(otherClientNodeId,
                                           amt,
                                           invoice.lnTags.paymentHash.hash,
                                           None,
                                           None,
                                           None,
                                           Some("ext_id"))
            wsEvent <- wsEventP.future
            succeeded <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
            _ <- client.close(channelId)
            bitcoind <- bitcoindRpcClientF
            address <- bitcoind.getNewAddress
            _ <- bitcoind.generateToAddress(6, address)
          } yield {
            assert(wsEvent.isInstanceOf[WebSocketEvent.PaymentSent])
            val paymentSent = wsEvent.asInstanceOf[WebSocketEvent.PaymentSent]
            assert(paymentSent.parts.nonEmpty)
            assert(paymentSent.id == paymentId)
            assert(paymentSent.parts.head.amount == amt)
            assert(paymentSent.parts.head.id == paymentId)
            assert(succeeded.nonEmpty)

            val succeededPayment = succeeded.head
            assert(succeededPayment.amount == amt)
            assert(succeededPayment.externalId.contains("ext_id"))
            succeededPayment.status match {
              case sent: OutgoingPaymentStatus.Succeeded =>
                assert(sent.paymentPreimage == preimage)
              case s: OutgoingPaymentStatus =>
                fail(s"Unexpected payment status ${s}")
            }
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
            channelId <-
              EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)
            preimage = PaymentPreimage.random
            invoice <- otherClient.createInvoice("test", amt, preimage)
            paymentId <- client.payInvoice(invoice)
            _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
            succeeded <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
            received <- otherClient.getReceivedInfo(invoice)
            _ <- client.close(channelId)
            _ <-
              EclairRpcTestUtil.awaitUntilChannelClosing(otherClient, channelId)
            channel <- otherClient.channel(channelId)
            bitcoind <- bitcoindRpcClientF
            address <- bitcoind.getNewAddress
            _ <- bitcoind.generateToAddress(6, address)
          } yield {
            assert(succeeded.nonEmpty)

            assert(
              received.get.paymentRequest.paymentHash == invoice.lnTags.paymentHash.hash)
            assert(
              received.get.status
                .asInstanceOf[IncomingPaymentStatus.Received]
                .amount == amt)

            val succeededPayment = succeeded.head
            assert(succeededPayment.amount == amt)
            succeededPayment.status match {
              case sent: OutgoingPaymentStatus.Succeeded =>
                assert(sent.paymentPreimage == preimage)
              case s: OutgoingPaymentStatus =>
                fail(s"Unexpected payment status ${s}")
            }

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
            channelId <-
              EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)
            invoice <- otherClient.createInvoice("no amount")
            paymentId <- client.payInvoice(invoice, amt)
            _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
            _ <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
            succeeded <- client.getSentInfo(paymentId)
            _ <- client.close(channelId)
          } yield {
            assert(succeeded.nonEmpty)

            val succeededPayment = succeeded.head
            assert(succeededPayment.amount == amt)
            assert(
              succeededPayment.status
                .isInstanceOf[OutgoingPaymentStatus.Succeeded])
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
    val expiry = 10.seconds

    val invoiceF = clientF.flatMap(
      _.createInvoice(description = description,
                      amountMsat = amt,
                      expireIn = expiry))

    val paymentRequestF: Future[InvoiceResult] = invoiceF.flatMap { i =>
      clientF.flatMap(_.parseInvoice(i))
    }

    paymentRequestF.map { paymentRequest =>
      val i = LnInvoice.fromString(paymentRequest.serialized)
      assert(i.amount.get.toMSat == amt)
      assert(paymentRequest.expiry == expiry)
    }
  }

  it should "fail to get received info about an invoice that hasn't been paid too, and then sucessfully get the info after the payment happened" in {
    val amt = 1000.msat
    for {
      c1 <- clientF
      c2 <- otherClientF
      invoice <- c2.createInvoice(s"invoice-payment")
      receiveOpt <- c2.getReceivedInfo(invoice)
      _ = assert(receiveOpt.get.status == IncomingPaymentStatus.Pending)
      _ <- c1.payInvoice(invoice, amt)
      _ <- AsyncUtil.retryUntilSatisfiedF(
        () =>
          c2.getReceivedInfo(invoice)
            .map(_.get.status.isInstanceOf[IncomingPaymentStatus.Received]),
        1.seconds)
      receivedAgainOpt <- c2.getReceivedInfo(invoice)
    } yield {
      assert(receivedAgainOpt.isDefined)
      assert(
        receivedAgainOpt.get.status
          .asInstanceOf[IncomingPaymentStatus.Received]
          .amount == amt)
      assert(
        receivedAgainOpt.get.paymentRequest.paymentHash == invoice.lnTags.paymentHash.hash)
    }
  }

  it should "monitor an invoice" in {
    val amt = 1234.msat
    val test = (client: EclairRpcClient, otherClient: EclairRpcClient) => {
      val res = for {
        bitcoind <- bitcoindRpcClientF
        _ <- EclairRpcTestUtil.awaitEclairInSync(otherClient, bitcoind)
        _ <- EclairRpcTestUtil.awaitEclairInSync(client, bitcoind)
        invoice: LnInvoice <-
          otherClient.createInvoice("monitor an invoice", amt)
        _ <- client.payInvoice(invoice)
        received <- otherClient.monitorInvoice(invoice,
                                               interval = 1.seconds,
                                               maxAttempts = 60)
      } yield {
        assert(
          received.status
            .asInstanceOf[IncomingPaymentStatus.Received]
            .amount == amt)
        assert(
          received.paymentRequest.paymentHash == invoice.lnTags.paymentHash.hash)
      }
      res
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
        .openChannel(c1, c2, Satoshis(500000), MilliSatoshis(500000))
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
      for {
        _ <- openedChannelsF
        bitcoind <- bitcoindRpcClientF
        address <- bitcoind.getNewAddress
        hashes <- bitcoind.generateToAddress(10, address)
      } yield hashes

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
          ourOpenChannels <-
            firstFreshClient
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
          _ <- EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)

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
          assert(sentInfo.head.amount == paymentAmount)
          assert(sentInfo2.head.amount == paymentAmount2)
        }
    }

    executeWithClientOtherClient(sendInBothDiercions)
  }

  it should "update the relay fee of a channel" in {
    val channelAndFeeF = for {
      channel <- EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)
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
      channelId <-
        EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)
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

  it should "get all nodes" in {
    clientF.flatMap(_.allNodes().flatMap(nodes => assert(nodes.nonEmpty)))
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

          AsyncUtil
            .retryUntilSatisfiedF((() => checkUpdates()),
                                  duration = 1.second,
                                  maxTries = 60)
            .transform(_ => succeed, ex => ex)
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

  it should "list invoices" in {
    for {
      c <- clientF
      res <- c.listInvoices(from = None, to = Some(Instant.now()))
      i <- c.createInvoice(description = "abc")
      pending <- c.listPendingInvoices(from = None, to = None)
    } yield {
      assert(res.nonEmpty)
      assert(pending.exists(_ == i))
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
      assert(nodeInfo2.features.activated.nonEmpty)
      assert(nodeInfo2.network == RegTest)
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

  private def updateIsInChannels(channels: Seq[OpenChannelInfo])(
      update: ChannelUpdate): Boolean = {
    channels.exists(_.shortChannelId == update.shortChannelId)
  }

  override def afterAll(): Unit = {
    clients.result().foreach(EclairRpcTestUtil.shutdown)
    super.afterAll
  }
}
