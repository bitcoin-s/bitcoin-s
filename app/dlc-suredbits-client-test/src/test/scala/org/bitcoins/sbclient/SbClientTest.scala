package org.bitcoins.sbclient

import org.bitcoins.commons.jsonmodels.eclair.{
  IncomingPaymentStatus,
  OutgoingPaymentStatus
}
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.testkit.eclair.MockEclairClient
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class SbClientTest extends BitcoinSAsyncTest {
  behavior of "SbClient"

  val str = "Never gonna give you up, never gonna let you down"
  val amt: MilliSatoshis = MilliSatoshis(10000)
  val server = new MockServer()
  val client = new MockEclairClient()

  override def beforeAll(): Unit = {
    super.beforeAll()
    client.otherClient = Some(server.mockEclair)
    server.mockEclair.otherClient = Some(client)
    val _ = Await.result(server.start(), 5.seconds)
  }

  override def afterAll: Unit = {
    Await.result(server.stop(), 5.seconds)
    super.afterAll
  }

  it should "successfully decrypt data from server" in {
    server.encryptData(str, amt.toLnCurrencyUnit).map {
      case (invoice, encrypted) =>
        val preImage = server.preImage(invoice)
        val decrypted = SbClient.decryptData(encrypted, preImage)
        assert(decrypted == str)
    }
  }

  it should "successfully make a payment" in {
    for {
      invoice <- server.mockEclair.createInvoice(str, amt)
      preImage <- SbClient.makePayment(client, invoice)
      serverNodeId <- server.mockEclair.nodeId()
    } yield {
      assert(server.preImage(invoice) == preImage)

      val incoming =
        server.mockEclair.incomingPayment(invoice.lnTags.paymentHash)
      val outgoing = client.outgoingPayment(invoice.lnTags.paymentHash)

      assert(incoming.paymentRequest == outgoing.paymentRequest.get)
      assert(incoming.paymentPreimage == preImage)
      assert(incoming.status.isInstanceOf[IncomingPaymentStatus.Received])
      assert(outgoing.status.isInstanceOf[OutgoingPaymentStatus.Succeeded])
      assert(
        outgoing.status
          .asInstanceOf[OutgoingPaymentStatus.Succeeded]
          .paymentPreimage == preImage)
      assert(outgoing.amount == amt)
      assert(outgoing.recipientNodeId == serverNodeId)
    }
  }

  def exchangeAndPairGen: Gen[(Exchange, TradingPair)] =
    Gen.oneOf(Exchange.all).flatMap { exchange =>
      Gen.oneOf(exchange.pairs).map { pair =>
        (exchange, pair)
      }
    }

  it should "successfully request and decrypt an R value" in {
    forAllAsync(exchangeAndPairGen) {
      case (exchange, pair) =>
        SbClient
          .requestAndPay(exchange,
                         pair,
                         RequestType.RValue,
                         client,
                         server.endpoint)
          .map { decrypted =>
            assert(
              decrypted == s"${exchange.toLongString}|${pair.toLowerString}|rvalue")
          }
    }
  }

  it should "successfully request and decrypt the last signature" in {
    forAllAsync(exchangeAndPairGen) {
      case (exchange, pair) =>
        SbClient
          .requestAndPay(exchange,
                         pair,
                         RequestType.LastSig,
                         client,
                         server.endpoint)
          .map { decrypted =>
            assert(
              decrypted == s"${exchange.toLongString}|${pair.toLowerString}|lastsig")
          }
    }
  }
}
