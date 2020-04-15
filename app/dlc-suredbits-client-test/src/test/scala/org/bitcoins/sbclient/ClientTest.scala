package org.bitcoins.sbclient

import org.bitcoins.commons.jsonmodels.eclair.{
  IncomingPaymentStatus,
  OutgoingPaymentStatus
}
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class ClientTest extends BitcoinSAsyncTest {
  behavior of "Client"

  val str = "Never gonna give you up, never gonna let you down"
  val amt = MilliSatoshis(10000)
  val server = new MockServer()
  val client = new MockEclairClient()

  override def beforeAll(): Unit = {
    super.beforeAll()
    client.otherClient = Some(server.mockEclair)
    server.mockEclair.otherClient = Some(client)
  }

  it should "successfully decrypt data from server" in {
    server.encryptData(str, amt.toLnCurrencyUnit).map {
      case (invoice, encrypted) =>
        val preImage = server.preImage(invoice)
        val decrypted = Client.decryptData(encrypted, preImage)
        assert(decrypted == str)
    }
  }

  it should "successfully make a payment" in {
    for {
      invoice <- server.mockEclair.createInvoice(str, amt)
      preImage <- Client.makePayment(client, invoice)
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
}
