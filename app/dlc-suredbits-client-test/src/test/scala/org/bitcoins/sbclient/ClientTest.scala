package org.bitcoins.sbclient

import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class ClientTest extends BitcoinSAsyncTest {
  behavior of "Client"

  val str = "Never gonna give you up, never gonna let you down"
  val server = new MockServer()

  it should "successfully decrypt data from server" in {
    server.encryptData(str, MilliSatoshis(1000).toLnCurrencyUnit).map {
      case (invoice, encrypted) =>
        val preImage = server.preImage(invoice)
        val decrypted = Client.decryptData(encrypted, preImage)
        assert(decrypted == str)
    }
  }
}
