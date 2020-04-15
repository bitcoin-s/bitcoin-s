package org.bitcoins.sbclient

import org.bitcoins.core.crypto.{AesCrypt, AesEncryptedData, AesKey}
import org.bitcoins.core.protocol.ln.{LnInvoice, PaymentPreimage}
import org.bitcoins.core.protocol.ln.currency.LnCurrencyUnit
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

class MockServer()(implicit ec: ExecutionContext) {
  val mockEclair: MockEclairClient = new MockEclairClient()

  def preImage(invoice: LnInvoice): PaymentPreimage = {
    mockEclair.preImage(invoice.lnTags.paymentHash)
  }

  def encryptData(data: String, price: LnCurrencyUnit)(
      implicit ec: ExecutionContext): Future[(LnInvoice, String)] = {
    val invoiceF =
      mockEclair.createInvoice(
        description = s"Mock Invoice",
        price.toMSat
      )

    invoiceF.map { invoice =>
      val preimage = preImage(invoice)

      val dataBytes: ByteVector = ByteVector.encodeUtf8(data) match {
        case Left(err) =>
          throw new RuntimeException(s"Could not UTF8 encode data!", err)
        case Right(bytes) => bytes
      }

      val key = AesKey.fromValidBytes(preimage.bytes)

      val encrypted: AesEncryptedData = AesCrypt.encrypt(dataBytes, key)

      (invoice, encrypted.toBase64)
    }
  }
}
