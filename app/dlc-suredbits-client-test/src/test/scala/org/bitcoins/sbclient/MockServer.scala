package org.bitcoins.sbclient

import akka.actor.ActorSystem
import org.bitcoins.core.crypto.{AesCrypt, AesEncryptedData, AesKey}
import org.bitcoins.core.protocol.ln.{LnInvoice, PaymentPreimage}
import org.bitcoins.core.protocol.ln.currency.LnCurrencyUnit
import scodec.bits.ByteVector

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class MockServer()(implicit system: ActorSystem) {
  val mockEclair: MockEclairClient = new MockEclairClient()

  val preImages: mutable.Map[LnInvoice, PaymentPreimage] = mutable.Map.empty

  def encryptData(data: String, price: LnCurrencyUnit)(
      implicit ec: ExecutionContext): Future[(LnInvoice, String)] = {
    val preimage = PaymentPreimage.random
    val invoiceF =
      mockEclair.createInvoice(
        description = s"Mock Invoice",
        price.toMSat,
        paymentPreimage = preimage
      )

    invoiceF.map { invoice =>
      preImages(invoice) = preimage

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
