package org.bitcoins.sbclient

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.commons.jsonmodels.eclair.OutgoingPaymentStatus
import org.bitcoins.crypto.{
  AesCrypt,
  AesEncryptedData,
  AesKey,
  ECPublicKey,
  SchnorrDigitalSignature
}
import org.bitcoins.core.protocol.ln.{LnInvoice, PaymentPreimage}
import org.bitcoins.eclair.rpc.api.EclairApi
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json, Reads}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object SbClient {

  def rawRestCall(uri: Uri)(implicit system: ActorSystem): Future[String] = {
    implicit val m: ActorMaterializer = ActorMaterializer.create(system)
    implicit val ec: ExecutionContextExecutor = m.executionContext

    Http()
      .singleRequest(HttpRequest(uri = uri))
      .flatMap(
        response =>
          response.entity.dataBytes
            .runFold(ByteString.empty)(_ ++ _)
            .map(payload => payload.decodeString(ByteString.UTF_8))
      )
  }

  def restCall(uri: Uri)(implicit system: ActorSystem): Future[JsValue] = {
    rawRestCall(uri).map(Json.parse)(system.dispatcher)
  }

  import org.bitcoins.commons.serializers.JsonReaders.lnInvoiceReads
  implicit val invoiceAndDataResponseReads: Reads[InvoiceAndDataResponse] =
    Json.reads[InvoiceAndDataResponse]

  def request(
      exchange: Exchange,
      tradingPair: TradingPair,
      requestType: RequestType,
      endpoint: String = "https://test.api.suredbits.com/dlc/v0")(
      implicit system: ActorSystem): Future[InvoiceAndDataResponse] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val uri =
      s"$endpoint/${exchange.toLongString}/${tradingPair.toLowerString}/${requestType.requestString}"
    restCall(uri)
      .map(_.validate[InvoiceAndDataResponse])
      .flatMap {
        case JsSuccess(response, _) => Future.successful(response)
        case JsError(error) =>
          Future.failed(
            new RuntimeException(
              s"Unexpected error when parsing response: $error"))
      }
  }

  def makePayment(
      eclairApi: EclairApi,
      invoice: LnInvoice,
      timeout: FiniteDuration = 5.seconds)(
      implicit ec: ExecutionContext): Future[PaymentPreimage] = {
    for {
      payment <- eclairApi.payAndMonitorInvoice(invoice,
                                                externalId = None,
                                                interval = timeout / 10,
                                                maxAttempts = 10)
    } yield payment.status match {
      case OutgoingPaymentStatus.Succeeded(preImage, _, _, _) => preImage
      case OutgoingPaymentStatus.Failed(errs) =>
        val errMsgs = errs.map(_.failureMessage).mkString(",\n")
        throw new RuntimeException(s"Payment failed: $errMsgs")
      case OutgoingPaymentStatus.Pending =>
        throw new IllegalStateException(
          "This should not be possible because invoice monitoring should only return on Succeeded or Failed.")
    }
  }

  def decryptData(data: String, preImage: PaymentPreimage): String = {
    AesCrypt.decrypt(AesEncryptedData.fromValidBase64(data),
                     AesKey.fromValidBytes(preImage.bytes)) match {
      case Left(err) => throw err
      case Right(decrypted) =>
        decrypted.decodeUtf8 match {
          case Right(decodedStr) => decodedStr
          case Left(err)         => throw err
        }
    }
  }

  def requestAndPay(
      exchange: Exchange,
      tradingPair: TradingPair,
      requestType: RequestType,
      eclairApi: EclairApi,
      endpoint: String = "https://test.api.suredbits.com/dlc/v0")(
      implicit system: ActorSystem): Future[String] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    for {
      InvoiceAndDataResponse(invoice, encryptedData) <- request(exchange,
                                                                tradingPair,
                                                                requestType,
                                                                endpoint)
      preImage <- makePayment(eclairApi, invoice)
    } yield decryptData(encryptedData, preImage)
  }

  def requestRValueAndPay(
      exchange: Exchange,
      tradingPair: TradingPair,
      eclairApi: EclairApi)(
      implicit system: ActorSystem): Future[ECPublicKey] = {
    val dataF =
      requestAndPay(exchange, tradingPair, RequestType.RValue, eclairApi)
    dataF.map(ECPublicKey.fromHex)(system.dispatcher)
  }

  def requestLastSigAndPay(
      exchange: Exchange,
      tradingPair: TradingPair,
      eclairApi: EclairApi)(
      implicit system: ActorSystem): Future[SchnorrDigitalSignature] = {
    val dataF =
      requestAndPay(exchange, tradingPair, RequestType.LastSig, eclairApi)
    dataF.map(SchnorrDigitalSignature.fromHex)(system.dispatcher)
  }
}
