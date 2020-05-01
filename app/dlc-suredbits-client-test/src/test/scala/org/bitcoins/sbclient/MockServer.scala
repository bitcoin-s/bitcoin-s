package org.bitcoins.sbclient

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import org.bitcoins.core.protocol.ln.currency.{LnCurrencyUnit, MilliSatoshis}
import org.bitcoins.core.protocol.ln.{LnInvoice, PaymentPreimage}
import org.bitcoins.crypto._
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.eclair.MockEclairClient
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.matching.Regex

class MockServer()(implicit ec: ExecutionContext) {
  val mockEclair: MockEclairClient = new MockEclairClient()

  def preImage(invoice: LnInvoice): PaymentPreimage = {
    mockEclair.preImage(invoice.lnTags.paymentHash)
  }

  def encryptData(data: String, price: LnCurrencyUnit)(implicit
      ec: ExecutionContext): Future[(LnInvoice, String)] = {
    val invoiceF =
      mockEclair.createInvoice(
        description = s"Mock Invoice",
        price.toMSat
      )

    invoiceF.map { invoice =>
      val preimage = preImage(invoice)

      val dataBytes: ByteVector = ByteVector.encodeUtf8(data) match {
        case Left(err) =>
          throw new RuntimeException(s"Could not encode data to UTF-8!", err)
        case Right(bytes) => bytes
      }

      val key = AesKey.fromValidBytes(preimage.bytes)

      val encrypted: AesEncryptedData = AesCrypt.encrypt(dataBytes, key)

      (invoice, encrypted.toBase64)
    }
  }

  val exchangeRegex: Regex =
    RegexUtil.noCaseOrRegex(Exchange.all.map(_.toLongString).:+("bitmex"))
  val tradingPairRegex: Regex = RegexUtil.noCaseOrRegex(TradingPair.all)

  val requestTypeRegex: Regex =
    RegexUtil.noCaseOrRegex(RequestType.all :+ "PublicKey")

  def route: Route = {
    path(exchangeRegex / tradingPairRegex / requestTypeRegex) {
      case (exchangeStr, pairStr, requestTypeStr) =>
        if (requestTypeStr.toLowerCase == "publickey") {
          val hash = CryptoUtil.sha256(
            ByteVector(s"$exchangeStr|$pairStr|pubkey"
              .getBytes()))
          val pubKey = ECPrivateKey(hash.bytes).schnorrPublicKey

          complete {
            HttpEntity(ContentTypes.`text/plain(UTF-8)`, pubKey.hex)
          }
        } else {
          val data = Vector(exchangeStr, pairStr, requestTypeStr).mkString("|")
          val encryptedF =
            encryptData(data, MilliSatoshis(10000).toLnCurrencyUnit)
          val responseF = encryptedF.map {
            case (invoice, encrypted) =>
              InvoiceAndDataResponse(invoice, encrypted).toJsonString
          }
          complete {
            responseF.map { response =>
              HttpEntity(ContentTypes.`application/json`, response)
            }
          }
        }
    } ~ path("ping") {
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>pong</h1>"))
      }
    }
  }

  val port: Int = RpcUtil.randomPort

  val endpoint: String = s"http://localhost:$port"

  private val serverBindingP = Promise[Future[Http.ServerBinding]]()

  private val serverBindingF: Future[Http.ServerBinding] =
    serverBindingP.future.flatten

  private val stoppedP = Promise[Future[Done]]()
  private val stoppedF: Future[Done] = stoppedP.future.flatten

  serverBindingF.map { binding =>
    scala.sys.addShutdownHook {
      if (!stoppedF.isCompleted) {
        val _ = binding.unbind()
      }
    }
  }

  def start()(implicit system: ActorSystem): Future[Http.ServerBinding] = {
    if (serverBindingP.isCompleted) {
      Future.failed(new RuntimeException("Mock Server already started!"))
    } else {
      val bindingF = Http().bindAndHandle(handler = route,
                                          interface = "localhost",
                                          port = port)
      val _ = serverBindingP.success(bindingF)
      bindingF
    }
  }

  def stop(): Future[Done] = {
    if (stoppedP.isCompleted || !serverBindingP.isCompleted) {
      Future.failed(new RuntimeException(
        "Cannot stop server if it has not been started or has already been stopped"))
    } else {
      val resultF = serverBindingF.flatMap(_.unbind())

      stoppedP.success(resultF)

      resultF
    }
  }
}
