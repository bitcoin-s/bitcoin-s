package org.bitcoins.rpc

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import org.bitcoins.core.protocol.Address
import play.api.libs.json._

import scala.concurrent.Future
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.util.BitcoinSLogger

import scala.util.{ Failure, Try }

class RpcClient {
  private val resultKey = "result"
  private val logger = BitcoinSLogger.logger

  def getNewAddress(account: Option[String])(implicit m: ActorMaterializer): Future[Address] = {
    val request = buildRequest("getnewaddress", JsArray.empty)
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)(m.executionContext)

    val addressF: Future[Try[Address]] = payloadF.map { payload =>
      val result: JsResult[String] = (payload \ resultKey).validate[String]
      result match {
        case res: JsSuccess[String] => Address.fromString(res.value)
        case res: JsError =>
          logger.error(JsError.toJson(res).toString())
          Failure(new IllegalArgumentException("Address was not formed"))
      }
    }(m.executionContext)

    addressF.flatMap { f =>
      Future.fromTry(f)
    }(m.executionContext)
  }

  private def getPayload(response: HttpResponse)(implicit m: ActorMaterializer): Future[JsValue] = {
    val payloadF = response.entity.dataBytes.runFold(ByteString(""))(_ ++ _)

    payloadF.map { payload =>
      Json.parse(payload.decodeString(ByteString.UTF_8))
    }(m.executionContext)
  }

  def sendRequest(req: HttpRequest)(implicit m: ActorMaterializer): Future[HttpResponse] = {
    Http(m.system).singleRequest(req)
  }

  def buildRequest(methodName: String, params: JsArray): HttpRequest = {
    val m: Map[String, JsValue] = Map(
      "method" -> JsString(methodName),
      "params" -> params,
      "id" -> JsString(""))
    val jsObject = JsObject(m)

    val uri = "http://localhost:18332"
    val username = "nadav"
    val password = "abc123"
    HttpRequest(method = HttpMethods.POST, uri,
      entity = HttpEntity(ContentTypes.`application/json`, jsObject.toString()))
      .addCredentials(HttpCredentials.createBasicHttpCredentials(username, password))
  }
}
