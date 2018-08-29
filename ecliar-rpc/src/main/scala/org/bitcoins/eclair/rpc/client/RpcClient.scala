package org.bitcoins.eclair.rpc.client

import java.util.UUID

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.util.BitcoinSLogger
import play.api.libs.json._

import scala.concurrent.{ ExecutionContext, Future }

class RpcClient(implicit m: ActorMaterializer) {
  implicit val ec: ExecutionContext = m.executionContext
  private val logger = BitcoinSLogger.logger

  def getInfo: Future[JsValue] = {
    val request = buildRequest("getinfo", JsArray.empty)
    val resonseF = sendRequest(request)

    resonseF.flatMap(getPayload)
  }

  private def getPayload(response: HttpResponse): Future[JsValue] = {
    val payloadF = response.entity.dataBytes.runFold(ByteString.empty)(_ ++ _)

    payloadF.map { payload =>
      Json.parse(payload.decodeString(ByteString.UTF_8))
    }
  }

  def sendRequest(req: HttpRequest): Future[HttpResponse] = {
    Http(m.system).singleRequest(req)
  }

  def buildRequest(methodName: String, params: JsArray): HttpRequest = {
    val uuid = UUID.randomUUID().toString

    val obj: JsObject = JsObject(Map(
      "method" -> JsString(methodName),
      "params" -> params,
      "id" -> JsString(uuid)))

    val uri = "http://localhost:9736"
    val username = "suredbits"
    val password = "abc123"
    HttpRequest(
      method = HttpMethods.POST,
      uri,
      entity = HttpEntity(ContentTypes.`application/json`, obj.toString))
      .addCredentials(HttpCredentials.createBasicHttpCredentials(username, password))
  }
}
