package org.bitcoins.rpc.client

import java.util.UUID

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels.Network
import org.slf4j.Logger
import play.api.libs.json._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

protected trait Client {
  protected def logger: Logger = BitcoinSLogger.logger

  protected implicit val executor: ExecutionContext
  protected implicit val materializer: ActorMaterializer

  protected val instance: BitcoindInstance
  protected implicit val network : NetworkParameters = instance.network

  protected val resultKey: String = "result"
  protected val errorKey : String = "error"


  protected def buildRequest(instance: BitcoindInstance,
                             methodName: String,
                             params: JsArray): HttpRequest = {
    val uuid = UUID.randomUUID().toString

    val m: Map[String, JsValue] = Map(
      "method" -> JsString(methodName),
      "params" -> params,
      "id" -> JsString(uuid))

    val jsObject = JsObject(m)

    logger.debug(s"json rpc request: $m")

    // Would toString work?
    val uri = "http://" + instance.rpcUri.getHost + ":" + instance.rpcUri.getPort
    val username = instance.authCredentials.username
    val password = instance.authCredentials.password
    HttpRequest(
      method = HttpMethods.POST,
      uri,
      entity = HttpEntity(ContentTypes.`application/json`, jsObject.toString()))
      .addCredentials(
        HttpCredentials.createBasicHttpCredentials(username, password))
  }

  protected def sendRequest(req: HttpRequest)(
    implicit materializer: ActorMaterializer
  ): Future[HttpResponse] = {
    Http(materializer.system).singleRequest(req)
  }

  def isStarted(implicit materializer: ActorMaterializer, executor: ExecutionContext): Boolean = {
    val request = buildRequest(instance, "ping", JsArray.empty)
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    // Ping successful if no error can be parsed from the payload
    val result = Try(Await.result(payloadF.map { payload =>
      (payload \ errorKey).validate[RpcError] match {
        case _: JsSuccess[RpcError] => false
        case _: JsError => true
      }
    }, 2.seconds))

    result.getOrElse(false)
  }

  protected def getPayload(response: HttpResponse)(
    implicit materializer: ActorMaterializer,
    executionContext: ExecutionContext
  ): Future[JsValue] = {
    val payloadF = response.entity.dataBytes.runFold(ByteString.empty)(_ ++ _)

    payloadF.map { payload =>
      Json.parse(payload.decodeString(ByteString.UTF_8))
    }
  }

  case class RpcError(code: Int, message: String)
  implicit val rpcErrorReads: Reads[RpcError] = Json.reads[RpcError]

}
