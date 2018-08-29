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
  private val resultKey = "result"
  private val errorKey = "error"
  implicit val ec: ExecutionContext = m.executionContext
  private val logger = BitcoinSLogger.logger

  // Errors for Unit return types are caught in RpcClient::checkUnit
  implicit object UnitReads extends Reads[Unit] {
    override def reads(json: JsValue): JsResult[Unit] = JsSuccess(Unit)
  }

  case class GetInfoResult(nodeId: String, alias: String, port: Int, chainHash: String, blockHeight: Long)
  implicit val getInfoResultReads: Reads[GetInfoResult] = Json.reads[GetInfoResult]

  def connect(nodeId: String, host: String, port: Int): Future[Unit] = {
    eclairCall[Unit]("connect", List(JsString(nodeId), JsString(host), JsNumber(port)))
  }

  def connect(uri: String): Future[Unit] = {
    eclairCall[Unit]("connect", List(JsString(uri)))
  }

  def getInfo: Future[GetInfoResult] = {
    eclairCall[GetInfoResult]("getinfo")
  }

  def help: Future[List[String]] = {
    eclairCall[List[String]]("help")
  }

  private def eclairCall[T](
    command: String,
    parameters: List[JsValue] = List.empty)(implicit reader: Reads[T]): Future[T] = {
    val request = buildRequest(command, JsArray(parameters))
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      parseResult((payload \ resultKey).validate[T], payload)
    }
  }

  case class RpcError(code: Int, message: String)
  implicit val rpcErrorReads: Reads[RpcError] = Json.reads[RpcError]

  private def parseResult[T](result: JsResult[T], json: JsValue): T = {
    checkUnitError[T](result, json)

    result match {
      case res: JsSuccess[T] => res.value
      case res: JsError =>
        (json \ errorKey).validate[RpcError] match {
          case err: JsSuccess[RpcError] =>
            logger.error(s"Error ${err.value.code}: ${err.value.message}")
            throw new RuntimeException(
              s"Error ${err.value.code}: ${err.value.message}")
          case _: JsError =>
            logger.error(JsError.toJson(res).toString())
            throw new IllegalArgumentException(
              s"Could not parse JsResult: ${(json \ resultKey).get}")
        }
    }
  }

  // Catches errors thrown by calls with Unit as the expected return type (which isn't handled by UnitReads)
  private def checkUnitError[T](result: JsResult[T], json: JsValue): Unit = {
    if (result == JsSuccess(())) {
      (json \ errorKey).validate[RpcError] match {
        case err: JsSuccess[RpcError] =>
          logger.error(s"Error ${err.value.code}: ${err.value.message}")
          throw new RuntimeException(
            s"Error ${err.value.code}: ${err.value.message}")
        case _: JsError =>
      }
    }
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

    val uri = "http://localhost:8081"
    val username = "suredbits"
    val password = "abc123"
    HttpRequest(
      method = HttpMethods.POST,
      uri,
      entity = HttpEntity(ContentTypes.`application/json`, obj.toString))
      .addCredentials(HttpCredentials.createBasicHttpCredentials(username, password))
  }
}
