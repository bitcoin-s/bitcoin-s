package org.bitcoins.rpc

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import org.bitcoins.core.protocol.Address
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.util.BitcoinSLogger

import scala.util.Try

class RpcClient {
  private val resultKey = "result"
  private val logger = BitcoinSLogger.logger

  def getBestBlockHash(implicit m: ActorMaterializer, ec: ExecutionContext): Future[DoubleSha256Digest] = {
    val request = buildRequest("getbestblockhash", JsArray.empty)
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      val result: JsResult[String] = (payload \ resultKey).validate[String]
      DoubleSha256Digest.fromHex(parseResult(result))
    }
  }

  def getBlockCount(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Int] = {
    val request = buildRequest("getblockcount", JsArray.empty)
    val reseponseF = sendRequest(request)

    val payloadF: Future[JsValue] = reseponseF.flatMap(getPayload)

    payloadF.map { payload =>
      val result: JsResult[Int] = (payload \ resultKey).validate[Int]
      parseResult(result)
    }
  }

  def getConnectionCount(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Int] = {
    val request = buildRequest("getconnectioncount", JsArray.empty)
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      val result: JsResult[Int] = (payload \ resultKey).validate[Int]
      parseResult(result)
    }
  }

  def getNewAddress(account: Option[String] = None)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Address] = {
    val parameters = List(JsString(account.getOrElse("")))
    val request = buildRequest("getnewaddress", JsArray(parameters))
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    val addressF: Future[Try[Address]] = payloadF.map { payload =>
      val result: JsResult[String] = (payload \ resultKey).validate[String]
      Address.fromString(parseResult(result))
    }

    addressF.flatMap { f =>
      Future.fromTry(f)
    }
  }

  private def parseResult[T](result: JsResult[T]): T = {
    result match {
      case res: JsSuccess[T] => res.value
      case res: JsError =>
        logger.error(JsError.toJson(res).toString())
        throw new IllegalArgumentException(s"Could not parse JsResult: ${res}")
    }
  }

  private def getPayload(response: HttpResponse)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[JsValue] = {
    val payloadF = response.entity.dataBytes.runFold(ByteString(""))(_ ++ _)

    payloadF.map { payload =>
      Json.parse(payload.decodeString(ByteString.UTF_8))
    }
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
