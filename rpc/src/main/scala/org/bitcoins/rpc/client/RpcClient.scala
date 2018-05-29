package org.bitcoins.rpc.client

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.BitcoinSLogger
import play.api.libs.json._
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._

import scala.concurrent.{ ExecutionContext, Future }
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
    noParameterCall[Int]("getblockcount")
  }

  def getConnectionCount(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Int] = {
    noParameterCall[Int]("getconnectioncount")
  }

  def getMiningInfo(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetMiningInfoResult] = {
    noParameterCall[GetMiningInfoResult]("getmininginfo")
  }

  def getChainTips(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[ChainTip]] = {
    noParameterCall[Array[ChainTip]]("getchaintips")
  }

  def getNetworkInfo(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetNetworkInfoResult] = {
    noParameterCall[GetNetworkInfoResult]("getnetworkinfo")
  }

  def getNewAddress(account: String = "")(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Address] = {
    val parameters = List(JsString(account))
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

  def getBlockHeaderRaw(headerHash: DoubleSha256Digest)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[BlockHeader] = {
    val parameters = List(JsString(headerHash.hex), JsBoolean(false))
    val request = buildRequest("getblockheader", JsArray(parameters))
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      parseResult((payload \ resultKey).validate[BlockHeader])
    }
  }

  def getBlockHeader(headerHash: DoubleSha256Digest)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetBlockHeaderResult] = {
    val parameters = List(JsString(headerHash.hex), JsBoolean(true))
    val request = buildRequest("getblockheader", JsArray(parameters))
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      parseResult((payload \ resultKey).validate[GetBlockHeaderResult])
    }
  }

  def generate(blocks: Int, maxTries: Int = 1000000)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[DoubleSha256Digest]] = {
    val parameters = List(JsNumber(blocks), JsNumber(maxTries))
    val request = buildRequest("generate", JsArray(parameters))
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      parseResult((payload \ resultKey).validate[Array[DoubleSha256Digest]])
    }
  }

  private def noParameterCall[T](command: String)(implicit m: ActorMaterializer, ec: ExecutionContext, reader: Reads[T]): Future[T] = {
    val request = buildRequest(command, JsArray.empty)
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      parseResult((payload \ resultKey).validate[T])
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
