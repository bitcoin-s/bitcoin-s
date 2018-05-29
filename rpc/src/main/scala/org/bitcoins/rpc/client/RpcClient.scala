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

class RpcClient {
  private val resultKey = "result"
  private val logger = BitcoinSLogger.logger

  def getBestBlockHash(implicit m: ActorMaterializer, ec: ExecutionContext): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getbestblockhash")
  }

  def getBlockCount(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Int] = {
    bitcoindCall[Int]("getblockcount")
  }

  def getConnectionCount(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Int] = {
    bitcoindCall[Int]("getconnectioncount")
  }

  def getMiningInfo(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetMiningInfoResult] = {
    bitcoindCall[GetMiningInfoResult]("getmininginfo")
  }

  def getChainTips(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[ChainTip]] = {
    bitcoindCall[Array[ChainTip]]("getchaintips")
  }

  def getNetworkInfo(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetNetworkInfoResult] = {
    bitcoindCall[GetNetworkInfoResult]("getnetworkinfo")
  }

  def getNewAddress(account: String = "")(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Address] = {
    bitcoindCall[Address]("getnewaddress", JsArray(List(JsString(account))))
  }

  def getBlockHeaderRaw(headerHash: DoubleSha256Digest)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[BlockHeader] = {
    bitcoindCall[BlockHeader]("getblockheader", JsArray(List(JsString(headerHash.hex), JsBoolean(false))))
  }

  def getBlockHeader(headerHash: DoubleSha256Digest)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetBlockHeaderResult] = {
    bitcoindCall[GetBlockHeaderResult]("getblockheader", JsArray(List(JsString(headerHash.hex), JsBoolean(true))))
  }

  def generate(blocks: Int, maxTries: Int = 1000000)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[DoubleSha256Digest]] = {
    bitcoindCall[Array[DoubleSha256Digest]]("generate", JsArray(List(JsNumber(blocks), JsNumber(maxTries))))
  }

  private def bitcoindCall[T](command: String, parameters: JsArray = JsArray.empty)
                             (implicit m: ActorMaterializer, ec: ExecutionContext, reader: Reads[T]): Future[T] = {
    val request = buildRequest(command, parameters)
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
