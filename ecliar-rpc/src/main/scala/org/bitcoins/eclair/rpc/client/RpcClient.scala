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

class RpcClient()(implicit m: ActorMaterializer) {
  private val resultKey = "result"
  private val errorKey = "error"
  implicit val ec: ExecutionContext = m.executionContext
  private val logger = BitcoinSLogger.logger

  case class GetInfoResult(nodeId: String, alias: String, port: Int, chainHash: String, blockHeight: Long)
  implicit val getInfoResultReads: Reads[GetInfoResult] = Json.reads[GetInfoResult]

  case class PeerInfo(nodeId: String, state: String, address: String, channels: Int)
  implicit val peerInfoReads: Reads[PeerInfo] = Json.reads[PeerInfo]

  case class ChannelInfo(nodeId: String, channelId: String, state: String)
  implicit val channelInfoReads: Reads[ChannelInfo] = Json.reads[ChannelInfo]

  case class ChannelResult(nodeId: String, shortChannelId: String, channelId: String, state: String, balanceMsat: Long, capacitySat: Option[Long])
  implicit val channelResultReads: Reads[ChannelResult] = Json.reads[ChannelResult]

  case class NodeInfo(signature: String, features: String, timestamp: Long, nodeId: String, rgbColor: String, alias: String, addresses: Vector[String])
  implicit val nodeInfoReads: Reads[NodeInfo] = Json.reads[NodeInfo]

  case class ChannelDesc(shortChannelId: String, a: String, b: String)
  implicit val channelDescReads: Reads[ChannelDesc] = Json.reads[ChannelDesc]

  case class ChannelUpdate(signature: String, chainHash: String, shortChannelId: String, timestamp: Long, flags: String, cltvExpiryDelta: Int, htlcMinimumMsat: Long, feeBaseMsat: Long, feeProportionalMillionths: Long)
  implicit val channelUpdateReads: Reads[ChannelUpdate] = Json.reads[ChannelUpdate]

  case class PaymentRequest(prefix: String, amount: Option[Long], timestamp: Long, nodeId: String, tags: Vector[JsObject], signature: String)
  implicit val paymentRequestReads: Reads[PaymentRequest] = Json.reads[PaymentRequest]

  sealed trait SendResult
  case class PaymentSucceeded(amountMsat: Long, paymentHash: String, paymentPreimage: String, route: JsArray) extends SendResult
  implicit val paymentSucceededReads: Reads[PaymentSucceeded] = Json.reads[PaymentSucceeded]
  implicit val sendResultReads: Reads[SendResult] = Reads[SendResult](_.validate[PaymentSucceeded])
  /*
  case class PaymentFailure(???) extends SendResult
  implicit val paymentFailureReads: Reads[PaymentFailure] = Json.reads[PaymentFailure]
  implicit val sendResultReads: Reads[SendResult] = Reads[SendResult] { json =>
    json.validate[PaymentSucceeded] match {
      case success: JsSuccess[PaymentSucceeded] => success
      case err1: JsError => json.validate[PaymentFailure] match {
        case failure: JsSuccess[PaymentFailure] => failure
        case err2: JsError => JsError.merge(err1, err2)
      }
    }
  }*/

  def allChannels: Future[Vector[ChannelDesc]] = {
    eclairCall[Vector[ChannelDesc]]("allchannels")
  }

  def allNodes: Future[Vector[NodeInfo]] = {
    eclairCall[Vector[NodeInfo]]("allnodes")
  }

  private def allUpdates(nodeId: Option[String]): Future[Vector[ChannelUpdate]] = {
    val params = if (nodeId.isEmpty) List.empty else List(JsString(nodeId.get))
    eclairCall[Vector[ChannelUpdate]]("allUpdates", params)
  }

  def allUpdates: Future[Vector[ChannelUpdate]] = allUpdates(None)

  def allUpdates(nodeId: String): Future[Vector[ChannelUpdate]] = allUpdates(Some(nodeId))

  def channel(channelId: String): Future[ChannelResult] = {
    eclairCall[ChannelResult]("channel", List(JsString(channelId)))
  }

  private def channels(nodeId: Option[String]): Future[Vector[ChannelInfo]] = {
    val params = if (nodeId.isEmpty) List.empty else List(JsString(nodeId.get))

    eclairCall[Vector[ChannelInfo]]("channels", params)
  }

  def channels: Future[Vector[ChannelInfo]] = channels(None)

  def channels(nodeId: String): Future[Vector[ChannelInfo]] = channels(Some(nodeId))

  def checkInvoice(invoice: String): Future[PaymentRequest] = {
    eclairCall[PaymentRequest]("checkinvoice", List(JsString(invoice)))
  }

  // When types are introduced this can be two different functions
  def checkPayment(invoiceOrHash: String): Future[Boolean] = {
    eclairCall[Boolean]("checkpayment", List(JsString(invoiceOrHash)))
  }

  private def close(channelId: String, scriptPubKey: Option[String]): Future[String] = {
    val params =
      if (scriptPubKey.isEmpty) {
        List(JsString(channelId))
      } else {
        List(JsString(channelId), JsString(scriptPubKey.get))
      }

    eclairCall[String]("close", params)
  }

  def close(channelId: String): Future[String] = close(channelId, None)

  def close(channelId: String, scriptPubKey: String): Future[String] = close(channelId, Some(scriptPubKey))

  def connect(nodeId: String, host: String, port: Int): Future[String] = {
    eclairCall[String]("connect", List(JsString(nodeId), JsString(host), JsNumber(port)))
  }

  def connect(uri: String): Future[String] = {
    eclairCall[String]("connect", List(JsString(uri)))
  }

  // When types are introduced this can be two different functions
  def findRoute(nodeIdOrInvoice: String): Future[Vector[String]] = {
    eclairCall[Vector[String]]("findroute", List(JsString(nodeIdOrInvoice)))
  }

  def forceClose(channelId: String): Future[String] = {
    eclairCall[String]("forceclose", List(JsString(channelId)))
  }

  def getInfo: Future[GetInfoResult] = {
    eclairCall[GetInfoResult]("getinfo")
  }

  def help: Future[Vector[String]] = {
    eclairCall[Vector[String]]("help")
  }

  private def open(
    nodeId: String,
    fundingSatoshis: Long,
    pushMsat: Option[Long],
    feerateSatPerByte: Option[Long],
    channelFlags: Option[Byte]): Future[String] = {
    val num: Long = pushMsat.getOrElse(0)
    val pushMsatJson = JsNumber(num)

    val params =
      if (feerateSatPerByte.isEmpty) {
        List(
          JsString(nodeId),
          JsNumber(fundingSatoshis),
          pushMsatJson)
      } else if (channelFlags.isEmpty) {
        List(
          JsString(nodeId),
          JsNumber(fundingSatoshis),
          pushMsatJson,
          JsNumber(feerateSatPerByte.get))
      } else {
        List(
          JsString(nodeId),
          JsNumber(fundingSatoshis),
          pushMsatJson,
          JsNumber(feerateSatPerByte.get),
          JsString(channelFlags.toString))
      }

    eclairCall[String]("open", params)
  }

  def open(nodeId: String, fundingSatoshis: Long): Future[String] =
    open(nodeId, fundingSatoshis, None, None, None)

  def open(nodeId: String, fundingSatoshis: Long, pushMsat: Long): Future[String] =
    open(nodeId, fundingSatoshis, Some(pushMsat), None, None)

  def open(nodeId: String, fundingSatoshis: Long, pushMsat: Long, feerateSatPerByte: Long): Future[String] =
    open(nodeId, fundingSatoshis, Some(pushMsat), Some(feerateSatPerByte), None)

  def open(
    nodeId: String,
    fundingSatoshis: Long,
    pushMsat: Long = 0,
    feerateSatPerByte: Long,
    channelFlags: Byte): Future[String] =
    open(nodeId, fundingSatoshis, Some(pushMsat), Some(feerateSatPerByte), Some(channelFlags))

  def open(
    nodeId: String,
    fundingSatoshis: Long,
    feerateSatPerByte: Long,
    channelFlags: Byte): Future[String] =
    open(nodeId, fundingSatoshis, None, Some(feerateSatPerByte), Some(channelFlags))

  def peers: Future[Vector[PeerInfo]] = {
    eclairCall[Vector[PeerInfo]]("peers")
  }

  private def receive(description: Option[String], amountMsat: Option[Long], expirySeconds: Option[Long]): Future[String] = {
    val params =
      if (amountMsat.isEmpty) {
        List(JsString(description.getOrElse("")))
      } else if (expirySeconds.isEmpty) {
        List(JsNumber(amountMsat.get), JsString(description.getOrElse("")))
      } else {
        List(JsNumber(amountMsat.get), JsString(description.getOrElse("")), JsNumber(expirySeconds.get))
      }

    eclairCall[String]("receive", params)
  }

  def receive(): Future[String] =
    receive(None, None, None)

  def receive(description: String): Future[String] =
    receive(Some(description), None, None)

  def receive(description: String, amountMsat: Long): Future[String] =
    receive(Some(description), Some(amountMsat), None)

  def receive(description: String, amountMsat: Long, expirySeconds: Long): Future[String] =
    receive(Some(description), Some(amountMsat), Some(expirySeconds))

  def receive(amountMsat: Long): Future[String] =
    receive(None, Some(amountMsat), None)

  def receive(amountMsat: Long, expirySeconds: Long): Future[String] =
    receive(None, Some(amountMsat), Some(expirySeconds))

  def send(amountMsat: Long, paymentHash: String, nodeId: String): Future[SendResult] = {
    eclairCall[SendResult]("send", List(
      JsNumber(amountMsat),
      JsString(paymentHash),
      JsString(nodeId)))
  }

  private def send(invoice: String, amountMsat: Option[Long]): Future[SendResult] = {
    val params =
      if (amountMsat.isEmpty) {
        List(JsString(invoice))
      } else {
        List(JsString(invoice), JsNumber(amountMsat.get))
      }

    eclairCall[SendResult]("send", params)
  }

  def send(invoice: String): Future[SendResult] = send(invoice, None)

  def send(invoice: String, amountMsat: Long): Future[SendResult] = send(invoice, Some(amountMsat))

  def updateRelayFee(channelId: String, feeBaseMsat: Long, feeProportionalMillionths: Long): Future[String] = {
    eclairCall[String]("updaterelayfee", List(
      JsString(channelId),
      JsNumber(feeBaseMsat),
      JsNumber(feeProportionalMillionths)))
  }

  // TODO: channelstats, audit, networkfees?
  // TODO: Add types

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
