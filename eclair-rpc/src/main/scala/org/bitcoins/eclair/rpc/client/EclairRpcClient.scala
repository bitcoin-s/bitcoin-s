package org.bitcoins.eclair.rpc.client

import java.util.UUID

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.config.EclairInstance
import org.bitcoins.eclair.rpc.json._
import org.bitcoins.rpc.RpcUtil
import play.api.libs.json._

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.DurationInt
import scala.sys.process._
import scala.util.Try

class EclairRpcClient(instance: EclairInstance)(implicit m: ActorMaterializer) {
  import JsonReaders._

  private val resultKey = "result"
  private val errorKey = "error"
  implicit val ec: ExecutionContext = m.executionContext
  private val logger = BitcoinSLogger.logger

  def getDaemon: EclairInstance = instance

  def allChannels: Future[Vector[ChannelDesc]] = {
    eclairCall[Vector[ChannelDesc]]("allchannels")
  }

  def allNodes: Future[Vector[NodeInfo]] = {
    eclairCall[Vector[NodeInfo]]("allnodes")
  }

  private def allUpdates(
    nodeId: Option[String]): Future[Vector[ChannelUpdate]] = {
    val params = if (nodeId.isEmpty) List.empty else List(JsString(nodeId.get))
    eclairCall[Vector[ChannelUpdate]]("allupdates", params)
  }

  def allUpdates: Future[Vector[ChannelUpdate]] = allUpdates(None)

  def allUpdates(nodeId: String): Future[Vector[ChannelUpdate]] =
    allUpdates(Some(nodeId))

  def channel(channelId: String): Future[ChannelResult] = {
    eclairCall[ChannelResult]("channel", List(JsString(channelId)))
  }

  private def channels(nodeId: Option[String]): Future[Vector[ChannelInfo]] = {
    val params = if (nodeId.isEmpty) List.empty else List(JsString(nodeId.get))

    eclairCall[Vector[ChannelInfo]]("channels", params)
  }

  def channels: Future[Vector[ChannelInfo]] = channels(None)

  def channels(nodeId: String): Future[Vector[ChannelInfo]] =
    channels(Some(nodeId))

  def checkInvoice(invoice: String): Future[PaymentRequest] = {
    eclairCall[PaymentRequest]("checkinvoice", List(JsString(invoice)))
  }

  // When types are introduced this can be two different functions
  def checkPayment(invoiceOrHash: String): Future[Boolean] = {
    eclairCall[Boolean]("checkpayment", List(JsString(invoiceOrHash)))
  }

  private def close(
    channelId: String,
    scriptPubKey: Option[String]): Future[String] = {
    val params =
      if (scriptPubKey.isEmpty) {
        List(JsString(channelId))
      } else {
        List(JsString(channelId), JsString(scriptPubKey.get))
      }

    eclairCall[String]("close", params)
  }

  def close(channelId: String): Future[String] = close(channelId, None)

  def close(channelId: String, scriptPubKey: String): Future[String] =
    close(channelId, Some(scriptPubKey))

  def connect(nodeId: String, host: String, port: Int): Future[String] = {
    logger.info(s"Connecting to ${nodeId}@${host}:${port}")
    eclairCall[String](
      "connect",
      List(JsString(nodeId), JsString(host), JsNumber(port)))
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

  def isConnected(nodeId: String): Future[Boolean] = {
    peers.map(_.exists(_.nodeId == nodeId))
  }

  private def open(
    nodeId: String,
    fundingSatoshis: Long,
    pushMsat: Option[Long],
    feerateSatPerByte: Option[Long],
    channelFlags: Option[Byte]): Future[String] = {
    val num: Long = pushMsat.getOrElse(0)
    val pushMsatJson = JsNumber(num)

    val params = {
      if (feerateSatPerByte.isEmpty) {
        List(JsString(nodeId), JsNumber(fundingSatoshis), pushMsatJson)
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
    }

    //this is unfortunately returned in this format
    //created channel 30bdf849eb9f72c9b41a09e38a6d83138c2edf332cb116dd7cf0f0dfb66be395
    val call = eclairCall[String]("open", params)

    //let's just return the chanId
    val chanId = call.map(_.split(" ").last)

    chanId
  }

  def open(nodeId: String, fundingSatoshis: Long): Future[String] =
    open(nodeId, fundingSatoshis, None, None, None)

  def open(
    nodeId: String,
    fundingSatoshis: Long,
    pushMsat: Long): Future[String] =
    open(nodeId, fundingSatoshis, Some(pushMsat), None, None)

  def open(
    nodeId: String,
    fundingSatoshis: Long,
    pushMsat: Long,
    feerateSatPerByte: Long): Future[String] =
    open(nodeId, fundingSatoshis, Some(pushMsat), Some(feerateSatPerByte), None)

  def open(
    nodeId: String,
    fundingSatoshis: Long,
    pushMsat: Long = 0,
    feerateSatPerByte: Long,
    channelFlags: Byte): Future[String] =
    open(
      nodeId,
      fundingSatoshis,
      Some(pushMsat),
      Some(feerateSatPerByte),
      Some(channelFlags))

  def open(
    nodeId: String,
    fundingSatoshis: Long,
    feerateSatPerByte: Long,
    channelFlags: Byte): Future[String] =
    open(
      nodeId,
      fundingSatoshis,
      None,
      Some(feerateSatPerByte),
      Some(channelFlags))

  def peers: Future[Vector[PeerInfo]] = {
    eclairCall[Vector[PeerInfo]]("peers")
  }

  private def receive(
    description: Option[String],
    amountMsat: Option[Long],
    expirySeconds: Option[Long]): Future[String] = {
    val params =
      if (amountMsat.isEmpty) {
        List(JsString(description.getOrElse("")))
      } else if (expirySeconds.isEmpty) {
        List(JsNumber(amountMsat.get), JsString(description.getOrElse("")))
      } else {
        List(
          JsNumber(amountMsat.get),
          JsString(description.getOrElse("")),
          JsNumber(expirySeconds.get))
      }

    eclairCall[String]("receive", params)
  }

  def receive(): Future[String] =
    receive(None, None, None)

  def receive(description: String): Future[String] =
    receive(Some(description), None, None)

  def receive(description: String, amountMsat: Long): Future[String] =
    receive(Some(description), Some(amountMsat), None)

  def receive(
    description: String,
    amountMsat: Long,
    expirySeconds: Long): Future[String] =
    receive(Some(description), Some(amountMsat), Some(expirySeconds))

  def receive(amountMsat: Long): Future[String] =
    receive(None, Some(amountMsat), None)

  def receive(amountMsat: Long, expirySeconds: Long): Future[String] =
    receive(None, Some(amountMsat), Some(expirySeconds))

  def send(
    amountMsat: Long,
    paymentHash: String,
    nodeId: String): Future[SendResult] = {
    eclairCall[SendResult](
      "send",
      List(JsNumber(amountMsat), JsString(paymentHash), JsString(nodeId)))
  }

  private def send(
    invoice: String,
    amountMsat: Option[Long]): Future[SendResult] = {
    val params =
      if (amountMsat.isEmpty) {
        List(JsString(invoice))
      } else {
        List(JsString(invoice), JsNumber(amountMsat.get))
      }

    eclairCall[SendResult]("send", params)
  }

  def send(invoice: String): Future[SendResult] = send(invoice, None)

  def send(invoice: String, amountMsat: Long): Future[SendResult] =
    send(invoice, Some(amountMsat))

  def updateRelayFee(
    channelId: String,
    feeBaseMsat: Long,
    feeProportionalMillionths: Long): Future[String] = {
    eclairCall[String](
      "updaterelayfee",
      List(
        JsString(channelId),
        JsNumber(feeBaseMsat),
        JsNumber(feeProportionalMillionths)))
  }

  // TODO: channelstats, audit, networkfees?
  // TODO: Add types

  private def eclairCall[T](
    command: String,
    parameters: List[JsValue] = List.empty)(
    implicit
    reader: Reads[T]): Future[T] = {
    val request = buildRequest(getDaemon, command, JsArray(parameters))
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

  def buildRequest(instance: EclairInstance, methodName: String, params: JsArray): HttpRequest = {
    val uuid = UUID.randomUUID().toString

    val obj: JsObject = JsObject(
      Map(
        "method" -> JsString(methodName),
        "params" -> params,
        "id" -> JsString(uuid)))

    val uri = "http://" + instance.rpcUri.getHost + ":" + instance.rpcUri.getPort
    val username = instance.authCredentials.username
    val password = instance.authCredentials.password
    HttpRequest(
      method = HttpMethods.POST,
      uri,
      entity =
        HttpEntity(ContentTypes.`application/json`, obj.toString))
      .addCredentials(
        HttpCredentials.createBasicHttpCredentials(username, password))
  }

  // TODO: THIS IS ALL HACKY

  private def pathToEclairJar: String = {
    System.getenv("ECLAIR_PATH") + "/eclair-node-0.2-beta5-8aa51f4.jar"
  }

  private var process: Option[Process] = None

  def start(): Unit = {

    if (process.isEmpty) {
      val p = Process("java -jar -Declair.datadir=" + instance.authCredentials.datadir + s" ${pathToEclairJar} &")
      val result = p.run()
      logger.info(s"Starting eclair with datadir ${instance.authCredentials.datadir}")

      process = Some(result)
      ()
    } else {
      logger.info(s"Eclair was already started!")
      ()
    }

  }

  def isStarted(): Boolean = {
    val t = Try(Await.result(getInfo, 1.second))
    t.isSuccess
  }

  def stop(): Option[Unit] = {
    process.map(_.destroy())
  }
}
