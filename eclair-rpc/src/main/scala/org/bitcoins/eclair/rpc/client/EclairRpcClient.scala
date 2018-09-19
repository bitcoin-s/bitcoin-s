package org.bitcoins.eclair.rpc.client

import java.util.UUID

import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.Sha256Digest
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.ln.channel.{ ChannelId, FundedChannelId }
import org.bitcoins.core.protocol.ln.{ LnCurrencyUnit, LnCurrencyUnits }
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.eclair.rpc.config.EclairInstance
import org.bitcoins.eclair.rpc.json._
import org.bitcoins.eclair.rpc.network.{ NodeId, NodeUri, PeerState }
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.sys.process._
import scala.util.Try

class EclairRpcClient(instance: EclairInstance)(implicit system: ActorSystem) {
  import JsonReaders._

  private val resultKey = "result"
  private val errorKey = "error"
  implicit val m = ActorMaterializer.create(system)
  implicit val ec: ExecutionContext = m.executionContext
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  def getDaemon: EclairInstance = instance

  def allChannels: Future[Vector[ChannelDesc]] = {
    eclairCall[Vector[ChannelDesc]]("allchannels")
  }

  def allNodes: Future[Vector[NodeInfo]] = {
    eclairCall[Vector[NodeInfo]]("allnodes")
  }

  private def allUpdates(
    nodeId: Option[NodeId]): Future[Vector[ChannelUpdate]] = {
    val params = if (nodeId.isEmpty) List.empty else List(JsString(nodeId.get.toString))
    eclairCall[Vector[ChannelUpdate]]("allupdates", params)
  }

  def allUpdates: Future[Vector[ChannelUpdate]] = allUpdates(None)

  def allUpdates(nodeId: NodeId): Future[Vector[ChannelUpdate]] =
    allUpdates(Some(nodeId))

  def channel(channelId: ChannelId): Future[ChannelResult] = {
    eclairCall[ChannelResult]("channel", List(JsString(channelId.hex)))
  }

  private def channels(nodeId: Option[NodeId]): Future[Vector[ChannelInfo]] = {
    val params = if (nodeId.isEmpty) List.empty else List(JsString(nodeId.get.toString))

    eclairCall[Vector[ChannelInfo]]("channels", params)
  }

  def channels: Future[Vector[ChannelInfo]] = channels(None)

  def channels(nodeId: NodeId): Future[Vector[ChannelInfo]] =
    channels(Some(nodeId))

  def checkInvoice(invoice: String): Future[PaymentRequest] = {
    eclairCall[PaymentRequest]("checkinvoice", List(JsString(invoice)))
  }

  // When types are introduced this can be two different functions
  def checkPayment(invoiceOrHash: String): Future[Boolean] = {
    eclairCall[Boolean]("checkpayment", List(JsString(invoiceOrHash)))
  }

  private def close(
    channelId: ChannelId,
    scriptPubKey: Option[String]): Future[String] = {
    val params =
      if (scriptPubKey.isEmpty) {
        List(JsString(channelId.hex))
      } else {
        List(JsString(channelId.hex), JsString(scriptPubKey.get))
      }

    eclairCall[String]("close", params)
  }

  def close(channelId: ChannelId): Future[String] = close(channelId, None)

  def close(channelId: ChannelId, scriptPubKey: String): Future[String] =
    close(channelId, Some(scriptPubKey))

  def connect(nodeId: NodeId, host: String, port: Int): Future[String] = {
    val uri = NodeUri(nodeId, host, port)
    connect(uri)
  }

  def connect(uri: NodeUri): Future[String] = {
    logger.info(s"Connecting to $uri")
    eclairCall[String]("connect", List(JsString(uri.toString)))
  }

  // When types are introduced this can be two different functions
  def findRoute(nodeIdOrInvoice: String): Future[Vector[String]] = {
    eclairCall[Vector[String]]("findroute", List(JsString(nodeIdOrInvoice)))
  }

  def forceClose(channelId: ChannelId): Future[String] = {
    eclairCall[String]("forceclose", List(JsString(channelId.hex)))
  }

  def getInfo: Future[GetInfoResult] = {
    val result = eclairCall[GetInfoResult]("getinfo")
    result
  }

  def help: Future[Vector[String]] = {
    eclairCall[Vector[String]]("help")
  }

  def isConnected(nodeId: NodeId): Future[Boolean] = {
    peers.map(_.exists(p => p.nodeId == nodeId && p.state == PeerState.CONNECTED))
  }

  private def open(
    nodeId: NodeId,
    fundingSatoshis: CurrencyUnit,
    pushMsat: Option[LnCurrencyUnit],
    feerateSatPerByte: Option[SatoshisPerByte],
    channelFlags: Option[Byte]): Future[FundedChannelId] = {
    val num = pushMsat.getOrElse(LnCurrencyUnits.zero).toPicoBitcoinDecimal
    val pushMsatJson = JsNumber(num)
    val sat = fundingSatoshis.satoshis.toBigDecimal

    val params = {
      if (feerateSatPerByte.isEmpty) {
        List(JsString(nodeId.toString), JsNumber(sat), pushMsatJson)
      } else if (channelFlags.isEmpty) {
        List(
          JsString(nodeId.toString),
          JsNumber(sat),
          pushMsatJson,
          JsNumber(feerateSatPerByte.get.toLong))
      } else {
        List(
          JsString(nodeId.toString),
          JsNumber(sat),
          pushMsatJson,
          JsNumber(feerateSatPerByte.get.toLong),
          JsString(channelFlags.toString))
      }
    }

    //this is unfortunately returned in this format
    //created channel 30bdf849eb9f72c9b41a09e38a6d83138c2edf332cb116dd7cf0f0dfb66be395
    val call = eclairCall[String]("open", params)

    //let's just return the chanId
    val chanIdF = call.map(_.split(" ").last)

    chanIdF.map(FundedChannelId.fromHex(_))
  }

  def open(nodeId: NodeId, fundingSatoshis: CurrencyUnit): Future[FundedChannelId] = {
    open(nodeId, fundingSatoshis, None, None, None)
  }

  def open(
    nodeId: NodeId,
    fundingSatoshis: CurrencyUnit,
    pushMsat: LnCurrencyUnit): Future[FundedChannelId] = {
    open(nodeId, fundingSatoshis, Some(pushMsat), None, None)
  }

  def open(
    nodeId: NodeId,
    fundingSatoshis: CurrencyUnit,
    pushMsat: LnCurrencyUnit,
    feerateSatPerByte: SatoshisPerByte): Future[FundedChannelId] = {
    open(nodeId, fundingSatoshis, Some(pushMsat), Some(feerateSatPerByte), None)
  }

  def open(
    nodeId: NodeId,
    fundingSatoshis: CurrencyUnit,
    pushMsat: LnCurrencyUnit = LnCurrencyUnits.zero,
    feerateSatPerByte: SatoshisPerByte,
    channelFlags: Byte): Future[FundedChannelId] = {
    open(
      nodeId,
      fundingSatoshis,
      Some(pushMsat),
      Some(feerateSatPerByte),
      Some(channelFlags))
  }

  def open(
    nodeId: NodeId,
    fundingSatoshis: CurrencyUnit,
    feerateSatPerByte: SatoshisPerByte,
    channelFlags: Byte): Future[FundedChannelId] = {
    open(
      nodeId,
      fundingSatoshis,
      None,
      Some(feerateSatPerByte),
      Some(channelFlags))
  }

  def peers: Future[Vector[PeerInfo]] = {
    eclairCall[Vector[PeerInfo]]("peers")
  }

  private def receive(
    description: Option[String],
    amountMsat: Option[LnCurrencyUnit],
    expirySeconds: Option[Long]): Future[String] = {
    val params =
      if (amountMsat.isEmpty) {
        List(JsString(description.getOrElse("")))
      } else if (expirySeconds.isEmpty) {
        List(JsNumber(amountMsat.get.toPicoBitcoinDecimal), JsString(description.getOrElse("")))
      } else {
        List(
          JsNumber(amountMsat.get.toPicoBitcoinDecimal),
          JsString(description.getOrElse("")),
          JsNumber(expirySeconds.get))
      }

    eclairCall[String]("receive", params)
  }

  def receive(): Future[String] =
    receive(None, None, None)

  def receive(description: String): Future[String] =
    receive(Some(description), None, None)

  def receive(description: String, amountMsat: LnCurrencyUnit): Future[String] =
    receive(Some(description), Some(amountMsat), None)

  def receive(
    description: String,
    amountMsat: LnCurrencyUnit,
    expirySeconds: Long): Future[String] =
    receive(Some(description), Some(amountMsat), Some(expirySeconds))

  def receive(amountMsat: LnCurrencyUnit): Future[String] =
    receive(None, Some(amountMsat), None)

  def receive(amountMsat: LnCurrencyUnit, expirySeconds: Long): Future[String] =
    receive(None, Some(amountMsat), Some(expirySeconds))

  def send(
    amountMsat: LnCurrencyUnit,
    paymentHash: Sha256Digest,
    nodeId: NodeId): Future[PaymentResult] = {
    eclairCall[PaymentResult](
      "send",
      List(JsNumber(amountMsat.toPicoBitcoinDecimal), JsString(paymentHash.hex), JsString(nodeId.toString)))
  }

  private def send(
    invoice: String,
    amountMsat: Option[LnCurrencyUnit]): Future[PaymentResult] = {
    val params =
      if (amountMsat.isEmpty) {
        List(JsString(invoice))
      } else {
        List(JsString(invoice), JsNumber(amountMsat.get.toPicoBitcoinDecimal))
      }

    eclairCall[PaymentResult]("send", params)
  }

  def send(invoice: String): Future[PaymentResult] = send(invoice, None)

  def send(invoice: String, amountMsat: LnCurrencyUnit): Future[PaymentResult] =
    send(invoice, Some(amountMsat))

  def updateRelayFee(
    channelId: ChannelId,
    feeBaseMsat: LnCurrencyUnit,
    feeProportionalMillionths: Long): Future[String] = {
    eclairCall[String](
      "updaterelayfee",
      List(
        JsString(channelId.hex),
        JsNumber(feeBaseMsat.toPicoBitcoinDecimal),
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
      val validated: JsResult[T] = (payload \ resultKey).validate[T]
      val parsed: T = parseResult(validated, payload)
      parsed
    }
  }

  case class RpcError(code: Int, message: String)
  implicit val rpcErrorReads: Reads[RpcError] = Json.reads[RpcError]

  private def parseResult[T](result: JsResult[T], json: JsValue): T = {
    result match {
      case res: JsSuccess[T] =>
        res.value
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
      val parsed: JsValue = Json.parse(payload.decodeString(ByteString.UTF_8))
      parsed
    }
  }

  private def sendRequest(req: HttpRequest): Future[HttpResponse] = {
    val respF = Http(m.system).singleRequest(req)
    respF
  }

  private def buildRequest(instance: EclairInstance, methodName: String, params: JsArray): HttpRequest = {
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
    val path = System.getenv("ECLAIR_PATH")
    path + "/eclair-node-0.2-beta5-8aa51f4.jar"
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
