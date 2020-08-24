package org.bitcoins.eclair.rpc.client

import java.io.File
import java.net.InetSocketAddress
import java.nio.file.NoSuchFileException
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

import akka.Done
import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString
import org.bitcoins.commons.jsonmodels.eclair._
import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.ln.channel.{ChannelId, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.ln.{
  LnInvoice,
  LnParams,
  PaymentPreimage,
  ShortChannelId
}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.{Address, BitcoinAddress}
import org.bitcoins.core.util.{BytesUtil, FutureUtil, StartStopAsync}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.crypto.{DoubleSha256DigestBE, Sha256Digest}
import org.bitcoins.eclair.rpc.api._
import org.bitcoins.eclair.rpc.config.EclairInstance
import org.bitcoins.eclair.rpc.network.NodeUri
import org.bitcoins.rpc.util.AsyncUtil
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.sys.process._
import scala.util.{Failure, Properties, Success}

/**
  * @param binary Path to Eclair Jar. If not present, reads
  *               environment variable `ECLAIR_PATH`
  */
class EclairRpcClient(
    val instance: EclairInstance,
    binary: Option[File] = None)(implicit system: ActorSystem)
    extends EclairApi
    with StartStopAsync[EclairRpcClient] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def getDaemon: EclairInstance = instance

  implicit override val executionContext: ExecutionContext = system.dispatcher

  override def allChannels(): Future[Vector[ChannelDesc]] = {
    eclairCall[Vector[ChannelDesc]]("allchannels")
  }

  override def allNodes(): Future[Vector[NodeInfo]] = {
    eclairCall[Vector[NodeInfo]]("nodes")
  }

  override def allUpdates(): Future[Vector[ChannelUpdate]] =
    eclairCall[Vector[ChannelUpdate]]("allupdates")

  override def allUpdates(nodeId: NodeId): Future[Vector[ChannelUpdate]] =
    eclairCall[Vector[ChannelUpdate]]("allupdates", "nodeId" -> nodeId.toString)

  /**
    * @inheritdoc
    */
  override def audit(): Future[AuditResult] =
    audit(None, None)

  /**
    * @inheritdoc
    */
  override def audit(
      from: Option[Instant],
      to: Option[Instant]): Future[AuditResult] =
    eclairCall[AuditResult](
      "audit",
      Seq(from.map(x => "from" -> x.getEpochSecond.toString),
          to.map(x => "to" -> x.getEpochSecond.toString)).flatten: _*)

  override def channel(channelId: ChannelId): Future[ChannelResult] = {
    eclairCall[ChannelResult]("channel", "channelId" -> channelId.hex)
  }

  private def channels(nodeId: Option[NodeId]): Future[Vector[ChannelInfo]] = {
    val params = Seq(nodeId.map(id => "nodeId" -> id.toString)).flatten
    eclairCall[Vector[ChannelInfo]]("channels", params: _*)
  }

  def channels(): Future[Vector[ChannelInfo]] = channels(nodeId = None)

  override def channels(nodeId: NodeId): Future[Vector[ChannelInfo]] =
    channels(Option(nodeId))

  private def close(
      channelId: ChannelId,
      shortChannelId: Option[ShortChannelId],
      scriptPubKey: Option[ScriptPubKey]): Future[ChannelCommandResult] = {
    val params =
      Seq("channelId" -> channelId.hex) ++ Seq(
        shortChannelId.map(x => "shortChannelId" -> x.toString),
        scriptPubKey.map(x => "scriptPubKey" -> BytesUtil.encodeHex(x.asmBytes))
      ).flatten

    eclairCall[ChannelCommandResult]("close", params: _*)
  }

  def close(channelId: ChannelId): Future[ChannelCommandResult] =
    close(channelId, scriptPubKey = None, shortChannelId = None)

  override def close(
      channelId: ChannelId,
      scriptPubKey: ScriptPubKey): Future[ChannelCommandResult] = {
    close(channelId, scriptPubKey = Some(scriptPubKey), shortChannelId = None)
  }

  override def connect(nodeId: NodeId, addr: InetSocketAddress): Future[Unit] =
    connect(nodeId, addr.getHostString, addr.getPort)

  override def connect(
      nodeId: NodeId,
      host: String,
      port: Int): Future[Unit] = {
    val uri = NodeUri(nodeId, host, port)
    connect(uri)
  }

  override def connect(uri: NodeUri): Future[Unit] = {
    eclairCall[String]("connect", "uri" -> uri.toString).map(_ => ())
  }

  override def connect(nodeId: NodeId): Future[Unit] = {
    eclairCall[String]("connect", "nodeId" -> nodeId.toString).map(_ => ())
  }

  override def findRoute(
      nodeId: NodeId,
      amountMsat: MilliSatoshis): Future[Vector[NodeId]] = {
    eclairCall[Vector[NodeId]]("findroutetonode",
                               "nodeId" -> nodeId.toString,
                               "amountMsat" -> amountMsat.toBigDecimal.toString)
  }

  override def findRoute(invoice: LnInvoice): Future[Vector[NodeId]] = {
    findRoute(invoice, None)
  }

  override def findRoute(
      invoice: LnInvoice,
      amount: MilliSatoshis): Future[Vector[NodeId]] = {
    findRoute(invoice, Some(amount))
  }

  def findRoute(
      invoice: LnInvoice,
      amountMsat: Option[MilliSatoshis]): Future[Vector[NodeId]] = {
    val params = Seq(
      Some("invoice" -> invoice.toString),
      amountMsat.map(x => "amountMsat" -> x.toBigDecimal.toString)).flatten
    eclairCall[Vector[NodeId]]("findroute", params: _*)
  }

  override def forceClose(
      channelId: ChannelId): Future[ChannelCommandResult] = {
    eclairCall[ChannelCommandResult]("forceclose", "channelId" -> channelId.hex)
  }

  override def forceClose(
      shortChannelId: ShortChannelId): Future[ChannelCommandResult] = {
    eclairCall[ChannelCommandResult](
      "forceclose",
      "shortChannelId" -> shortChannelId.toString)
  }

  override def getInfo: Future[GetInfoResult] = {
    val result = eclairCall[GetInfoResult]("getinfo")
    result
  }

  override def getNodeURI: Future[NodeUri] = {
    getInfo.map { info =>
      val id = info.nodeId
      val host = instance.uri.getHost
      val port = instance.uri.getPort
      NodeUri(nodeId = id, host = host, port = port)
    }
  }

  def help: Future[Vector[String]] = {
    eclairCall[Vector[String]]("help")
  }

  override def isConnected(nodeId: NodeId): Future[Boolean] = {
    getPeers.map(
      _.exists(p => p.nodeId == nodeId && p.state == PeerState.CONNECTED))
  }

  override def network: LnParams = {
    LnParams.fromNetworkParameters(instance.network)
  }

  override def open(
      nodeId: NodeId,
      funding: CurrencyUnit,
      pushMsat: Option[MilliSatoshis],
      feerateSatPerByte: Option[SatoshisPerByte],
      channelFlags: Option[Byte],
      openTimeout: Option[FiniteDuration]): Future[FundedChannelId] = {
    val fundingSatoshis = funding.satoshis.toBigDecimal.toString

    val params: Seq[(String, String)] =
      Seq("nodeId" -> nodeId.toString,
          "fundingSatoshis" -> fundingSatoshis) ++ Seq(
        pushMsat.map(x => "pushMsat" -> x.toBigDecimal.toString),
        feerateSatPerByte.map(x => "feerateSatPerByte" -> x.toLong.toString),
        channelFlags.map(x => "channelFlags" -> x.toString),
        openTimeout.map(x => "openTimeoutSeconds" -> x.toSeconds.toString)
      ).flatten

    //this is unfortunately returned in this format
    //created channel 30bdf849eb9f72c9b41a09e38a6d83138c2edf332cb116dd7cf0f0dfb66be395
    val call = eclairCall[String]("open", params: _*)

    //let's just return the chanId
    val chanIdF = call.map(_.split(" ").last)

    chanIdF.map(FundedChannelId.fromHex)
  }

  def open(nodeId: NodeId, funding: CurrencyUnit): Future[FundedChannelId] = {
    open(nodeId,
         funding,
         pushMsat = None,
         feerateSatPerByte = None,
         channelFlags = None,
         openTimeout = None)
  }

  def open(
      nodeId: NodeId,
      funding: CurrencyUnit,
      pushMsat: MilliSatoshis): Future[FundedChannelId] = {
    open(nodeId,
         funding,
         Some(pushMsat),
         feerateSatPerByte = None,
         channelFlags = None,
         openTimeout = None)
  }

  def open(
      nodeId: NodeId,
      funding: CurrencyUnit,
      pushMsat: MilliSatoshis,
      feerateSatPerByte: SatoshisPerByte): Future[FundedChannelId] = {
    open(nodeId,
         funding,
         Some(pushMsat),
         Some(feerateSatPerByte),
         channelFlags = None,
         openTimeout = None)
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit,
      pushMsat: MilliSatoshis = MilliSatoshis.zero,
      feerateSatPerByte: SatoshisPerByte,
      channelFlags: Byte): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         Some(pushMsat),
         Some(feerateSatPerByte),
         Some(channelFlags),
         None)
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit,
      feerateSatPerByte: SatoshisPerByte,
      channelFlags: Byte): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         pushMsat = None,
         Some(feerateSatPerByte),
         Some(channelFlags),
         None)
  }

  override def getPeers: Future[Vector[PeerInfo]] = {
    eclairCall[Vector[PeerInfo]]("peers")
  }

  override def createInvoice(description: String): Future[LnInvoice] = {
    createInvoice(description, None, None, None, None)
  }

  override def createInvoice(
      description: String,
      amountMsat: MilliSatoshis): Future[LnInvoice] = {
    createInvoice(description, Some(amountMsat), None, None, None)
  }

  override def createInvoice(
      description: String,
      amountMsat: MilliSatoshis,
      expireIn: FiniteDuration): Future[LnInvoice] = {
    createInvoice(description, Some(amountMsat), Some(expireIn), None, None)
  }

  override def createInvoice(
      description: String,
      amountMsat: MilliSatoshis,
      paymentPreimage: PaymentPreimage): Future[LnInvoice] = {
    createInvoice(description,
                  Some(amountMsat),
                  None,
                  None,
                  Some(paymentPreimage))
  }

  override def createInvoice(
      description: String,
      amountMsat: MilliSatoshis,
      expireIn: FiniteDuration,
      paymentPreimage: PaymentPreimage): Future[LnInvoice] = {
    createInvoice(description,
                  Some(amountMsat),
                  Some(expireIn),
                  None,
                  Some(paymentPreimage))
  }

  override def createInvoice(
      description: String,
      amountMsat: Option[MilliSatoshis],
      expireIn: Option[FiniteDuration],
      fallbackAddress: Option[Address],
      paymentPreimage: Option[PaymentPreimage]): Future[LnInvoice] = {
    val params = Seq(
      Some("description" -> description),
      amountMsat.map(x => "amountMsat" -> x.toBigDecimal.toString),
      expireIn.map(x => "expireIn" -> x.toSeconds.toString),
      fallbackAddress.map(x => "fallbackAddress" -> x.toString),
      paymentPreimage.map(x => "paymentPreimage" -> x.hex)
    ).flatten

    val responseF = eclairCall[InvoiceResult]("createinvoice", params: _*)

    responseF.flatMap { res =>
      Future.fromTry(LnInvoice.fromStringT(res.serialized))
    }
  }

  /** @inheritdoc */
  override def monitorInvoice(
      lnInvoice: LnInvoice,
      interval: FiniteDuration = 1.second,
      maxAttempts: Int = 60): Future[IncomingPayment] = {
    val p: Promise[IncomingPayment] = Promise[IncomingPayment]()
    val attempts = new AtomicInteger(0)
    val runnable = new Runnable() {

      override def run(): Unit = {
        val receivedInfoF = getReceivedInfo(lnInvoice)

        //register callback that publishes a payment to our actor system's
        //event stream,
        receivedInfoF.foreach {
          case None |
              Some(IncomingPayment(_, _, _, IncomingPaymentStatus.Pending)) =>
            if (attempts.incrementAndGet() >= maxAttempts) {
              // too many tries to get info about a payment
              // either Eclair is down or the payment is still in PENDING state for some reason
              // complete the promise with an exception so the runnable will be canceled
              p.failure(new RuntimeException(
                s"EclairApi.monitorInvoice() [${instance.authCredentials.datadir}] too many attempts: ${attempts
                  .get()} for invoice=${lnInvoice}"))
            }
          case Some(result) =>
            //invoice has been paid, let's publish to event stream
            //so subscribers so the even stream can see that a payment
            //was received
            //we need to create a `PaymentSucceeded`
            system.eventStream.publish(result)

            //complete the promise so the runnable will be canceled
            p.success(result)

        }
      }
    }

    val cancellable =
      system.scheduler.scheduleAtFixedRate(interval, interval)(runnable)

    p.future.onComplete(_ => cancellable.cancel())

    p.future
  }

  override def parseInvoice(invoice: LnInvoice): Future[InvoiceResult] = {
    eclairCall[InvoiceResult]("parseinvoice", "invoice" -> invoice.toString)
  }

  override def payInvoice(invoice: LnInvoice): Future[PaymentId] =
    payInvoice(invoice, None, None, None, None, None)

  override def payInvoice(
      invoice: LnInvoice,
      amount: MilliSatoshis): Future[PaymentId] =
    payInvoice(invoice, Some(amount), None, None, None, None)

  override def payInvoice(
      invoice: LnInvoice,
      externalId: Option[String]): Future[PaymentId] =
    payInvoice(invoice, None, None, None, None, externalId)

  override def payInvoice(
      invoice: LnInvoice,
      amount: MilliSatoshis,
      externalId: Option[String]): Future[PaymentId] =
    payInvoice(invoice, Some(amount), None, None, None, externalId)

  override def payInvoice(
      invoice: LnInvoice,
      amountMsat: Option[MilliSatoshis],
      maxAttempts: Option[Int],
      feeThresholdSat: Option[Satoshis],
      maxFeePct: Option[Int],
      externalId: Option[String]): Future[PaymentId] = {
    val params = Seq(
      Some("invoice" -> invoice.toString),
      amountMsat.map(x => "amountMsat" -> x.toBigDecimal.toString),
      maxAttempts.map(x => "maxAttempts" -> x.toString),
      feeThresholdSat.map(x => "feeThresholdSat" -> x.toBigDecimal.toString),
      maxFeePct.map(x => "maxFeePct" -> x.toString),
      externalId.map(x => "externalId" -> x)
    ).flatten

    eclairCall[PaymentId]("payinvoice", params: _*)
  }

  override def getReceivedInfo(
      paymentHash: Sha256Digest): Future[Option[IncomingPayment]] = {
    getReceivedInfo("paymentHash" -> paymentHash.hex)
  }

  override def getReceivedInfo(
      invoice: LnInvoice): Future[Option[IncomingPayment]] = {
    getReceivedInfo("invoice" -> invoice.toString)
  }

  private def getReceivedInfo(params: (String, String)*) = {
    //eclair continues the tradition of not responding to things in json...
    //the failure case here is the string 'Not found'
    implicit val r: Reads[Option[IncomingPayment]] = Reads { js =>
      val result: JsResult[IncomingPayment] =
        js.validate[IncomingPayment]
      result match {
        case JsSuccess(result, _) => JsSuccess(Some(result))
        case _: JsError           => JsSuccess(None)
      }
    }
    eclairCall[Option[IncomingPayment]]("getreceivedinfo", params: _*)(r)
  }

  override def getSentInfo(
      paymentHash: Sha256Digest): Future[Vector[OutgoingPayment]] = {
    eclairCall[Vector[OutgoingPayment]]("getsentinfo",
                                        "paymentHash" -> paymentHash.hex)
  }

  override def getSentInfo(id: PaymentId): Future[Vector[OutgoingPayment]] = {
    eclairCall[Vector[OutgoingPayment]]("getsentinfo", "id" -> id.toString)
  }

  override def sendToNode(
      nodeId: NodeId,
      amountMsat: MilliSatoshis,
      paymentHash: Sha256Digest,
      maxAttempts: Option[Int],
      feeThresholdSat: Option[Satoshis],
      maxFeePct: Option[Int],
      externalId: Option[String]): Future[PaymentId] = {
    val params = Seq("nodeId" -> nodeId.toString,
                     "amountMsat" -> amountMsat.toBigDecimal.toString,
                     "paymentHash" -> paymentHash.hex) ++ Seq(
      maxAttempts.map(x => "maxAttempts" -> x.toString),
      feeThresholdSat.map(x => "feeThresholdSat" -> x.toBigDecimal.toString),
      maxFeePct.map(x => "maxFeePct" -> x.toString),
      externalId.map(x => "externalId" -> x)
    ).flatten

    eclairCall[PaymentId]("sendtonode", params: _*)
  }

  def sendToRoute(
      invoice: LnInvoice,
      route: scala.collection.immutable.Seq[NodeId],
      amountMsat: MilliSatoshis,
      paymentHash: Sha256Digest,
      finalCltvExpiry: Long,
      recipientAmountMsat: Option[MilliSatoshis],
      parentId: Option[PaymentId],
      externalId: Option[String]): Future[SendToRouteResult] = {
    val params = Seq(
      "invoice" -> invoice.toString,
      "route" -> route.iterator.mkString(","),
      "amountMsat" -> amountMsat.toBigDecimal.toString,
      "paymentHash" -> paymentHash.hex,
      "finalCltvExpiry" -> finalCltvExpiry.toString
    ) ++ Seq(recipientAmountMsat.map(x => "recipientAmountMsat" -> x.toString),
             parentId.map(x => "parentId" -> x.toString),
             externalId.map(x => "externalId" -> x)).flatten
    eclairCall[SendToRouteResult]("sendtoroute", params: _*)
  }

  override def updateRelayFee(
      channelId: ChannelId,
      feeBaseMsat: MilliSatoshis,
      feeProportionalMillionths: Long): Future[ChannelCommandResult] = {
    eclairCall[ChannelCommandResult](
      "updaterelayfee",
      "channelId" -> channelId.hex,
      "feeBaseMsat" -> feeBaseMsat.toLong.toString,
      "feeProportionalMillionths" -> feeProportionalMillionths.toString
    )
  }

  override def updateRelayFee(
      shortChannelId: ShortChannelId,
      feeBaseMsat: MilliSatoshis,
      feeProportionalMillionths: Long): Future[ChannelCommandResult] = {
    eclairCall[ChannelCommandResult](
      "updaterelayfee",
      "shortChannelId" -> shortChannelId.toHumanReadableString,
      "feeBaseMsat" -> feeBaseMsat.toLong.toString,
      "feeProportionalMillionths" -> feeProportionalMillionths.toString
    )
  }

  override def channelStats(): Future[Vector[ChannelStats]] = {
    eclairCall[Vector[ChannelStats]]("channelstats")
  }

  override def networkFees(
      from: Option[FiniteDuration],
      to: Option[FiniteDuration]): Future[Vector[NetworkFeesResult]] = {
    eclairCall[Vector[NetworkFeesResult]](
      "networkfees",
      Seq(from.map(x => "from" -> x.toSeconds.toString),
          to.map(x => "to" -> x.toSeconds.toString)).flatten: _*)
  }

  override def getInvoice(paymentHash: Sha256Digest): Future[LnInvoice] = {
    val resF =
      eclairCall[InvoiceResult]("getinvoice", "paymentHash" -> paymentHash.hex)
    resF.flatMap { res =>
      Future.fromTry(LnInvoice.fromStringT(res.serialized))
    }
  }

  override def listInvoices(
      from: Option[Instant],
      to: Option[Instant]): Future[Vector[LnInvoice]] = {
    listInvoices("listinvoices", from, to)
  }

  override def listPendingInvoices(
      from: Option[Instant],
      to: Option[Instant]): Future[Vector[LnInvoice]] = {
    listInvoices("listpendinginvoices", from, to)
  }

  private def listInvoices(
      command: String,
      from: Option[Instant],
      to: Option[Instant]): Future[Vector[LnInvoice]] = {
    val resF = eclairCall[Vector[InvoiceResult]](
      command,
      Seq(from.map(x => "from" -> x.getEpochSecond.toString),
          to.map(x => "to" -> x.getEpochSecond.toString)).flatten: _*)
    resF.flatMap(xs =>
      Future.sequence(
        xs.map(x => Future.fromTry(LnInvoice.fromStringT(x.serialized)))))
  }

  override def usableBalances(): Future[Vector[UsableBalancesResult]] = {
    eclairCall[Vector[UsableBalancesResult]]("usablebalances")
  }

  override def disconnect(nodeId: NodeId): Future[Unit] = {
    eclairCall[String]("disconnect", "nodeId" -> nodeId.hex).map(_ => ())
  }

  override def getNewAddress(): Future[BitcoinAddress] = {
    eclairCall[BitcoinAddress]("getnewaddress")
  }

  override def onChainBalance(): Future[OnChainBalance] = {
    eclairCall[OnChainBalance]("onchainbalance")
  }

  override def onChainTransactions(): Future[Vector[WalletTransaction]] = {
    eclairCall[Vector[WalletTransaction]]("onchaintransactions")
  }

  override def sendOnChain(
      address: BitcoinAddress,
      amount: Satoshis,
      confirmationTarget: Int): Future[DoubleSha256DigestBE] = {
    eclairCall[DoubleSha256DigestBE](
      "sendonchain",
      "address" -> address.toString,
      "amountSatoshis" -> amount.toLong.toString,
      "confirmationTarget" -> confirmationTarget.toString)
  }

  private def eclairCall[T](command: String, parameters: (String, String)*)(
      implicit reader: Reads[T]): Future[T] = {
    val request = buildRequest(getDaemon, command, parameters: _*)

    logger.trace(s"eclair rpc call ${request}")
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)
    payloadF.map { payload =>
      val validated: JsResult[T] = payload.validate[T]
      val parsed: T = parseResult(validated, payload, command)
      parsed
    }
  }

  case class RpcError(error: String)
  implicit val rpcErrorReads: Reads[RpcError] = Json.reads[RpcError]

  private def parseResult[T](
      result: JsResult[T],
      json: JsValue,
      commandName: String): T = {
    result match {
      case res: JsSuccess[T] =>
        res.value
      case res: JsError =>
        json.validate[RpcError] match {
          case err: JsSuccess[RpcError] =>
            val datadirMsg = instance.authCredentials.datadir
              .map(d => s"datadir=${d}")
              .getOrElse("")
            val errMsg =
              s"Error for command=${commandName} ${datadirMsg}, ${err.value.error}"
            logger.error(errMsg)
            throw new RuntimeException(err.value.error)
          case _: JsError =>
            logger.error(
              s"Could not parse JsResult for command=$commandName: ${JsError
                .toJson(res)
                .toString()} JSON ${json}")
            throw new IllegalArgumentException(
              s"Could not parse JsResult for command=$commandName")
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
    val respF = Http(system).singleRequest(req)
    respF
  }

  private def buildRequest(
      instance: EclairInstance,
      methodName: String,
      params: (String, String)*): HttpRequest = {

    val uri = instance.rpcUri.resolve("/" + methodName).toString
    // Eclair doesn't use a username
    val username = ""
    val password = instance.authCredentials.password
    HttpRequest(method = HttpMethods.POST,
                uri,
                entity = FormData(params: _*).toEntity)
      .addCredentials(
        HttpCredentials.createBasicHttpCredentials(username, password))
  }

  private def pathToEclairJar: String = {

    (binary, Properties.envOrNone("ECLAIR_PATH")) match {
      // default to provided binary
      case (Some(binary), _) =>
        if (binary.exists) {
          binary.toString
        } else {
          throw new NoSuchFileException(
            s"Given binary ($binary) does not exist!")
        }
      case (None, Some(path)) =>
        val eclairBinDir =
          s"eclair-node-${EclairRpcClient.version}-${EclairRpcClient.commit}${File.separator}bin${File.separator}"
        val eclairV =
          if (sys.props("os.name").toLowerCase.contains("windows"))
            eclairBinDir + "eclair-node.bat"
          else
            eclairBinDir + "eclair-node.sh"

        val jar = new File(path, eclairV)
        if (jar.exists) {
          jar.getPath
        } else {
          throw new NoSuchFileException(
            s"Could not Eclair Jar at location ${jar.getPath}")
        }
      case (None, None) =>
        val msg = List(
          "Environment variable ECLAIR_PATH is not set, and no binary is given!",
          "Either needs to be set in order to start Eclair.")
        throw new RuntimeException(msg.mkString(" "))
    }
  }

  private var process: Option[Process] = None

  /** Starts eclair on the local system.
    *
    * @return a future of the started EclairRpcClient when eclair is fully started.
    *         If eclair has not successfully started in 60 seconds
    *         the future times out.
    */
  override def start(): Future[EclairRpcClient] = {

    val _ = {

      require(instance.authCredentials.datadir.isDefined,
              s"A datadir needs to be provided to start eclair")
      if (process.isEmpty) {
        val logback = instance.logbackXmlPath
          .map(path => s"-Dlogback.configurationFile=$path")
          .getOrElse("")
        val cmd =
          s"${pathToEclairJar} -Declair.datadir=${instance.authCredentials.datadir.get} $logback"
        val p = Process(cmd)
        val result = p.run()
        logger.debug(
          s"Starting eclair with datadir ${instance.authCredentials.datadir.get}")

        process = Some(result)
        ()
      } else {
        logger.info(s"Eclair was already started!")
        ()
      }
    }

    val started: Future[EclairRpcClient] = {
      for {
        _ <- AsyncUtil.retryUntilSatisfiedF(() => isStarted,
                                            duration = 1.seconds,
                                            maxTries = 60)
      } yield this
    }
    started
  }

  /**
    * Boolean check to verify the state of the client
    * @return Future Boolean representing if client has started
    */
  def isStarted(): Future[Boolean] = {
    val p = Promise[Boolean]()

    getInfo.onComplete {
      case Success(_) =>
        p.success(true)
      case Failure(_) =>
        p.success(false)
    }

    p.future
  }

  /** Returns a Future EclairRpcClient if able to shut down
    * Eclair instance, inherits from the StartStop trait
    * @return A future EclairRpcClient that is stopped
    */
  def stop(): Future[EclairRpcClient] = {
    val _ = process.map(_.destroy()) match {
      case None    => false
      case Some(_) => true
    }
    val actorSystemF = if (system.name == EclairRpcClient.ActorSystemName) {
      system.terminate()
    } else {
      FutureUtil.unit
    }
    actorSystemF.map(_ => this)
  }

  /**
    * Checks to see if the client stopped successfully
    * @return
    */
  def isStopped: Future[Boolean] = {
    isStarted.map(started => !started)
  }

  /**
    * Pings eclair to see if a invoice has been paid
    * If the invoice has been paid or the payment has failed, we publish a
    * [[OutgoingPayment]]
    * event to the [[akka.actor.ActorSystem ActorSystem]]'s
    * [[akka.event.EventStream ActorSystem.eventStream]]
    *
    * We also return a Future[PaymentResult] that is completed when one of three things is true
    * 1. The payment has succeeded
    * 2. The payment has failed
    * 3. We have attempted to query the eclair more than maxAttempts, and the payment is still pending
    */
  override def monitorSentPayment(
      paymentId: PaymentId,
      interval: FiniteDuration,
      maxAttempts: Int): Future[OutgoingPayment] = {
    val p: Promise[OutgoingPayment] = Promise[OutgoingPayment]()

    val runnable = new Runnable() {

      private val attempts = new AtomicInteger(0)

      override def run(): Unit = {
        if (attempts.incrementAndGet() > maxAttempts) {
          // too many tries to get info about a payment
          // either Eclair is down or the payment is still in PENDING state for some reason
          // complete the promise with an exception so the runnable will be canceled
          p.failure(
            new RuntimeException(
              s"EclairApi.monitorSentPayment() too many attempts: ${attempts
                .get()} for paymentId=${paymentId} for interval=${interval}"))
        } else {
          val resultsF = getSentInfo(paymentId)
          resultsF.recover {
            case e: Throwable =>
              logger.error(
                s"Cannot check payment status for paymentId=${paymentId}",
                e)
          }
          val _ = for {
            results <- resultsF
          } yield {
            results.foreach { result =>
              result.status match {
                case OutgoingPaymentStatus.Pending =>
                //do nothing, while we wait for eclair to attempt to process
                case (_: OutgoingPaymentStatus.Succeeded |
                    _: OutgoingPaymentStatus.Failed) =>
                  // invoice has been succeeded or has failed, let's publish to event stream
                  // so subscribers to the event stream can see that a payment
                  // was received or failed
                  // complete the promise so the runnable will be canceled
                  p.success(result)
              }
            }
          }
        }
      }
    }

    val cancellable =
      system.scheduler.scheduleAtFixedRate(interval, interval)(runnable)

    val f = p.future

    f.onComplete(_ => cancellable.cancel())
    f.foreach(system.eventStream.publish)

    f
  }

  /** @inheritdoc */
  override def connectToWebSocket(
      eventHandler: WebSocketEvent => Unit): Future[Unit] = {
    val incoming: Sink[Message, Future[Done]] =
      Sink.foreach[Message] {
        case message: TextMessage.Strict =>
          val parsed: JsValue = Json.parse(message.text)
          val validated: JsResult[WebSocketEvent] =
            parsed.validate[WebSocketEvent]
          val event = parseResult[WebSocketEvent](validated, parsed, "ws")
          eventHandler(event)
        case _: Message => ()
      }

    val flow =
      Flow.fromSinkAndSource(incoming, Source.maybe)

    val uri =
      instance.rpcUri.resolve("/ws").toString.replace("http://", "ws://")
    instance.authCredentials.bitcoinAuthOpt
    val request = WebSocketRequest(
      uri,
      extraHeaders = Vector(
        Authorization(
          BasicHttpCredentials("", instance.authCredentials.password))))
    val (upgradeResponse, _) = Http().singleWebSocketRequest(request, flow)

    val connected = upgradeResponse.map { upgrade =>
      if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
        Done
      } else {
        throw new RuntimeException(
          s"Connection failed: ${upgrade.response.status}")
      }
    }

    connected.failed.foreach(ex =>
      logger.error(s"Cannot connect to web socket $uri ", ex))

    connected.map(_ => ())
  }
}

object EclairRpcClient {

  /** THe name we use to create actor systems. We use this to know which
    * actor systems to shut down on node shutdown
    */
  private[eclair] val ActorSystemName = "eclair-rpc-client-created-by-bitcoin-s"

  /**
    * Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, wher you need fine grained control
    * over the RPC client.
    */
  def apply(
      instance: EclairInstance,
      binary: Option[File] = None): EclairRpcClient = {
    implicit val systme = ActorSystem.create(ActorSystemName)
    withActorSystem(instance, binary)
  }

  /**
    * Constructs a RPC client from the given datadir, or
    * the default datadir if no directory is provided
    */
  def withActorSystem(instance: EclairInstance, binary: Option[File] = None)(
      implicit system: ActorSystem) = new EclairRpcClient(instance, binary)

  /** The current commit we support of Eclair */
  private[bitcoins] val commit = "e5fb281"

  /** The current version we support of Eclair */
  private[bitcoins] val version = "0.4.1"
}
