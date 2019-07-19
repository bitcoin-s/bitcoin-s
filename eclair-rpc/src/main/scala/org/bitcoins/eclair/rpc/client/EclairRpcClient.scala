package org.bitcoins.eclair.rpc.client

import java.io.File
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.Sha256Digest
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.Address
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
import org.bitcoins.core.util.{BitcoinSUtil, FutureUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.eclair.rpc.api.EclairApi
import org.bitcoins.eclair.rpc.config.EclairInstance
import org.bitcoins.eclair.rpc.json._
import org.bitcoins.eclair.rpc.network.{NodeUri, PeerState}
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.util.AsyncUtil
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.sys.process._
import scala.util.{Failure, Properties, Success}

class EclairRpcClient(val instance: EclairInstance)(
    implicit system: ActorSystem)
    extends EclairApi {
  import JsonReaders._

  implicit val m = ActorMaterializer.create(system)
  private val logger = LoggerFactory.getLogger(this.getClass)

  def getDaemon: EclairInstance = instance

  implicit override def executionContext: ExecutionContext = m.executionContext

  override def allChannels(): Future[Vector[ChannelDesc]] = {
    eclairCall[Vector[ChannelDesc]]("allchannels")
  }

  override def allNodes(): Future[Vector[NodeInfo]] = {
    eclairCall[Vector[NodeInfo]]("allnodes")
  }

  override def allUpdates(): Future[Vector[ChannelUpdate]] =
    eclairCall[Vector[ChannelUpdate]]("allupdates")

  override def allUpdates(nodeId: NodeId): Future[Vector[ChannelUpdate]] =
    eclairCall[Vector[ChannelUpdate]]("allupdates", "nodeId" -> nodeId.toString)

  /**
    * @inheritdoc
    */
  override def audit(): Future[AuditResult] =
    eclairCall[AuditResult]("audit")

  /**
    * @inheritdoc
    */
  override def audit(
      from: Option[FiniteDuration],
      to: Option[FiniteDuration]): Future[AuditResult] =
    eclairCall[AuditResult](
      "audit",
      Seq(from.map(x => "from" -> x.toSeconds.toString),
          to.map(x => "to" -> x.toSeconds.toString)).flatten: _*)

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
      scriptPubKey: Option[ScriptPubKey]): Future[Unit] = {
    val params =
      if (scriptPubKey.isEmpty) {
        Seq("channelId" -> channelId.hex)
      } else {

        val asmHex = BitcoinSUtil.encodeHex(scriptPubKey.get.asmBytes)

        Seq("channelId" -> channelId.hex, "scriptPubKey" -> asmHex)
      }

    eclairCall[String]("close", params: _*).map(_ => ())
  }

  def close(channelId: ChannelId): Future[Unit] =
    close(channelId, scriptPubKey = None)

  override def close(
      channelId: ChannelId,
      scriptPubKey: ScriptPubKey): Future[Unit] = {
    close(channelId, Some(scriptPubKey))
  }

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

  override def forceClose(channelId: ChannelId): Future[Unit] = {
    eclairCall[String]("forceclose", "channelId" -> channelId.hex).map(_ => ())
  }

  override def forceClose(shortChannelId: ShortChannelId): Future[Unit] = {
    eclairCall[String]("forceclose",
                       "shortChannelId" -> shortChannelId.toString).map(_ => ())
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
      fundingSatoshis: CurrencyUnit,
      pushMsat: Option[MilliSatoshis],
      feerateSatPerByte: Option[SatoshisPerByte],
      channelFlags: Option[Byte]): Future[FundedChannelId] = {
    val _pushMsat = pushMsat.getOrElse(MilliSatoshis.zero).toBigDecimal.toString
    val _fundingSatoshis = fundingSatoshis.satoshis.toBigDecimal.toString

    val params: Seq[(String, String)] = {
      if (feerateSatPerByte.isEmpty) {
        Seq("nodeId" -> nodeId.toString,
            "fundingSatoshis" -> _fundingSatoshis,
            "pushMsat" -> _pushMsat)
      } else if (channelFlags.isEmpty) {
        Seq("nodeId" -> nodeId.toString,
            "fundingSatoshis" -> _fundingSatoshis,
            "pushMsat" -> _pushMsat,
            "fundingFeerateSatByte" -> feerateSatPerByte.get.toLong.toString)
      } else {
        Seq(
          "nodeId" -> nodeId.toString,
          "fundingSatoshis" -> _fundingSatoshis,
          "pushMsat" -> _pushMsat,
          "fundingFeerateSatByte" -> feerateSatPerByte.get.toLong.toString,
          "channelFlags" -> channelFlags.get.toString
        )
      }
    }

    //this is unfortunately returned in this format
    //created channel 30bdf849eb9f72c9b41a09e38a6d83138c2edf332cb116dd7cf0f0dfb66be395
    val call = eclairCall[String]("open", params: _*)

    //let's just return the chanId
    val chanIdF = call.map(_.split(" ").last)

    chanIdF.map(FundedChannelId.fromHex)
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         pushMsat = None,
         feerateSatPerByte = None,
         channelFlags = None)
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit,
      pushMsat: MilliSatoshis): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         Some(pushMsat),
         feerateSatPerByte = None,
         channelFlags = None)
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit,
      pushMsat: MilliSatoshis,
      feerateSatPerByte: SatoshisPerByte): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         Some(pushMsat),
         Some(feerateSatPerByte),
         channelFlags = None)
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
         Some(channelFlags))
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
         Some(channelFlags))
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
      Future.fromTry(LnInvoice.fromString(res.serialized))
    }
  }

  /** @inheritdoc */
  override def monitorInvoice(
      lnInvoice: LnInvoice,
      maxAttempts: Int = 60): Future[ReceivedPaymentResult] = {
    val p: Promise[ReceivedPaymentResult] = Promise[ReceivedPaymentResult]()
    val attempts = new AtomicInteger(0)
    val runnable = new Runnable() {

      override def run(): Unit = {
        val receivedInfoF = getReceivedInfo(lnInvoice)

        //register callback that publishes a payment to our actor system's
        //event stream,
        receivedInfoF.map {
          case None =>
            if (attempts.incrementAndGet() > maxAttempts) {
              // too many tries to get info about a payment
              // either Eclair is down or the payment is still in PENDING state for some reason
              // complete the promise with an exception so the runnable will be canceled
              p.failure(
                new RuntimeException(
                  s"EclairApi.monitorInvoice() too many attempts: ${attempts
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

    val cancellable = system.scheduler.schedule(1.seconds, 1.seconds, runnable)

    p.future.map(_ => cancellable.cancel())

    p.future
  }

  override def parseInvoice(invoice: LnInvoice): Future[InvoiceResult] = {
    eclairCall[InvoiceResult]("parseinvoice", "invoice" -> invoice.toString)
  }

  override def payInvoice(invoice: LnInvoice): Future[PaymentId] = {
    payInvoice(invoice, None, None, None, None)
  }

  override def payInvoice(
      invoice: LnInvoice,
      amount: MilliSatoshis): Future[PaymentId] = {
    payInvoice(invoice, Some(amount), None, None, None)
  }

  override def payInvoice(
      invoice: LnInvoice,
      amountMsat: Option[MilliSatoshis],
      maxAttempts: Option[Int],
      feeThresholdSat: Option[Satoshis],
      maxFeePct: Option[Int]): Future[PaymentId] = {
    val params = Seq(
      Some("invoice" -> invoice.toString),
      amountMsat.map(x => "amountMsat" -> x.toBigDecimal.toString),
      maxAttempts.map(x => "maxAttempts" -> x.toString),
      feeThresholdSat.map(x => "feeThresholdSat" -> x.toBigDecimal.toString),
      maxFeePct.map(x => "maxFeePct" -> x.toString)
    ).flatten

    eclairCall[PaymentId]("payinvoice", params: _*)
  }

  override def getReceivedInfo(
      paymentHash: Sha256Digest): Future[Option[ReceivedPaymentResult]] = {

    //eclair continues the tradition of not responding to things in json...
    //the failure case here is the string 'Not found'
    implicit val r: Reads[Option[ReceivedPaymentResult]] = Reads { js =>
      val result: JsResult[ReceivedPaymentResult] =
        js.validate[ReceivedPaymentResult]
      result match {
        case JsSuccess(result, _) => JsSuccess(Some(result))
        case _: JsError           => JsSuccess(None)
      }
    }
    eclairCall[Option[ReceivedPaymentResult]](
      "getreceivedinfo",
      "paymentHash" -> paymentHash.hex)(r)
  }

  override def getSentInfo(
      paymentHash: Sha256Digest): Future[Vector[PaymentResult]] = {
    eclairCall[Vector[PaymentResult]]("getsentinfo",
                                      "paymentHash" -> paymentHash.hex)
  }

  override def getSentInfo(id: PaymentId): Future[Vector[PaymentResult]] = {
    eclairCall[Vector[PaymentResult]]("getsentinfo", "id" -> id.toString)
  }

  override def sendToNode(
      nodeId: NodeId,
      amountMsat: MilliSatoshis,
      paymentHash: Sha256Digest,
      maxAttempts: Option[Int],
      feeThresholdSat: Option[Satoshis],
      maxFeePct: Option[Int]): Future[PaymentId] = {
    val params = Seq("nodeId" -> nodeId.toString,
                     "amountMsat" -> amountMsat.toBigDecimal.toString,
                     "paymentHash" -> paymentHash.hex) ++ Seq(
      maxAttempts.map(x => "maxAttempts" -> x.toString),
      feeThresholdSat.map(x => "feeThresholdSat" -> x.toBigDecimal.toString),
      maxFeePct.map(x => "maxFeePct" -> x.toString)
    ).flatten

    eclairCall[PaymentId]("sendtonode", params: _*)
  }

  def sendToRoute(
      route: TraversableOnce[NodeId],
      amountMsat: MilliSatoshis,
      paymentHash: Sha256Digest,
      finalCltvExpiry: Long): Future[PaymentId] = {
    eclairCall[PaymentId](
      "sendtoroute",
      "route" -> route.mkString(","),
      "amountMsat" -> amountMsat.toBigDecimal.toString,
      "paymentHash" -> paymentHash.hex,
      "finalCltvExpiry" -> finalCltvExpiry.toString
    )
  }

  override def updateRelayFee(
      channelId: ChannelId,
      feeBaseMsat: MilliSatoshis,
      feeProportionalMillionths: Long): Future[Unit] = {
    eclairCall[Unit](
      "updaterelayfee",
      "channelId" -> channelId.hex,
      "feeBaseMsat" -> feeBaseMsat.toLong.toString,
      "feeProportionalMillionths" -> feeProportionalMillionths.toString
    )
  }

  override def updateRelayFee(
      shortChannelId: ShortChannelId,
      feeBaseMsat: MilliSatoshis,
      feeProportionalMillionths: Long): Future[Unit] = {
    eclairCall[Unit](
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
      Future.fromTry(LnInvoice.fromString(res.serialized))
    }
  }

  override def listInvoices(
      from: Option[FiniteDuration],
      to: Option[FiniteDuration]): Future[Vector[LnInvoice]] = {
    val resF = eclairCall[Vector[InvoiceResult]](
      "listinvoices",
      Seq(from.map(x => "from" -> x.toSeconds.toString),
          to.map(x => "to" -> x.toSeconds.toString)).flatten: _*)
    resF.flatMap(xs =>
      Future.sequence(xs.map(x =>
        Future.fromTry(LnInvoice.fromString(x.serialized)))))
  }

  override def usableBalances(): Future[Vector[UsableBalancesResult]] = {
    eclairCall[Vector[UsableBalancesResult]]("usablebalances")
  }

  override def disconnect(nodeId: NodeId): Future[Unit] = {
    eclairCall[String]("disconnect", "nodeId" -> nodeId.hex).map(_ => ())
  }

  private def eclairCall[T](command: String, parameters: (String, String)*)(
      implicit
      reader: Reads[T]): Future[T] = {
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
            throw new RuntimeException(errMsg)
          case _: JsError =>
            logger.error(JsError.toJson(res).toString())
            throw new IllegalArgumentException(
              s"Could not parse JsResult! JSON: ${json}")
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
    val path = Properties
      .envOrNone("ECLAIR_PATH")
      .getOrElse(throw new RuntimeException(
        List("Environment variable ECLAIR_PATH is not set!",
             "This needs to be set to the directory containing the Eclair Jar")
          .mkString(" ")))

    val eclairV = "/eclair-node-0.3.1-6906ecb.jar"
    val fullPath = path + eclairV

    val jar = new File(fullPath)
    if (jar.exists) {
      fullPath
    } else {
      throw new RuntimeException(s"Could not Eclair Jar at location $fullPath")
    }
  }

  private var process: Option[Process] = None

  /** Starts eclair on the local system.
    *
    * @return a future that completes when eclair is fully started.
    *         If eclair has not successfully started in 60 seconds
    *         the future times out.
    */
  def start(): Future[Unit] = {

    val _ = {

      require(instance.authCredentials.datadir.isDefined,
              s"A datadir needs to be provided to start eclair")

      if (process.isEmpty) {
        val p = Process(
          s"java -jar -Declair.datadir=${instance.authCredentials.datadir.get} $pathToEclairJar &")
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

    val started = AsyncUtil.retryUntilSatisfiedF(() => isStarted,
                                                 duration = 1.seconds,
                                                 maxTries = 60)

    started
  }

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

  def stop(): Option[Unit] = {
    process.map(_.destroy())
  }

  /**
    * Pings eclair to see if a invoice has been paid
    * If the invoice has been paid or the payment has failed, we publish a
    * [[org.bitcoins.eclair.rpc.json.PaymentResult PaymentResult]]
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
      maxAttempts: Int): Future[PaymentResult] = {
    val p: Promise[PaymentResult] = Promise[PaymentResult]()

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
                case PaymentStatus.PENDING =>
                //do nothing, while we wait for eclair to attempt to process
                case PaymentStatus.SUCCEEDED | PaymentStatus.FAILED =>
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

    val cancellable = system.scheduler.schedule(interval, interval, runnable)

    val f = p.future

    f.onComplete(_ => cancellable.cancel())
    f.foreach(system.eventStream.publish)

    f
  }
}
