package org.bitcoins.sbclient

import java.net.InetSocketAddress
import java.time.Instant

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy, QueueOfferResult}
import akka.stream.scaladsl.{
  Keep,
  Sink,
  SinkQueueWithCancel,
  Source,
  SourceQueueWithComplete
}
import org.bitcoins.core.crypto.{ECPrivateKey, ECPublicKey, Sha256Digest}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{Address, BitcoinAddress}
import org.bitcoins.core.protocol.ln.LnParams.LnBitcoinRegTest
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.channel.{ChannelId, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.{LnCurrencyUnits, MilliSatoshis}
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.eclair.rpc.api._
import org.bitcoins.eclair.rpc.network.NodeUri

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{FiniteDuration, _}

class MockEclairClient()(implicit system: ActorSystem)
    extends EclairApi
    with BitcoinSLogger {
  implicit private val m: ActorMaterializer = ActorMaterializer.create(system)

  implicit override val executionContext: ExecutionContext = system.dispatcher

  private def now: Instant = Instant.now

  private val paymentSource: Source[
    LnInvoice,
    SourceQueueWithComplete[
      LnInvoice
    ]] =
    Source.queue(16, OverflowStrategy.backpressure)

  private val paymentSink: Sink[LnInvoice, SinkQueueWithCancel[LnInvoice]] =
    Sink.queue()

  private val queues = paymentSource.toMat(paymentSink)(Keep.both).run()

  private val paymentSender: SourceQueueWithComplete[LnInvoice] = queues._1

  private val sentPaymentQueue: SinkQueueWithCancel[LnInvoice] = queues._2

  private var paymentsById =
    Map.empty[PaymentId, Vector[OutgoingPayment]].withDefaultValue(Vector.empty)

  private var paymentsByHash = Map
    .empty[Sha256Digest, Vector[OutgoingPayment]]
    .withDefaultValue(Vector.empty)

  def lastSent(): Future[Option[LnInvoice]] = {
    sentPaymentQueue.pull()
  }

  def publishReceivedFromLastSent(system: ActorSystem): Future[Unit] = {
    lastSent().flatMap {
      case Some(paymentSent) =>
        Future.successful(publishReceivedFromSent(paymentSent, system))
      case None =>
        Future.failed(new RuntimeException("No last sent to be received"))
    }
  }

  private def toPaymentRequest(invoice: LnInvoice): PaymentRequest = {
    PaymentRequest(
      prefix = invoice.hrp,
      timestamp = java.time.Instant.ofEpochSecond(invoice.timestamp.toLong),
      nodeId = invoice.nodeId,
      serialized = invoice.toString(),
      description = invoice.lnTags.description.get.string,
      paymentHash = invoice.lnTags.paymentHash.hash,
      0.seconds,
      amount = invoice.amount.map(_.toMSat)
    )
  }

  private def publishReceivedFromSent(
      invoice: LnInvoice,
      system: ActorSystem
  ): Unit = {
    val succeed = IncomingPayment(
      toPaymentRequest(invoice),
      paymentPreimage = PaymentPreimage.random,
      createdAt = now,
      status = IncomingPaymentStatus
        .Received(invoice.amount.getOrElse(LnCurrencyUnits.zero).toMSat, now)
    )
    system.eventStream.publish(succeed)
  }

  private val unsupportedFailure: Future[Nothing] = {
    Future.failed(
      new UnsupportedOperationException(
        "MockEcliarClient does not support this function yet"
      )
    )
  }

  override def audit(): Future[AuditResult] = audit(None, None)

  override def audit(
      from: Option[Instant],
      to: Option[Instant]): Future[AuditResult] =
    Future.successful(AuditResult(Vector.empty, Vector.empty, Vector.empty))

  override def allUpdates(): Future[Vector[ChannelUpdate]] =
    Future.successful(Vector.empty)

  override def allUpdates(nodeId: NodeId): Future[Vector[ChannelUpdate]] =
    Future.successful(Vector.empty)

  override val network: LnParams = LnBitcoinRegTest

  override def createInvoice(
      description: String,
      amountMsat: Option[MilliSatoshis],
      expireIn: Option[FiniteDuration],
      fallbackAddress: Option[Address],
      paymentPreimage: Option[PaymentPreimage]): Future[LnInvoice] = {

    val hrp = LnHumanReadablePart.fromParamsAmount(
      network = LnParams.LnBitcoinRegTest,
      amount = amountMsat.map(_.toLnCurrencyUnit)
    )

    val hash = ECPrivateKey().bytes
    val emptyHash = Sha256Digest.fromBytes(hash)

    val taggedFields = LnTaggedFields(
      paymentHash = LnTag.PaymentHashTag(emptyHash),
      descriptionOrHash = Left(LnTag.DescriptionTag(description)),
      nodeId = None,
      expiryTime = expireIn.map(x => LnTag.ExpiryTimeTag(UInt32(x.toSeconds))),
      cltvExpiry = None,
      fallbackAddress = None,
      routingInfo = None
    )

    val privateKey = ECPrivateKey.freshPrivateKey
    val invoice = LnInvoice.build(
      hrp = hrp,
      lnTags = taggedFields,
      privateKey = privateKey
    )

    Future.successful(invoice)
  }

  /**
    * Emulates a direct channel
    */
  override def findRoute(
      nodeId: NodeId,
      amountMsat: MilliSatoshis): Future[Vector[NodeId]] = {
    val route = (1 to 2).map(_ => NodeId.fromPubKey(ECPublicKey.freshPublicKey))
    Future.successful(route.toVector)
  }

  override def findRoute(invoice: LnInvoice): Future[Vector[NodeId]] = {
    val route = (1 to 2).map(_ => NodeId.fromPubKey(ECPublicKey.freshPublicKey))
    Future.successful(route.toVector)
  }

  override def findRoute(
      invoice: LnInvoice,
      amountMsat: MilliSatoshis): Future[Vector[NodeId]] = {
    val route = (1 to 2).map(_ => NodeId.fromPubKey(ECPublicKey.freshPublicKey))
    Future.successful(route.toVector)
  }

  override def payInvoice(invoice: LnInvoice): Future[PaymentId] = {
    val paymentId = PaymentId(java.util.UUID.randomUUID())
    val randomNodeId = NodeId(ECPublicKey.freshPublicKey)
    val amountMsat = invoice.amount.getOrElse(LnCurrencyUnits.zero).toMSat
    val paymentHash = invoice.lnTags.paymentHash.hash

    paymentSender.offer(invoice).map { queueOfferResult =>
      val paymentResult: OutgoingPayment = queueOfferResult match {
        case QueueOfferResult.Enqueued =>
          val status = OutgoingPaymentStatus.Succeeded(
            paymentPreimage = PaymentPreimage.random,
            feesPaid = MilliSatoshis.zero,
            route = Vector.empty,
            completedAt = now)

          OutgoingPayment(
            id = paymentId,
            parentId = paymentId,
            externalId = Some(paymentId.toString()),
            paymentHash = paymentHash,
            amount = amountMsat,
            createdAt = now,
            paymentRequest = None,
            status = status,
            recipientAmount = amountMsat,
            recipientNodeId = randomNodeId,
            paymentType = PaymentType.Standard
          )
        case _: QueueOfferResult =>
          val status = OutgoingPaymentStatus.Failed(failures = Vector.empty)
          OutgoingPayment(
            id = paymentId,
            parentId = paymentId,
            externalId = Some(paymentId.toString()),
            paymentHash = paymentHash,
            amount = amountMsat,
            createdAt = now,
            paymentRequest = None,
            status = status,
            recipientAmount = amountMsat,
            recipientNodeId = randomNodeId,
            paymentType = PaymentType.Standard
          )
      }

      synchronized {
        paymentsById = paymentsById.updated(paymentId, Vector(paymentResult))
        paymentsByHash = paymentsByHash
          .updated(paymentHash, paymentResult +: paymentsByHash(paymentHash))
      }

      paymentId
    }
  }

  override def payInvoice(
      invoice: LnInvoice,
      amt: MilliSatoshis
  ): Future[PaymentId] = {
    val newHrp = LnHumanReadablePart(
      network = invoice.hrp.network,
      amount = Some(amt.toLnCurrencyUnit)
    )

    val privateKey = ECPrivateKey.freshPrivateKey

    val newInvoice = LnInvoice.build(
      hrp = newHrp,
      lnTags = invoice.lnTags,
      privateKey = privateKey
    )

    payInvoice(newInvoice)
  }

  override def createInvoice(description: String): Future[LnInvoice] =
    createInvoice(description, None, None, None, None)

  override def createInvoice(
      description: String,
      amountMsat: MilliSatoshis): Future[LnInvoice] =
    createInvoice(description, Some(amountMsat), None, None, None)

  override def createInvoice(
      description: String,
      amountMsat: MilliSatoshis,
      expiryIn: FiniteDuration
  ): Future[LnInvoice] =
    createInvoice(description, Some(amountMsat), Some(expiryIn), None, None)
  override def createInvoice(
      description: String,
      amountMsat: MilliSatoshis,
      paymentPreimage: PaymentPreimage): Future[LnInvoice] =
    createInvoice(description,
                  Some(amountMsat),
                  None,
                  None,
                  Some(paymentPreimage))

  override def createInvoice(
      description: String,
      amountMsat: MilliSatoshis,
      expireIn: FiniteDuration,
      paymentPreimage: PaymentPreimage): Future[LnInvoice] =
    createInvoice(description,
                  Some(amountMsat),
                  Some(expireIn),
                  None,
                  Some(paymentPreimage))

  override def getSentInfo(
      paymentHash: Sha256Digest): Future[Vector[OutgoingPayment]] = Future {
    synchronized {
      paymentsByHash(paymentHash)
    }
  }

  override def getSentInfo(id: PaymentId): Future[Vector[OutgoingPayment]] =
    Future {
      synchronized {
        paymentsById(id)
      }
    }

  override def monitorInvoice(
      lnInvoice: LnInvoice,
      interval: FiniteDuration,
      maxAttempts: Int
  ): Future[IncomingPayment] = {
    val amt = lnInvoice.amount.getOrElse(LnCurrencyUnits.zero).toMSat
    //??? is this right ???
    val preimage: PaymentPreimage = PaymentPreimage.random
    val paymentRequest = toPaymentRequest(lnInvoice)

    val createdAt = now
    val timeReceived = now
    val incoming = IncomingPayment(
      paymentRequest = paymentRequest,
      paymentPreimage = preimage,
      createdAt = createdAt,
      status = IncomingPaymentStatus.Received(amt, timeReceived))
    Future.successful(incoming)
  }

  override def monitorSentPayment(
      paymentId: PaymentId,
      interval: FiniteDuration,
      maxAttempts: Int): Future[OutgoingPayment] = {
    for {
      results <- getSentInfo(paymentId)
    } yield {

      val result = results.filter(_.parentId == paymentId)
      system.eventStream.publish(result.last)
      result.last
    }
  }

  override def payAndMonitorInvoice(
      invoice: LnInvoice,
      externalId: Option[String],
      interval: FiniteDuration,
      maxAttempts: Int): Future[OutgoingPayment] = {
    val paidF = payInvoice(invoice)
    paidF.map { id =>
      paymentsById(id).head
    }

  }

  override def payAndMonitorInvoice(
      invoice: LnInvoice,
      amount: MilliSatoshis,
      externalId: Option[String],
      interval: FiniteDuration,
      maxAttempts: Int): Future[OutgoingPayment] = {
    val paidF = payInvoice(invoice, amount)
    paidF.map { id =>
      paymentsById(id).head
    }
  }

  override def getNewAddress(): Future[BitcoinAddress] = {
    unsupportedFailure
  }

  override def getInfo: Future[GetInfoResult] =
    unsupportedFailure

  override def updateRelayFee(
      channelId: ChannelId,
      feeBaseMsat: MilliSatoshis,
      feePropertionalMillionths: Long
  ): Future[Unit] =
    unsupportedFailure

  override def updateRelayFee(
      shortChannelId: ShortChannelId,
      feeBaseMsat: MilliSatoshis,
      feePropertionalMillionths: Long
  ): Future[Unit] =
    unsupportedFailure

  override def payInvoice(
      invoice: LnInvoice,
      amountMsat: Option[MilliSatoshis],
      maxAttempts: Option[Int],
      feeThresholdSat: Option[Satoshis],
      maxFeePct: Option[Int],
      externalId: Option[String]): Future[PaymentId] =
    unsupportedFailure

  override def payInvoice(
      invoice: LnInvoice,
      externalId: Option[String]): Future[PaymentId] = {
    unsupportedFailure
  }

  override def payInvoice(
      invoice: LnInvoice,
      amount: MilliSatoshis,
      externalId: Option[String]): Future[PaymentId] = {
    unsupportedFailure
  }

  override def isConnected(nodeId: NodeId): Future[Boolean] =
    unsupportedFailure

  override def listPendingInvoices(
      from: Option[Instant],
      to: Option[Instant]
  ): Future[Vector[LnInvoice]] =
    unsupportedFailure

  override def connectToWebSocket(
      eventHandler: org.bitcoins.eclair.rpc.api.WebSocketEvent => Unit): scala.concurrent.Future[
    Unit] = {
    unsupportedFailure
  }

  override def connect(nodeId: NodeId): Future[Unit] = {
    unsupportedFailure
  }

  override def connect(
      nodeId: NodeId,
      addr: InetSocketAddress): Future[Unit] = {
    unsupportedFailure
  }

  override def connect(nodeURI: NodeUri): Future[Unit] =
    unsupportedFailure

  override def connect(nodeId: NodeId, host: String, port: Int): Future[Unit] =
    unsupportedFailure

  override def getInvoice(paymentHash: Sha256Digest): Future[LnInvoice] =
    unsupportedFailure

  override def allChannels(): Future[Vector[ChannelDesc]] =
    unsupportedFailure

  override def allNodes(): Future[Vector[NodeInfo]] =
    unsupportedFailure

  override def sendToNode(
      nodeId: NodeId,
      amountMsat: MilliSatoshis,
      paymentHash: Sha256Digest,
      maxAttempts: Option[Int],
      feeThresholdSat: Option[Satoshis],
      maxFeePct: Option[Int],
      externalId: Option[String]): Future[PaymentId] =
    unsupportedFailure

  override def sendToRoute(
      invoice: LnInvoice,
      route: Seq[NodeId],
      amountMsat: MilliSatoshis,
      paymentHash: Sha256Digest,
      finalCltvExpiry: Long,
      recipientAmountMsat: Option[MilliSatoshis],
      parentId: Option[PaymentId],
      externalId: Option[String]): Future[SendToRouteResult] = {
    unsupportedFailure
  }

  override def forceClose(channelId: ChannelId): Future[Unit] =
    unsupportedFailure

  override def forceClose(shortChannelId: ShortChannelId): Future[Unit] =
    unsupportedFailure

  override def close(id: ChannelId, spk: ScriptPubKey): Future[Unit] =
    unsupportedFailure

  override def usableBalances(): Future[Vector[UsableBalancesResult]] =
    unsupportedFailure

  override def getNodeURI: Future[NodeUri] =
    unsupportedFailure

  override def networkFees(
      from: Option[FiniteDuration],
      to: Option[FiniteDuration]): Future[Vector[NetworkFeesResult]] = {
    unsupportedFailure
  }

  override def disconnect(nodeId: NodeId): Future[Unit] =
    unsupportedFailure

  override def channels(nodeId: NodeId): Future[Vector[ChannelInfo]] =
    unsupportedFailure

  override def channel(id: ChannelId): Future[ChannelResult] =
    unsupportedFailure

  override def open(
      nodeId: NodeId,
      funding: CurrencyUnit,
      pushMsat: Option[MilliSatoshis],
      feerateSatPerByte: Option[SatoshisPerByte],
      channelFlags: Option[Byte],
      openTimeout: Option[FiniteDuration]): Future[FundedChannelId] = {
    unsupportedFailure
  }

  override def getPeers: Future[Vector[PeerInfo]] =
    unsupportedFailure

  override def parseInvoice(invoice: LnInvoice): Future[InvoiceResult] =
    unsupportedFailure

  override def getReceivedInfo(
      paymentHash: Sha256Digest
  ): Future[Option[IncomingPayment]] =
    unsupportedFailure

  override def getReceivedInfo(
      invoice: LnInvoice
  ): Future[Option[IncomingPayment]] =
    unsupportedFailure

  override def channelStats(): Future[Vector[ChannelStats]] =
    unsupportedFailure

  override def listInvoices(
      from: Option[Instant],
      to: Option[Instant]
  ): Future[Vector[LnInvoice]] =
    unsupportedFailure
}
