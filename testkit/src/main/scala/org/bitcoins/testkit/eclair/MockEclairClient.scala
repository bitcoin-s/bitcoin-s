package org.bitcoins.testkit.eclair

import java.net.InetSocketAddress
import java.time.Instant
import java.util.concurrent.locks.ReentrantReadWriteLock

import org.bitcoins.commons.jsonmodels.eclair._
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.ln.LnParams.LnBitcoinRegTest
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.channel.{ChannelId, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.{LnCurrencyUnits, MilliSatoshis}
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.{Address, BitcoinAddress}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey, ECPublicKey, Sha256Digest}
import org.bitcoins.eclair.rpc.api.EclairApi
import org.bitcoins.eclair.rpc.network.NodeUri

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{ExecutionContext, Future}

class MockEclairClient()(implicit
    override val executionContext: ExecutionContext)
    extends EclairApi
    with BitcoinSLogger {
  private def now: Instant = Instant.now

  private val privKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  private val readWriteLock = new ReentrantReadWriteLock
  private val readLock = readWriteLock.readLock
  private val writeLock = readWriteLock.writeLock

  private var paymentsById =
    Map.empty[PaymentId, Vector[OutgoingPayment]].withDefaultValue(Vector.empty)

  private var paymentsByHash = Map
    .empty[Sha256Digest, Vector[OutgoingPayment]]
    .withDefaultValue(Vector.empty)

  private var preImagesByHash: Map[LnTag.PaymentHashTag, PaymentPreimage] =
    Map.empty

  private var outgoingPayments: Vector[OutgoingPayment] = Vector.empty

  private var incomingPayments: Vector[IncomingPayment] = Vector.empty

  var otherClient: Option[MockEclairClient] = None

  def preImage(hashTag: LnTag.PaymentHashTag): PaymentPreimage = {
    readLock.lock()
    try {
      preImagesByHash(hashTag)
    } finally {
      readLock.unlock()
    }
  }

  def outgoingPayment(hash: LnTag.PaymentHashTag): OutgoingPayment = {
    readLock.lock()
    try {
      paymentsByHash(hash.hash).head
    } finally {
      readLock.unlock()
    }
  }

  def incomingPayment(hash: LnTag.PaymentHashTag): IncomingPayment = {
    readLock.lock()
    try {
      incomingPayments.find(_.paymentRequest.paymentHash == hash.hash).get
    } finally {
      readLock.unlock()
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

  def receiveIncomingPayment(
      invoice: LnInvoice
  ): Unit = {
    readLock.lock()
    val incoming =
      try {
        IncomingPayment(
          toPaymentRequest(invoice),
          paymentPreimage = preImagesByHash(invoice.lnTags.paymentHash),
          createdAt = now,
          status = IncomingPaymentStatus
            .Received(invoice.amount.getOrElse(LnCurrencyUnits.zero).toMSat,
                      now)
        )
      } finally {
        readLock.unlock()
      }

    writeLock.lock()
    incomingPayments = incomingPayments.:+(incoming)
    writeLock.unlock()
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

    val preImage = paymentPreimage.getOrElse(PaymentPreimage.random)
    val hash = CryptoUtil.sha256(preImage.bytes)
    val hashTag = LnTag.PaymentHashTag(hash)

    nodeId().map { id =>
      val taggedFields = LnTaggedFields(
        paymentHash = hashTag,
        descriptionOrHash = Left(LnTag.DescriptionTag(description)),
        nodeId = Some(LnTag.NodeIdTag(id)),
        expiryTime =
          expireIn.map(x => LnTag.ExpiryTimeTag(UInt32(x.toSeconds))),
        cltvExpiry = None,
        fallbackAddress = None,
        routingInfo = None
      )

      val invoice = LnInvoice.build(
        hrp = hrp,
        lnTags = taggedFields,
        privateKey = privKey
      )

      writeLock.lock()
      preImagesByHash = preImagesByHash.updated(hashTag, preImage)
      writeLock.unlock()

      invoice
    }
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
    val nodeId = invoice.nodeId
    val amountMsat = invoice.amount.getOrElse(LnCurrencyUnits.zero).toMSat
    val paymentHash = invoice.lnTags.paymentHash.hash

    val outgoingPaymentStatus = otherClient match {
      case Some(payee) =>
        val status = OutgoingPaymentStatus.Succeeded(
          paymentPreimage = payee.preImagesByHash(invoice.lnTags.paymentHash),
          feesPaid = MilliSatoshis.zero,
          route = Vector.empty,
          completedAt = now)

        payee.receiveIncomingPayment(invoice)

        status
      case None =>
        OutgoingPaymentStatus.Failed(failures = Vector.empty)
    }

    val outgoingPayment = OutgoingPayment(
      id = paymentId,
      parentId = paymentId,
      externalId = Some(paymentId.toString()),
      paymentHash = paymentHash,
      amount = amountMsat,
      createdAt = now,
      paymentRequest = Some(toPaymentRequest(invoice)),
      status = outgoingPaymentStatus,
      recipientAmount = amountMsat,
      recipientNodeId = nodeId,
      paymentType = PaymentType.Standard
    )

    writeLock.lock()
    outgoingPayments = outgoingPayments.:+(outgoingPayment)
    paymentsById = paymentsById.updated(paymentId, Vector(outgoingPayment))
    paymentsByHash = paymentsByHash
      .updated(paymentHash, outgoingPayment +: paymentsByHash(paymentHash))
    writeLock.unlock()

    Future.successful(paymentId)
  }

  override def payInvoice(
      invoice: LnInvoice,
      amt: MilliSatoshis
  ): Future[PaymentId] = {
    unsupportedFailure
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
      paymentHash: Sha256Digest): Future[Vector[OutgoingPayment]] =
    Future {
      readLock.lock()
      try {
        paymentsByHash(paymentHash)
      } finally {
        readLock.unlock()
      }
    }

  override def getSentInfo(id: PaymentId): Future[Vector[OutgoingPayment]] =
    Future {
      readLock.lock()
      try {
        paymentsById(id)
      } finally {
        readLock.unlock()
      }
    }

  override def monitorInvoice(
      lnInvoice: LnInvoice,
      interval: FiniteDuration,
      maxAttempts: Int
  ): Future[IncomingPayment] = {
    val paymentOpt = incomingPayments.find(incoming =>
      incoming.paymentRequest.paymentHash == lnInvoice.lnTags.paymentHash.hash)
    paymentOpt match {
      case Some(payment) => Future.successful(payment)
      case None =>
        if (maxAttempts <= 0) {
          Future.failed(
            new RuntimeException(s"No incoming payment for $lnInvoice"))
        } else {
          Thread.sleep(interval.toMillis)
          monitorInvoice(lnInvoice, interval, maxAttempts - 1)
        }
    }
  }

  override def monitorSentPayment(
      paymentId: PaymentId,
      interval: FiniteDuration,
      maxAttempts: Int): Future[OutgoingPayment] = {
    for {
      results <- getSentInfo(paymentId)
    } yield {

      val result = results.filter(_.parentId == paymentId)
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
      readLock.lock()
      try {
        paymentsById(id).head
      } finally {
        readLock.unlock()
      }
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
      readLock.lock()
      try {
        paymentsById(id).head
      } finally {
        readLock.unlock()
      }
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
      eventHandler: WebSocketEvent => Unit): scala.concurrent.Future[Unit] = {
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
      route: scala.collection.immutable.Seq[NodeId],
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

  override lazy val getNodeURI: Future[NodeUri] = {
    val uri =
      NodeUri(NodeId(privKey.publicKey), "MOCK", scala.util.Random.nextInt())
    Future.successful(uri)
  }

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
