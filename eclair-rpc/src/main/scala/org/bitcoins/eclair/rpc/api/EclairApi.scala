package org.bitcoins.eclair.rpc.api

import java.net.InetSocketAddress
import java.time.Instant

import org.bitcoins.commons.jsonmodels.eclair.{
  AuditResult,
  ChannelDesc,
  ChannelInfo,
  ChannelResult,
  ChannelStats,
  ChannelUpdate,
  GetInfoResult,
  IncomingPayment,
  InvoiceResult,
  NetworkFeesResult,
  NodeInfo,
  OutgoingPayment,
  PaymentId,
  PeerInfo,
  SendToRouteResult,
  UsableBalancesResult,
  WebSocketEvent
}
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
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.eclair.rpc.network.NodeUri

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/**
  * This trait defines methods to interact with the Eclair lightning node via its API.
  *
  * @see [[https://acinq.github.io/eclair/]]
  */
trait EclairApi {

  implicit def executionContext: ExecutionContext

  def allChannels(): Future[Vector[ChannelDesc]]

  def allNodes(): Future[Vector[NodeInfo]]

  /**
    * List all sent/received/relayed payments
    */
  def audit(): Future[AuditResult]

  /**
    * List all sent/received/relayed payments in the given interval
    * @param from start timestamp
    * @param to end timestamp
    */
  def audit(from: Option[Instant], to: Option[Instant]): Future[AuditResult]

  def allUpdates(): Future[Vector[ChannelUpdate]]

  def allUpdates(nodeId: NodeId): Future[Vector[ChannelUpdate]]

  def allUpdates(
      nodeIdOpt: Option[NodeId] = None): Future[Vector[ChannelUpdate]] =
    nodeIdOpt match {
      case Some(nodeId) => allUpdates(nodeId)
      case None         => allUpdates()
    }

  def channels(nodeId: NodeId): Future[Vector[ChannelInfo]]

  def channel(id: ChannelId): Future[ChannelResult]

  def channelStats(): Future[Vector[ChannelStats]]

  def connect(nodeURI: NodeUri): Future[Unit]

  def connect(nodeId: NodeId, host: String, port: Int): Future[Unit]

  def connect(nodeId: NodeId, addr: InetSocketAddress): Future[Unit]

  def connect(nodeId: NodeId): Future[Unit]

  def disconnect(nodeId: NodeId): Future[Unit]

  def close(id: ChannelId, spk: ScriptPubKey): Future[Unit]

  def findRoute(
      nodeId: NodeId,
      amountMsat: MilliSatoshis): Future[Vector[NodeId]]

  def findRoute(invoice: LnInvoice): Future[Vector[NodeId]]

  def findRoute(
      invoice: LnInvoice,
      amountMsat: MilliSatoshis): Future[Vector[NodeId]]

  def forceClose(channelId: ChannelId): Future[Unit]

  def forceClose(shortChannelId: ShortChannelId): Future[Unit]

  def getInfo: Future[GetInfoResult]

  def getNodeURI: Future[NodeUri]

  def getPeers: Future[Vector[PeerInfo]]

  def isConnected(nodeId: NodeId): Future[Boolean]

  def updateRelayFee(
      channelId: ChannelId,
      feeBaseMsat: MilliSatoshis,
      feePropertionalMillionths: Long): Future[Unit]

  def updateRelayFee(
      shortChannelId: ShortChannelId,
      feeBaseMsat: MilliSatoshis,
      feePropertionalMillionths: Long
  ): Future[Unit]

  def open(
      nodeId: NodeId,
      funding: CurrencyUnit,
      pushMsat: Option[MilliSatoshis],
      feerateSatPerByte: Option[SatoshisPerByte],
      channelFlags: Option[Byte],
      openTimeout: Option[FiniteDuration]): Future[FundedChannelId]

  /** The network that this [[org.bitcoins.eclair.rpc.api.EclairApi EclairApi]] is
    * running on. This is not available directly from the eclair api, but is a very
    * useful helper method
    * @return
    */
  def network: LnParams

  def networkFees(
      from: Option[FiniteDuration],
      to: Option[FiniteDuration]): Future[Vector[NetworkFeesResult]]

  def nodeId(): Future[NodeId] = {
    getNodeURI.map(_.nodeId)
  }

  def createInvoice(description: String): Future[LnInvoice]

  def createInvoice(
      description: String,
      amountMsat: MilliSatoshis): Future[LnInvoice]

  def createInvoice(
      description: String,
      amountMsat: MilliSatoshis,
      expireIn: FiniteDuration): Future[LnInvoice]

  def createInvoice(
      description: String,
      amountMsat: MilliSatoshis,
      paymentPreimage: PaymentPreimage): Future[LnInvoice]

  def createInvoice(
      description: String,
      amountMsat: MilliSatoshis,
      expireIn: FiniteDuration,
      paymentPreimage: PaymentPreimage): Future[LnInvoice]

  def createInvoice(
      description: String,
      amountMsat: Option[MilliSatoshis],
      expireIn: Option[FiniteDuration],
      fallbackAddress: Option[Address],
      paymentPreimage: Option[PaymentPreimage]): Future[LnInvoice]

  /**
    * Returns a future that is completed when this invoice has been paid too.
    * This also publishes the [[IncomingPayment received payment result]] to the event bush
    * when the payment is received
    *
    * @param lnInvoice the invoice to monitor
    * @param maxAttempts the number of attempts we ping eclair until we fail the returned future. Pinging occurrs every 1 second
    * */
  def monitorInvoice(
      lnInvoice: LnInvoice,
      interval: FiniteDuration,
      maxAttempts: Int): Future[IncomingPayment]

  def getInvoice(paymentHash: Sha256Digest): Future[LnInvoice]

  def listInvoices(
      from: Option[Instant],
      to: Option[Instant]): Future[Vector[LnInvoice]]

  def listPendingInvoices(
      from: Option[Instant],
      to: Option[Instant]): Future[Vector[LnInvoice]]

  def parseInvoice(invoice: LnInvoice): Future[InvoiceResult]

  def payInvoice(invoice: LnInvoice): Future[PaymentId]

  def payInvoice(invoice: LnInvoice, amount: MilliSatoshis): Future[PaymentId]

  def payInvoice(
      invoice: LnInvoice,
      externalId: Option[String]): Future[PaymentId]

  def payInvoice(
      invoice: LnInvoice,
      amount: MilliSatoshis,
      externalId: Option[String]): Future[PaymentId]

  def payInvoice(
      invoice: LnInvoice,
      amountMsat: Option[MilliSatoshis],
      maxAttempts: Option[Int],
      feeThresholdSat: Option[Satoshis],
      maxFeePct: Option[Int],
      externalId: Option[String]): Future[PaymentId]

  /**
    * Pings eclair to see if a invoice has been paid and returns [[OutgoingPayment PaymentResult]]
    *
    * @param paymentId the payment id returnned by [[org.bitcoins.eclair.rpc.api.EclairApi.payInvoice payInvoice]]
    * @param interval the ping interval
    * @param maxAttempts the maximum number of pings
    *
    */
  def monitorSentPayment(
      paymentId: PaymentId,
      interval: FiniteDuration,
      maxAttempts: Int): Future[OutgoingPayment]

  def payAndMonitorInvoice(
      invoice: LnInvoice,
      externalId: Option[String],
      interval: FiniteDuration,
      maxAttempts: Int): Future[OutgoingPayment] =
    for {
      paymentId <- payInvoice(invoice, externalId)
      paymentResult <- monitorSentPayment(paymentId, interval, maxAttempts)
    } yield paymentResult

  def payAndMonitorInvoice(
      invoice: LnInvoice,
      amount: MilliSatoshis,
      externalId: Option[String],
      interval: FiniteDuration,
      maxAttempts: Int): Future[OutgoingPayment] =
    for {
      paymentId <- payInvoice(invoice, amount, externalId)
      paymentResult <- monitorSentPayment(paymentId, interval, maxAttempts)
    } yield paymentResult

  def getSentInfo(paymentHash: Sha256Digest): Future[Vector[OutgoingPayment]]

  def getSentInfo(id: PaymentId): Future[Vector[OutgoingPayment]]

  def getReceivedInfo(
      paymentHash: Sha256Digest): Future[Option[IncomingPayment]]

  def getReceivedInfo(invoice: LnInvoice): Future[Option[IncomingPayment]]

  def sendToNode(
      nodeId: NodeId,
      amountMsat: MilliSatoshis,
      paymentHash: Sha256Digest,
      maxAttempts: Option[Int],
      feeThresholdSat: Option[Satoshis],
      maxFeePct: Option[Int],
      externalId: Option[String]): Future[PaymentId]

  /**
    * Documented by not implemented in Eclair
    */
  def sendToRoute(
      invoice: LnInvoice,
      route: scala.collection.immutable.Seq[NodeId],
      amountMsat: MilliSatoshis,
      paymentHash: Sha256Digest,
      finalCltvExpiry: Long,
      recipientAmountMsat: Option[MilliSatoshis],
      parentId: Option[PaymentId],
      externalId: Option[String]): Future[SendToRouteResult]

  def usableBalances(): Future[Vector[UsableBalancesResult]]

  /** Connects to the Eclair web socket end point and passes [[WebSocketEvent]]s to the given [[eventHandler]] */
  def connectToWebSocket(eventHandler: WebSocketEvent => Unit): Future[Unit]

  def getNewAddress(): Future[BitcoinAddress]
}
