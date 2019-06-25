package org.bitcoins.eclair.rpc.api

import org.bitcoins.core.crypto.Sha256Digest
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.{Address, NetworkElement}
import org.bitcoins.core.protocol.ln.{LnInvoice, LnParams, PaymentId, PaymentPreimage, ShortChannelId}
import org.bitcoins.core.protocol.ln.channel.{ChannelId, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.{LnCurrencyUnit, MilliSatoshis}
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.eclair.rpc.json._
import org.bitcoins.eclair.rpc.network.NodeUri

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

trait EclairApi {

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
  def audit(from: Long, to: Long): Future[AuditResult]

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

  def connect(nodeURI: NodeUri): Future[String]

  def connect(nodeId: NodeId, host: String, port: Int): Future[String]

  def close(id: ChannelId, spk: ScriptPubKey): Future[String]

  def findRoute(nodeId: NodeId, amountMsat: MilliSatoshis): Future[Vector[NodeId]]

  def findRoute(invoice: LnInvoice): Future[Vector[NodeId]]

  def findRoute(invoice: LnInvoice, amountMsat: MilliSatoshis): Future[Vector[NodeId]]

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
      fundingSatoshis: CurrencyUnit,
      pushMsat: Option[MilliSatoshis],
      feerateSatPerByte: Option[SatoshisPerByte],
      channelFlags: Option[Byte]): Future[FundedChannelId]

  /** The network that this [[org.bitcoins.eclair.rpc.api.EclairApi EclairApi]] is
    * running on. This is not available directly from the eclair api, but is a very
    * useful helper method
    * @return
    */
  def network: LnParams

  def nodeId()(implicit ec: ExecutionContext): Future[NodeId] = {
    getNodeURI.map(_.nodeId)
  }

  def createInvoice(description: String, amountMsat: Option[LnCurrencyUnit], expireIn: Option[FiniteDuration], fallbackAddress: Option[Address], paymentPreimage: Option[PaymentPreimage]): Future[LnInvoice]

  def parseInvoice(nvoice: LnInvoice): Future[InvoiceResult]

  def payInvoice(invoice: LnInvoice, amountMsat: Option[LnCurrencyUnit], maxAttempts: Option[Int], feeThresholdSat: Option[LnCurrencyUnit], maxFeePct: Option[Int]): Future[PaymentId]

  def getSentInfo(paymentHash: Sha256Digest): Future[Vector[PaymentResult]]

  def getSentInfo(id: PaymentId): Future[Vector[PaymentResult]]

  def getReceivedInfo(paymentHash: Sha256Digest): Future[ReceivedPaymentResult]

  def getReceivedInfo(invoice: LnInvoice): Future[ReceivedPaymentResult]

  def sendToNode(nodeId: NodeId, amountMsat: LnCurrencyUnit, paymentHash: Sha256Digest, maxAttempts: Option[Int], feeThresholdSat: Option[LnCurrencyUnit], maxFeePct: Option[Int]): Future[PaymentId]

  /**
    * Documented by not implemented in Eclair
    */
  def sendToRoute(route: TraversableOnce[NodeId], amountMsat: LnCurrencyUnit, paymentHash: Sha256Digest, finalCltvExpiry: Long): Future[PaymentId]

}
