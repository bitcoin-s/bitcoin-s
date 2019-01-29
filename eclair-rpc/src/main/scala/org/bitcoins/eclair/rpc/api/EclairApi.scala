package org.bitcoins.eclair.rpc.api

import org.bitcoins.core.crypto.Sha256Digest
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.ln.{LnInvoice, LnParams, ShortChannelId}
import org.bitcoins.core.protocol.ln.channel.{ChannelId, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.{LnCurrencyUnit, MilliSatoshis}
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.eclair.rpc.json._
import org.bitcoins.eclair.rpc.network.NodeUri

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

  def checkInvoice(invoice: LnInvoice): Future[PaymentRequest]

  def checkPayment(
      invoiceOrHash: Either[LnInvoice, Sha256Digest]): Future[Boolean]

  def connect(nodeURI: NodeUri): Future[String]

  def close(id: ChannelId, spk: ScriptPubKey): Future[String]

  def findRoute(nodeId: NodeId): Future[Vector[NodeId]]

  def findRoute(invoice: LnInvoice): Future[Vector[NodeId]]

  def forceClose(id: ChannelId): Future[String]

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

  def receive(
      amountMsat: LnCurrencyUnit,
      description: String): Future[LnInvoice]

  def receive(
      amountMsat: Option[LnCurrencyUnit],
      description: Option[String],
      expirySeconds: Option[Long]): Future[LnInvoice]

  def send(paymentRequest: LnInvoice): Future[PaymentResult]

  def send(invoice: LnInvoice, amount: LnCurrencyUnit): Future[PaymentResult]

}
