package org.bitcoins.commons.jsonmodels.eclair

import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.ln.channel.{
  ChannelId,
  ChannelState,
  FundedChannelId,
  ShortChannelId
}
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.fee.FeeProportionalMillionths
import org.bitcoins.core.protocol.ln.node.{Feature, FeatureSupport, NodeId}
import org.bitcoins.core.protocol.ln.{LnHumanReadablePart, PaymentPreimage}
import org.bitcoins.crypto._
import play.api.libs.json.JsObject

import java.net.InetSocketAddress
import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

sealed abstract class EclairModels

case class GetInfoResult(
    version: String,
    nodeId: NodeId,
    alias: String,
    features: Features,
    chainHash: DoubleSha256Digest,
    network: BitcoinNetwork,
    blockHeight: Long,
    publicAddresses: Seq[InetSocketAddress],
    instanceId: UUID)

case class PeerInfo(
    nodeId: NodeId,
    state: PeerState,
    address: Option[String],
    channels: Int)

case class ChannelCommandResult(
    results: scala.collection.Map[
      Either[ShortChannelId, FundedChannelId],
      State]
)

case class UpdateRelayFeeResult(
    results: Map[Either[ShortChannelId, FundedChannelId], UpdateRelayFee])

sealed trait UpdateRelayFee

object UpdateRelayFee {

  case class OK(
      channelId: ChannelId,
      feeBaseMsat: MilliSatoshis,
      feeProportionalMillionths: Long)
      extends UpdateRelayFee

  case class Error(message: String) extends UpdateRelayFee
}

sealed trait State

object ChannelCommandResult extends StringFactory[State] {

  case object OK extends State
  case object ChannelOpened extends State
  case object ChannelClosed extends State
  case class Error(message: String) extends State

  override def fromString(s: String): State =
    if (s == "ok") {
      ChannelCommandResult.OK
    } else if (s.startsWith("created channel ")) {
      ChannelCommandResult.ChannelOpened
    } else if (s.startsWith("closed channel ")) {
      ChannelCommandResult.ChannelClosed
    } else {
      ChannelCommandResult.Error(s)
    }
}

/**
  * This is the data model returned by the RPC call
  * `channels nodeId`. The content of the objects
  * being returne differ based on whatever state
  * the channel is in. The member of this abstract
  * class are in eveyr channel state, whereas other
  * channel states may have extra information.
  */
sealed abstract class ChannelInfo {
  def nodeId: NodeId
  def channelId: FundedChannelId
  def localMsat: MilliSatoshis
  def remoteMsat: MilliSatoshis
  def state: ChannelState
}

/**
  * This represents the case where the
  * [[org.bitcoins.core.protocol.ln.channel.ChannelState ChannelState]] is
  * undetermined
  */
case class BaseChannelInfo(
    nodeId: NodeId,
    channelId: FundedChannelId,
    localMsat: MilliSatoshis,
    remoteMsat: MilliSatoshis,
    state: ChannelState
) extends ChannelInfo

/**
  * This represents the case where the channel is
  * in state `NORMAL` (i.e. an open channel)
  */
case class OpenChannelInfo(
    nodeId: NodeId,
    shortChannelId: ShortChannelId,
    channelId: FundedChannelId,
    localMsat: MilliSatoshis,
    remoteMsat: MilliSatoshis,
    state: ChannelState.NORMAL.type
) extends ChannelInfo

case class ActivatedFeature(feature: Feature, support: FeatureSupport)

case class UnknownFeature(bitIndex: Int)

case class Features(
    activated: Set[ActivatedFeature],
    unknown: Set[UnknownFeature])

case class NodeInfo(
    signature: ECDigitalSignature,
    features: Features,
    timestamp: Instant,
    nodeId: NodeId,
    rgbColor: String,
    alias: String,
    addresses: Vector[InetSocketAddress],
    unknownFields: String)

case class ChannelDesc(shortChannelId: ShortChannelId, a: NodeId, b: NodeId)

case class AuditResult(
    sent: Vector[SentPayment],
    relayed: Vector[RelayedPayment],
    received: Vector[ReceivedPayment]
)

case class NetworkFeesResult(
    remoteNodeId: NodeId,
    channelId: FundedChannelId,
    txId: DoubleSha256DigestBE,
    fee: Satoshis,
    txType: String,
    timestamp: Instant //milliseconds
)

case class ChannelStats(
    channelId: FundedChannelId,
    direction: ChannelStats.Direction,
    avgPaymentAmount: Satoshis,
    paymentCount: Long,
    relayFee: Satoshis,
    networkFee: Satoshis
)

object ChannelStats {
  sealed trait Direction
  case object In extends Direction
  case object Out extends Direction

  object Direction extends StringFactory[Direction] {

    override def fromString(s: String): Direction =
      if (s.toUpperCase == "IN") {
        ChannelStats.In
      } else if (s.toUpperCase == "OUT") {
        ChannelStats.Out
      } else {
        throw new RuntimeException(s"Unknown payment direction: `$s`")
      }
  }
}

case class UsableBalancesResult(
    remoteNodeId: NodeId,
    shortChannelId: ShortChannelId,
    canSend: MilliSatoshis,
    canReceive: MilliSatoshis,
    isPublic: Boolean
)

case class ReceivedPayment(
    paymentHash: Sha256Digest,
    parts: Vector[ReceivedPayment.Part]
)

object ReceivedPayment {

  case class Part(
      amount: MilliSatoshis,
      fromChannelId: FundedChannelId,
      timestamp: Instant //milliseconds
  )
}

case class RelayedPayment(
    amountIn: MilliSatoshis,
    amountOut: MilliSatoshis,
    paymentHash: Sha256Digest,
    fromChannelId: FundedChannelId,
    toChannelId: FundedChannelId,
    timestamp: Instant //milliseconds
)

case class SentPayment(
    id: PaymentId,
    paymentHash: Sha256Digest,
    paymentPreimage: PaymentPreimage,
    recipientAmount: MilliSatoshis,
    recipientNodeId: NodeId,
    parts: Vector[SentPayment.Part]
)

object SentPayment {

  case class Part(
      id: PaymentId,
      amount: MilliSatoshis,
      feesPaid: MilliSatoshis,
      toChannelId: FundedChannelId,
      timestamp: Instant //milliseconds
  )
}

case class ChannelUpdate(
    signature: ECDigitalSignature,
    chainHash: DoubleSha256Digest,
    shortChannelId: ShortChannelId,
    timestamp: Instant, //seconds
    messageFlags: Int,
    channelFlags: Int,
    cltvExpiryDelta: Int,
    htlcMinimumMsat: MilliSatoshis,
    feeProportionalMillionths: FeeProportionalMillionths,
    htlcMaximumMsat: Option[MilliSatoshis],
    feeBaseMsat: MilliSatoshis)

case class ChannelResult(
    nodeId: NodeId,
    channelId: FundedChannelId,
    state: ChannelState,
    feeBaseMsat: Option[MilliSatoshis],
    feeProportionalMillionths: Option[FeeProportionalMillionths],
    data: JsObject) {

  lazy val shortChannelId: Option[ShortChannelId] =
    (data \ "shortChannelId").validate[ShortChannelId].asOpt
}

// ChannelResult ends here

case class InvoiceResult(
    prefix: LnHumanReadablePart,
    timestamp: Instant, //seconds
    nodeId: NodeId,
    serialized: String,
    description: String,
    paymentHash: Sha256Digest,
    expiry: FiniteDuration)

case class PaymentId(value: UUID) {
  override def toString: String = value.toString
}

case class SendToRouteResult(paymentId: PaymentId, parentId: PaymentId)

case class PaymentRequest(
    prefix: LnHumanReadablePart,
    timestamp: Instant, //seconds
    nodeId: NodeId,
    serialized: String,
    description: String,
    paymentHash: Sha256Digest,
    expiry: FiniteDuration, //seconds
    amount: Option[MilliSatoshis])

sealed trait PaymentType

object PaymentType extends StringFactory[PaymentType] {

  case object Standard extends PaymentType
  case object SwapIn extends PaymentType
  case object SwapOut extends PaymentType

  override def fromString(str: String): PaymentType =
    str match {
      case "Standard" => Standard
      case "SwapIn"   => SwapIn
      case "SwapOut"  => SwapOut
      case _          => throw new RuntimeException(s"Unknown payment type `$str`")
    }

}

case class OutgoingPayment(
    id: PaymentId,
    parentId: PaymentId,
    externalId: Option[String],
    paymentHash: Sha256Digest,
    paymentType: PaymentType,
    amount: MilliSatoshis,
    recipientAmount: MilliSatoshis,
    recipientNodeId: NodeId,
    createdAt: Instant, //milliseconds
    paymentRequest: Option[PaymentRequest],
    status: OutgoingPaymentStatus)

case class IncomingPayment(
    paymentRequest: PaymentRequest,
    paymentPreimage: PaymentPreimage,
    createdAt: Instant, //milliseconds
    status: IncomingPaymentStatus)

sealed trait IncomingPaymentStatus

object IncomingPaymentStatus {

  case object Pending extends IncomingPaymentStatus

  case object Expired extends IncomingPaymentStatus

  case class Received(
      amount: MilliSatoshis,
      receivedAt: Instant //milliseconds
  ) extends IncomingPaymentStatus

}

sealed trait OutgoingPaymentStatus

object OutgoingPaymentStatus {
  case object Pending extends OutgoingPaymentStatus

  case class Succeeded(
      paymentPreimage: PaymentPreimage,
      feesPaid: MilliSatoshis,
      route: Seq[Hop],
      completedAt: Instant //milliseconds
  ) extends OutgoingPaymentStatus

  case class Failed(failures: Seq[PaymentFailure], completedAt: Instant)
      extends OutgoingPaymentStatus
}

case class PaymentFailure(
    failureType: PaymentFailure.Type,
    failureMessage: String,
    failedRoute: Seq[Hop])

object PaymentFailure {
  sealed trait Type
  case object Local extends Type
  case object Remote extends Type
  case object UnreadableRemote extends Type
}

case class Hop(
    nodeId: NodeId,
    nextNodeId: NodeId,
    shortChannelId: Option[ShortChannelId])

sealed trait WebSocketEvent

object WebSocketEvent {

  case class PaymentRelayed(
      amountIn: MilliSatoshis,
      amountOut: MilliSatoshis,
      paymentHash: Sha256Digest,
      fromChannelId: FundedChannelId,
      toChannelId: FundedChannelId,
      timestamp: Instant //milliseconds
  ) extends WebSocketEvent

  case class PaymentReceived(
      paymentHash: Sha256Digest,
      parts: Vector[PaymentReceived.Part]
  ) extends WebSocketEvent

  object PaymentReceived {

    case class Part(
        amount: MilliSatoshis,
        fromChannelId: FundedChannelId,
        timestamp: Instant // milliseconds
    )
  }

  case class PaymentFailed(
      id: PaymentId,
      paymentHash: Sha256Digest,
      failures: Vector[String],
      timestamp: Instant // milliseconds
  ) extends WebSocketEvent

  case class PaymentSent(
      id: PaymentId,
      paymentHash: Sha256Digest,
      paymentPreimage: PaymentPreimage,
      parts: Vector[PaymentSent.Part]
  ) extends WebSocketEvent

  object PaymentSent {

    case class Part(
        id: PaymentId,
        amount: MilliSatoshis,
        feesPaid: MilliSatoshis,
        toChannelId: FundedChannelId,
        timestamp: Instant // milliseconds
    )
  }

  case class PaymentSettlingOnchain(
      amount: MilliSatoshis,
      paymentHash: Sha256Digest,
      timestamp: Instant //milliseconds
  ) extends WebSocketEvent

}

case class OnChainBalance(confirmed: Satoshis, unconfirmed: Satoshis)

case class WalletTransaction(
    address: String,
    amount: Satoshis,
    fees: Satoshis,
    blockHash: DoubleSha256DigestBE,
    confirmations: Long,
    txid: DoubleSha256DigestBE,
    timestamp: Long)
