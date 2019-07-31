package org.bitcoins.eclair.rpc.json

import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE, ECDigitalSignature, Sha256Digest}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.ln.channel.{ChannelState, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.fee.FeeProportionalMillionths
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.ln.{LnHumanReadablePart, LnInvoiceSignature, PaymentPreimage, ShortChannelId}
import org.bitcoins.eclair.rpc.network.PeerState
import play.api.libs.json.JsObject

import scala.concurrent.duration.FiniteDuration

sealed abstract class EclairModels

case class GetInfoResult(
    nodeId: NodeId,
    alias: String,
    chainHash: DoubleSha256Digest,
    blockHeight: Long,
    publicAddresses: Seq[String])

case class PeerInfo(
    nodeId: NodeId,
    state: PeerState,
    address: Option[String],
    channels: Int)

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

case class NodeInfo(
    signature: ECDigitalSignature,
    features: String,
    timestamp: Long,
    nodeId: NodeId,
    rgbColor: String,
    alias: String,
    addresses: Vector[String])

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
    feeSat: Satoshis,
    txType: String,
    timestamp: FiniteDuration
)

case class ChannelStats(
    channelId: FundedChannelId,
    avgPaymentAmountSatoshi: Satoshis,
    paymentCount: Long,
    relayFeeSatoshi: Satoshis,
    networkFeeSatoshi: Satoshis
)

case class UsableBalancesResult(
    canSendMsat: MilliSatoshis,
    canReceiveMsat: MilliSatoshis,
    isPublic: Boolean
)

case class ReceivedPayment(
    amount: MilliSatoshis,
    paymentHash: Sha256Digest,
    fromChannelId: FundedChannelId,
    timestamp: FiniteDuration
)

case class RelayedPayment(
    amountIn: MilliSatoshis,
    amountOut: MilliSatoshis,
    paymentHash: Sha256Digest,
    fromChannelId: FundedChannelId,
    toChannelId: FundedChannelId,
    timestamp: FiniteDuration
)

case class SentPayment(
    amount: MilliSatoshis,
    feesPaid: MilliSatoshis,
    paymentHash: Sha256Digest,
    paymentPreimage: String,
    toChannelId: FundedChannelId,
    timestamp: FiniteDuration
)

case class ChannelUpdate(
    signature: ECDigitalSignature,
    chainHash: DoubleSha256Digest,
    shortChannelId: ShortChannelId,
    timestamp: Long,
    messageFlags: Int,
    channelFlags: Int,
    cltvExpiryDelta: Int,
    htlcMinimumMsat: MilliSatoshis,
    feeProportionalMillionths: FeeProportionalMillionths,
    htlcMaximumMsat: Option[MilliSatoshis],
    feeBaseMsat: MilliSatoshis)

/* ChannelResult starts here, some of this may be useful but it seems that data is different at different times

case class CommitInput(
  outPoint: String,
  amountSatoshis: Long)
implicit val commitInputReads: Reads[CommitInput] =
  Json.reads[CommitInput]

case class CommitChanges(
  proposed: Vector[String], // IDK WHAT TYPE THIS SHOULD BE
  signed: Vector[String], // IDK WHAT TYPE THIS SHOULD BE
  acked: Vector[String] // IDK WHAT TYPE THIS SHOULD BE
)
implicit val commitChangesReads: Reads[CommitChanges] =
  Json.reads[CommitChanges]

case class CommitSpec(
  htlcs: Vector[String],
  feeratePerKw: Long,
  toLocalMsat: Long,
  toRemoteMsat: Long)
implicit val commitSpecReads: Reads[CommitSpec] =
  Json.reads[CommitSpec]

case class RemoteCommit(
  index: Int,
  spec: CommitSpec,
  txid: String,
  remotePerCommitmentPoint: String)
implicit val remoteCommitReads: Reads[RemoteCommit] =
  Json.reads[RemoteCommit]

case class PublishableTxs(
  commitTx: String,
  htlcTxsAndSigs: Vector[String])
implicit val publishableTxsReads: Reads[PublishableTxs] =
  Json.reads[PublishableTxs]

case class LocalCommit(
  index: Int,
  spec: CommitSpec,
  publishableTxs: PublishableTxs)
implicit val localCommitReads: Reads[LocalCommit] =
  Json.reads[LocalCommit]

case class RemoteParams(
  nodeId: String,
  dustLimitSatoshis: Long,
  maxHtlcValueInFlightMsat: Long,
  channelReserveSatoshis: Long,
  htlcMinimumMsat: Long,
  toSelfDelay: Long,
  maxAcceptedHtlcs: Long,
  fundingPubKey: String,
  revocationBasepoint: String,
  paymentBasepoint: String,
  delayedPaymentBasepoint: String,
  htlcBasepoint: String,
  globalFeatures: String,
  localFeatures: String)
implicit val remoteParamsReads: Reads[RemoteParams] =
  Json.reads[RemoteParams]

case class ChannelKeyPath(
  path: Vector[Long])
implicit val channelKeyPathReads: Reads[ChannelKeyPath] =
  Json.reads[ChannelKeyPath]

case class LocalParams(
  nodeId: String,
  channelKeyPath: ChannelKeyPath,
  dustLimitSatoshis: Long,
  maxHtlcValueInFlightMsat: Long,
  channelReserveSatoshis: Long,
  htlcMinimumMsat: Long,
  toSelfDelay: Long,
  maxAcceptedHtlcs: Long,
  isFunder: Boolean,
  defaultFinalScriptPubKey: String,
  globalFeatures: String,
  localFeatures: String)
implicit val localParamsReads: Reads[LocalParams] =
  Json.reads[LocalParams]

case class ChannelCommitments(
  localParams: LocalParams,
  remoteParams: RemoteParams,
  channelFlags: Int,
  localCommit: LocalCommit,
  remoteCommit: RemoteCommit,
  localChanges: CommitChanges,
  remoteChanges: CommitChanges,
  localNextHtlcId: Long,
  remoteNextHtlcId: Long,
  originChannels: String, // IDK WHAT TYPE THIS SHOULD BE
  remoteNextCommitInfo: String,
  commitInput: CommitInput,
  remotePerCommitmentSecrets: Option[String], // IDK WHAT TYPE THIS SHOULD BE
  channelId: String)
implicit val channelCommitmentsReads: Reads[ChannelCommitments] =
  Json.reads[ChannelCommitments]

case class ChannelData(
  commitments: ChannelCommitments,
  shortChannelId: String,
  buried: Boolean,
  channelUpdate: ChannelUpdate)
implicit val channelDataReads: Reads[ChannelData] =
  Json.reads[ChannelData]
 */
case class ChannelResult(
    nodeId: NodeId,
    channelId: FundedChannelId,
    state: ChannelState,
    feeBaseMsat: Option[MilliSatoshis],
    feeProportionalMillionths: Option[FeeProportionalMillionths],
    data: JsObject) {
  import JsonReaders._
  lazy val shortChannelId: Option[ShortChannelId] = (data \ "shortChannelId").validate[ShortChannelId].asOpt
}

// ChannelResult ends here

case class InvoiceResult(
  prefix: LnHumanReadablePart,
  timestamp: FiniteDuration,
  nodeId: NodeId,
  serialized: String,
  description: String,
  paymentHash: Sha256Digest,
  expiry: FiniteDuration)

case class PaymentRequest(
    prefix: LnHumanReadablePart,
    amount: Option[MilliSatoshis],
    timestamp: Long,
    nodeId: NodeId,
    tags: Vector[JsObject],
    signature: LnInvoiceSignature)

case class PaymentResult(
  id: String,
  paymentHash: Sha256Digest,
  preimage: Option[PaymentPreimage],
  amountMsat: MilliSatoshis,
  createdAt: FiniteDuration,
  completedAt: Option[FiniteDuration],
  status: PaymentStatus)

case class ReceivedPaymentResult(
  paymentHash: Sha256Digest,
  amountMsat: MilliSatoshis,
  receivedAt: FiniteDuration)

sealed trait PaymentStatus
object PaymentStatus {
  case object PENDING extends PaymentStatus
  case object SUCCEEDED extends PaymentStatus
  case object FAILED extends PaymentStatus

  def apply(s: String): PaymentStatus = s match {
    case "PENDING" => PENDING
    case "SUCCEEDED" => SUCCEEDED
    case "FAILED" => FAILED
    case err => throw new IllegalArgumentException(s"Unknown payment status code `${err}`")
  }
}

case class PaymentId(value: String) {
  override def toString: String = value
}


sealed trait WebSocketEvent

object WebSocketEvent {


  case class PaymentRelayed(
      amountIn: MilliSatoshis,
      amountOut: MilliSatoshis,
      paymentHash: Sha256Digest,
      fromChannelId: FundedChannelId,
      toChannelId: FundedChannelId,
      timestamp: FiniteDuration) extends WebSocketEvent

  case class PaymentReceived(
      amount: MilliSatoshis,
      paymentHash: Sha256Digest,
      fromChannelId: FundedChannelId,
      timestamp: FiniteDuration) extends WebSocketEvent

  case class PaymentFailed(
      paymentHash: Sha256Digest,
      failures: Vector[String]) extends WebSocketEvent

  case class PaymentSent(
      amount: MilliSatoshis,
      feesPaid: MilliSatoshis,
      paymentHash: Sha256Digest,
      paymentPreimage: PaymentPreimage,
      toChannelId: FundedChannelId,
      timestamp: FiniteDuration) extends WebSocketEvent

  case class PaymentSettlingOnchain(
      amount: MilliSatoshis,
      paymentHash: Sha256Digest,
      timestamp: FiniteDuration) extends WebSocketEvent

}