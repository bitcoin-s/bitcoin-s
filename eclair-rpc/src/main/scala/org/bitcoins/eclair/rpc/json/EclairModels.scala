package org.bitcoins.eclair.rpc.json

import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  ECDigitalSignature,
  Sha256Digest
}
import org.bitcoins.core.protocol.ln.{
  LnHumanReadablePart,
  LnInvoiceSignature,
  ShortChannelId
}
import org.bitcoins.core.protocol.ln.channel.{ChannelState, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.fee.FeeProportionalMillionths
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.eclair.rpc.network.PeerState
import play.api.libs.json.{JsArray, JsObject}

sealed abstract class EclairModels

case class GetInfoResult(
    nodeId: NodeId,
    alias: String,
    port: Int,
    chainHash: DoubleSha256Digest,
    blockHeight: Long)

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
    shortChannelId: ShortChannelId,
    addresses: Vector[String])

case class ChannelDesc(shortChannelId: ShortChannelId, a: NodeId, b: NodeId)

case class AuditResult(
    sent: Vector[SentPayment],
    relayed: Vector[RelayedPayment],
    received: Vector[ReceivedPayment]
)

case class ReceivedPayment(
    amount: MilliSatoshis,
    paymentHash: Sha256Digest,
    fromChannelId: FundedChannelId,
    timestamp: Long
)

case class RelayedPayment(
    amountIn: MilliSatoshis,
    amountOut: MilliSatoshis,
    paymentHash: Sha256Digest,
    fromChannelId: FundedChannelId,
    toChannelId: FundedChannelId,
    timestamp: Long
)

case class SentPayment(
    amount: MilliSatoshis,
    feesPaid: MilliSatoshis,
    paymentHash: Sha256Digest,
    paymentPreimage: String,
    toChannelId: FundedChannelId,
    timestamp: Long
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
    htlcMaximumMsat: MilliSatoshis,
    feeBaseMsat: MilliSatoshis,
    feeProportionalMillionths: Long)

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
    data: JsObject)

// ChannelResult ends here

case class PaymentRequest(
    prefix: LnHumanReadablePart,
    amount: Option[MilliSatoshis],
    timestamp: Long,
    nodeId: NodeId,
    tags: Vector[JsObject],
    signature: LnInvoiceSignature)

sealed abstract class PaymentResult
case class PaymentSucceeded(
    amountMsat: MilliSatoshis,
    paymentHash: Sha256Digest,
    paymentPreimage: String,
    route: JsArray)
    extends PaymentResult

case class PaymentFailed(paymentHash: Sha256Digest, failures: Vector[JsObject])
    extends PaymentResult
/*
case class PaymentFailure(???) extends SendResult
implicit val paymentFailureReads: Reads[PaymentFailure] = Json.reads[PaymentFailure]
implicit val sendResultReads: Reads[SendResult] = Reads[SendResult] { json =>
  json.validate[PaymentSucceeded] match {
    case success: JsSuccess[PaymentSucceeded] => success
    case err1: JsError => json.validate[PaymentFailure] match {
      case failure: JsSuccess[PaymentFailure] => failure
      case err2: JsError => JsError.merge(err1, err2)
    }
  }
}*/
