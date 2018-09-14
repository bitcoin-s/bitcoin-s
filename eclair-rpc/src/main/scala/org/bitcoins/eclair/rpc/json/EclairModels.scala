package org.bitcoins.eclair.rpc.json

import play.api.libs.json.{ JsArray, JsObject }

sealed abstract class EclairModels

case class GetInfoResult(
  nodeId: String,
  alias: String,
  port: Int,
  chainHash: String,
  blockHeight: Long)

case class PeerInfo(
  nodeId: String,
  state: String,
  //address: String,
  channels: Int)

case class ChannelInfo(nodeId: String, channelId: String, state: String)

case class NodeInfo(
  signature: String,
  features: String,
  timestamp: Long,
  nodeId: String,
  rgbColor: String,
  alias: String,
  addresses: Vector[String])

case class ChannelDesc(shortChannelId: String, a: String, b: String)

case class ChannelUpdate(
  signature: String,
  chainHash: String,
  shortChannelId: String,
  timestamp: Long,
  flags: String,
  cltvExpiryDelta: Int,
  htlcMinimumMsat: Long,
  feeBaseMsat: Long,
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
  nodeId: String,
  channelId: String,
  state: String,
  data: JsObject)

// ChannelResult ends here

case class PaymentRequest(
  prefix: String,
  amount: Option[Long],
  timestamp: Long,
  nodeId: String,
  tags: Vector[JsObject],
  signature: String)

sealed trait SendResult
case class PaymentSucceeded(
  amountMsat: Long,
  paymentHash: String,
  paymentPreimage: String,
  route: JsArray) extends SendResult

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
