package org.bitcoins.eclair.rpc.client

import java.util.UUID

import org.bitcoins.core.crypto.Sha256Digest
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.channel.{ChannelState, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.{MilliSatoshis, PicoBitcoins}
import org.bitcoins.core.protocol.ln.fee.FeeProportionalMillionths
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.eclair.rpc.api.{
  AuditResult,
  BaseChannelInfo,
  ChannelDesc,
  ChannelInfo,
  ChannelResult,
  ChannelStats,
  ChannelUpdate,
  GetInfoResult,
  Hop,
  IncomingPayment,
  IncomingPaymentStatus,
  InvoiceResult,
  NetworkFeesResult,
  NodeInfo,
  OpenChannelInfo,
  OutgoingPayment,
  OutgoingPaymentStatus,
  PaymentFailure,
  PaymentId,
  PaymentRequest,
  PeerInfo,
  ReceivedPayment,
  RelayedPayment,
  SentPayment,
  UsableBalancesResult,
  WebSocketEvent
}
import org.bitcoins.eclair.rpc.network.PeerState
import org.bitcoins.rpc.serializers.SerializerUtil
import play.api.libs.json._

import scala.concurrent.duration._
import scala.util.{Failure, Success}

object JsonReaders {
  import org.bitcoins.rpc.serializers.JsonReaders._

  implicit val feeProportionalMillionthsReads: Reads[
    FeeProportionalMillionths] = Reads { js =>
    SerializerUtil.processJsNumberBigInt(FeeProportionalMillionths.fromBigInt)(
      js)
  }

  implicit val channelStateReads: Reads[ChannelState] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsStringOpt(ChannelState.fromString)(jsValue)
    }
  }

  implicit val normalChannelStateReads: Reads[ChannelState.NORMAL.type] =
    Reads { jsValue =>
      jsValue
        .validate[ChannelState]
        .flatMap {
          case ChannelState.NORMAL => JsSuccess(ChannelState.NORMAL)
          case state: ChannelState =>
            JsError(s"$state is not ChannelState.NORMAL")
        }
    }

  implicit val peerStateReads: Reads[PeerState] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsStringOpt(PeerState.fromString)(jsValue)
    }
  }

  implicit val picoBitcoinsReads: Reads[PicoBitcoins] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsNumberBigInt(PicoBitcoins.apply)(jsValue)
    }
  }

  implicit val msatReads: Reads[MilliSatoshis] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsNumberBigInt(MilliSatoshis.apply)(jsValue)

    }
  }

  implicit val nodeIdReads: Reads[NodeId] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsString(NodeId.fromHex)(jsValue)
    }
  }

  implicit val lnHrpReads: Reads[LnHumanReadablePart] = {
    Reads { jsValue =>
      SerializerUtil.processJsStringOpt(
        LnHumanReadablePart.fromString(_).toOption)(jsValue)
    }
  }

  implicit val lnInvoiceSignatureReads: Reads[LnInvoiceSignature] = {
    Reads { jsValue =>
      SerializerUtil.processJsString(LnInvoiceSignature.fromHex)(jsValue)
    }
  }

  implicit val getInfoResultReads: Reads[GetInfoResult] = {
    Json.reads[GetInfoResult]
  }

  implicit val peerInfoReads: Reads[PeerInfo] = {
    Json.reads[PeerInfo]
  }

  implicit val shortChannelIdReads: Reads[ShortChannelId] = {
    Reads { jsValue =>
      SerializerUtil.processJsString(ShortChannelId.fromHumanReadableString)(
        jsValue)
    }
  }

  implicit val nodeInfoReads: Reads[NodeInfo] = {
    Json.reads[NodeInfo]
  }

  implicit val paymentPreimageReads: Reads[PaymentPreimage] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsString(PaymentPreimage.fromHex)(jsValue)
    }
  }

  implicit val fundedChannelIdReads: Reads[FundedChannelId] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsString(FundedChannelId.fromHex)(jsValue)
    }
  }

  implicit val channelDescReads: Reads[ChannelDesc] = {
    Json.reads[ChannelDesc]
  }

  implicit val createInvoiceResultReads: Reads[InvoiceResult] = {
    Reads { jsValue =>
      for {
        prefix <- (jsValue \ "prefix").validate[LnHumanReadablePart]
        timestamp <- (jsValue \ "timestamp").validate[Long]
        nodeId <- (jsValue \ "nodeId").validate[NodeId]
        serialized <- (jsValue \ "serialized").validate[String]
        description <- (jsValue \ "description").validate[String]
        paymentHash <- (jsValue \ "paymentHash").validate[Sha256Digest]
        expiry <- (jsValue \ "expiry").validate[Long]
      } yield InvoiceResult(prefix,
                            timestamp.seconds,
                            nodeId,
                            serialized,
                            description,
                            paymentHash,
                            expiry.seconds)
    }
  }

  implicit val openChannelInfoReads: Reads[OpenChannelInfo] = Reads { jsValue =>
    for {
      nodeId <- (jsValue \ "nodeId").validate[NodeId]
      shortChannelId <- (jsValue \ "data" \ "shortChannelId")
        .validate[ShortChannelId]
      channelId <- (jsValue \ "channelId").validate[FundedChannelId]
      state <- (jsValue \ "state").validate[ChannelState.NORMAL.type]
      remoteMsat <- (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toRemote")
        .validate[MilliSatoshis]
      localMsat <- (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toLocal")
        .validate[MilliSatoshis]

    } yield OpenChannelInfo(nodeId = nodeId,
                            shortChannelId = shortChannelId,
                            channelId = channelId,
                            localMsat = localMsat,
                            remoteMsat = remoteMsat,
                            state = state)
  }

  implicit val baseChannelInfoReads: Reads[BaseChannelInfo] = Reads { jsValue =>
    for {
      nodeId <- (jsValue \ "nodeId").validate[NodeId]
      channelId <- (jsValue \ "channelId").validate[FundedChannelId]
      state <- (jsValue \ "state").validate[ChannelState]
      remoteMsat <- (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toRemote")
        .validate[MilliSatoshis]
      localMsat <- (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toLocal")
        .validate[MilliSatoshis]

    } yield BaseChannelInfo(nodeId = nodeId,
                            channelId = channelId,
                            localMsat = localMsat,
                            remoteMsat = remoteMsat,
                            state = state)
  }

  implicit val channelInfoReads: Reads[ChannelInfo] = Reads { jsValue =>
    (jsValue \ "state")
      .validate[ChannelState]
      .flatMap {
        case ChannelState.NORMAL =>
          jsValue.validate[OpenChannelInfo]
        case _: ChannelState =>
          jsValue.validate[BaseChannelInfo]
      }
  }

  implicit val channelUpdateReads: Reads[ChannelUpdate] = {
    Json.reads[ChannelUpdate]
  }

  implicit val paymentIdReads: Reads[PaymentId] = Reads { jsValue =>
    SerializerUtil.processJsString(s => PaymentId(UUID.fromString(s)))(jsValue)
  }

  implicit val finiteDurationReads: Reads[FiniteDuration] =
    Reads { js =>
      SerializerUtil.processJsNumberBigInt(_.longValue.millis)(js)
    }

  implicit val paymentReceivedReads: Reads[IncomingPaymentStatus.Received] =
    Json.reads[IncomingPaymentStatus.Received]
  implicit val hopReads: Reads[Hop] =
    Json.reads[Hop]
  implicit val paymentSentReads: Reads[OutgoingPaymentStatus.Succeeded] =
    Json.reads[OutgoingPaymentStatus.Succeeded]
  implicit val paymentFailureTypeReads: Reads[PaymentFailure.Type] = Reads {
    jsValue =>
      (jsValue \ "type")
        .validate[String]
        .flatMap { s =>
          s.toLowerCase match {
            case "local"  => JsSuccess(PaymentFailure.Local)
            case "remote" => JsSuccess(PaymentFailure.Remote)
            case "unreadableremote" =>
              JsSuccess(PaymentFailure.UnreadableRemote)
            case _ =>
              throw new RuntimeException(s"Unknown payment failure type `$s`")
          }
        }
  }
  implicit val paymentFailureReads: Reads[PaymentFailure] =
    Json.reads[PaymentFailure]
  implicit val paymentFailedReads: Reads[OutgoingPaymentStatus.Failed] =
    Json.reads[OutgoingPaymentStatus.Failed]

  implicit val outgoingPaymentStatusReads: Reads[OutgoingPaymentStatus] =
    Reads { jsValue =>
      (jsValue \ "type")
        .validate[String]
        .flatMap {
          case "pending" => JsSuccess(OutgoingPaymentStatus.Pending)
          case "sent"    => jsValue.validate[OutgoingPaymentStatus.Succeeded]
          case "failed"  => jsValue.validate[OutgoingPaymentStatus.Failed]
        }
    }

  implicit val incomingPaymentStatusReads: Reads[IncomingPaymentStatus] =
    Reads { jsValue =>
      (jsValue \ "type")
        .validate[String]
        .flatMap {
          case "pending"  => JsSuccess(IncomingPaymentStatus.Pending)
          case "expired"  => JsSuccess(IncomingPaymentStatus.Expired)
          case "received" => jsValue.validate[IncomingPaymentStatus.Received]
        }
    }

  implicit val paymentRequestReads: Reads[PaymentRequest] = {
    Json.reads[PaymentRequest]
  }

  implicit val paymentSucceededReads: Reads[OutgoingPayment] =
    Json.reads[OutgoingPayment]

  implicit val receivedPaymentResultReads: Reads[IncomingPayment] =
    Json.reads[IncomingPayment]

  implicit val channelResultReads: Reads[ChannelResult] = Reads { js =>
    for {
      nodeId <- (js \ "nodeId").validate[NodeId]
      channelId <- (js \ "channelId").validate[FundedChannelId]
      state <- (js \ "state").validate[ChannelState]
      feeBaseMsat <- (js \ "data" \ "channelUpdate" \ "feeBaseMsat")
        .validateOpt[MilliSatoshis]
      feeProportional <- (js \ "data" \ "channelUpdate" \ "feeProportionalMillionths")
        .validateOpt[FeeProportionalMillionths]
      data <- (js \ "data").validate[JsObject]
    } yield ChannelResult(nodeId = nodeId,
                          state = state,
                          channelId = channelId,
                          feeBaseMsat = feeBaseMsat,
                          feeProportionalMillionths = feeProportional,
                          data = data)
  }

  implicit val lnInvoiceReads: Reads[LnInvoice] =
    Reads[LnInvoice] {
      case JsString(invoice) =>
        LnInvoice.fromString(invoice) match {
          case Success(paymentRequest) => JsSuccess(paymentRequest)
          case Failure(err) =>
            JsError(s"Invalid refund invoice: ${err.toString}")
        }
      case bad @ (_: JsNumber | _: JsObject | _: JsArray | _: JsBoolean |
          JsNull) =>
        JsError(s"Invalid type on refund invoice: $bad, expected JsString")
    }

  implicit val receivedPaymentPartReads: Reads[ReceivedPayment.Part] =
    Json.reads[ReceivedPayment.Part]
  implicit val receivedPaymentReads: Reads[ReceivedPayment] =
    Json.reads[ReceivedPayment]
  implicit val sentPaymentPartReads: Reads[SentPayment.Part] =
    Json.reads[SentPayment.Part]
  implicit val sentPaymentReads: Reads[SentPayment] = Json.reads[SentPayment]
  implicit val relayedPaymentReads: Reads[RelayedPayment] =
    Json.reads[RelayedPayment]
  implicit val auditResultReads: Reads[AuditResult] = Json.reads[AuditResult]

  implicit val networkFeesResultReads: Reads[NetworkFeesResult] =
    Json.reads[NetworkFeesResult]

  implicit val channelStatsReads: Reads[ChannelStats] =
    Json.reads[ChannelStats]

  implicit val usableBalancesResultReads: Reads[UsableBalancesResult] =
    Json.reads[UsableBalancesResult]

  implicit val paymentRelayedEventReads: Reads[WebSocketEvent.PaymentRelayed] =
    Json.reads[WebSocketEvent.PaymentRelayed]

  implicit val paymentReceivedEventReads: Reads[
    WebSocketEvent.PaymentReceived] =
    Json.reads[WebSocketEvent.PaymentReceived]

  implicit val paymentFailedEventReads: Reads[WebSocketEvent.PaymentFailed] =
    Json.reads[WebSocketEvent.PaymentFailed]

  implicit val paymentSentEventReads: Reads[WebSocketEvent.PaymentSent] =
    Json.reads[WebSocketEvent.PaymentSent]

  implicit val paymentSettlingOnchainEventReads: Reads[
    WebSocketEvent.PaymentSettlingOnchain] =
    Json.reads[WebSocketEvent.PaymentSettlingOnchain]

}
