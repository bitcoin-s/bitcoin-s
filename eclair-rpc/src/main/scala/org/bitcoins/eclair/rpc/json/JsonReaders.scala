package org.bitcoins.eclair.rpc.json

import org.bitcoins.core.crypto.Sha256Digest
import org.bitcoins.core.protocol.ln.channel.{ChannelState, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.{MilliSatoshis, PicoBitcoins}
import org.bitcoins.core.protocol.ln.fee.FeeProportionalMillionths
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.ln._
import org.bitcoins.eclair.rpc.network.PeerState
import org.bitcoins.rpc.serializers.SerializerUtil
import play.api.libs.json._

import scala.concurrent.duration._
import scala.util.{Failure, Success}

object JsonReaders {
  import org.bitcoins.rpc.serializers.JsonReaders._

  implicit val feeProportionalMillionthsReads: Reads[FeeProportionalMillionths] = Reads { js =>
    SerializerUtil.processJsNumberBigInt(FeeProportionalMillionths.fromBigInt)(js)
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
      SerializerUtil.processJsString(ShortChannelId.fromHumanReadableString)(jsValue)
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
      } yield
        InvoiceResult(
          prefix,
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
      remoteMsat <- (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toLocalMsat")
        .validate[MilliSatoshis]
      localMsat <- (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toLocalMsat")
        .validate[MilliSatoshis]

    } yield
      OpenChannelInfo(nodeId = nodeId,
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
      remoteMsat <- (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toRemoteMsat")
        .validate[MilliSatoshis]
      localMsat <- (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toLocalMsat")
        .validate[MilliSatoshis]

    } yield
      BaseChannelInfo(nodeId = nodeId,
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

  implicit val paymentRequestReads: Reads[PaymentRequest] = {
    Json.reads[PaymentRequest]
  }

  implicit val paymentIdReads: Reads[PaymentId] = Reads { jsValue =>
    SerializerUtil.processJsString(PaymentId.apply)(jsValue)
  }

  implicit val paymentStatusReads: Reads[PaymentStatus] = Reads { jsValue =>
    SerializerUtil.processJsString(PaymentStatus.apply)(jsValue)
  }

  implicit val finiteDurationReads: Reads[FiniteDuration] =
    Reads { js =>
      SerializerUtil.processJsNumberBigInt(_.longValue.millis)(js)
    }

  implicit val paymentSucceededReads: Reads[PaymentResult] =
    Json.reads[PaymentResult]

  implicit val receivedPaymentResultReads: Reads[ReceivedPaymentResult] =
    Json.reads[ReceivedPaymentResult]

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
    } yield
      ChannelResult(nodeId = nodeId,
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

  implicit val receivedPaymentReads: Reads[ReceivedPayment] =
    Json.reads[ReceivedPayment]
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

  import WebSocketEvent._

  implicit val paymentRelayedEventReads: Reads[PaymentRelayed] =
    Json.reads[PaymentRelayed]

  implicit val paymentReceivedEventReads: Reads[PaymentReceived] =
    Json.reads[PaymentReceived]

  implicit val paymentFailedEventReads: Reads[PaymentFailed] =
    Json.reads[PaymentFailed]

  implicit val paymentSentEventReads: Reads[PaymentSent] =
    Json.reads[PaymentSent]

  implicit val paymentSettlingOnchainEventReads: Reads[PaymentSettlingOnchain] =
    Json.reads[PaymentSettlingOnchain]

}
