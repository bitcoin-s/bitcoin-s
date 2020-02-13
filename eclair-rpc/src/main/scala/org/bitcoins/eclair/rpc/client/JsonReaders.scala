package org.bitcoins.eclair.rpc.client

import java.net.InetSocketAddress
import java.time.Instant
import java.util.UUID

import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECDigitalSignature,
  Sha256Digest
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.channel.{ChannelState, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.{MilliSatoshis, PicoBitcoins}
import org.bitcoins.core.protocol.ln.fee.FeeProportionalMillionths
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.eclair.rpc.api._
import org.bitcoins.eclair.rpc.network.PeerState
import org.bitcoins.rpc.serializers.SerializerUtil
import play.api.libs.json._

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

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

  implicit val inetSocketAddressReads: Reads[InetSocketAddress] = {
    Reads { jsValue =>
      SerializerUtil.processJsString({ addr =>
        addr.split(":") match {
          case Array(host, portStr) =>
            val port = Try(portStr.toInt).getOrElse(
              throw new RuntimeException(s"Invalid port number `$portStr`"))
            InetSocketAddress.createUnresolved(host, port.toInt)
          case _ => throw new RuntimeException(s"Invalid inet address `$addr`")
        }
      })(jsValue)
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
    Reads { jsValue =>
      for {
        signature <- (jsValue \ "signature").validate[ECDigitalSignature]
        features <- (jsValue \ "features").validate[String]
        timestamp <- (jsValue \ "timestamp")
          .validate[Instant](instantReadsSeconds)
        nodeId <- (jsValue \ "nodeId").validate[NodeId]
        rgbColor <- (jsValue \ "rgbColor").validate[String]
        alias <- (jsValue \ "alias").validate[String]
        addresses <- (jsValue \ "addresses").validate[Vector[InetSocketAddress]]
      } yield NodeInfo(signature,
                       features,
                       timestamp,
                       nodeId,
                       rgbColor,
                       alias,
                       addresses)
    }
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
        timestamp <- (jsValue \ "timestamp")
          .validate[Instant](instantReadsSeconds)
        nodeId <- (jsValue \ "nodeId").validate[NodeId]
        serialized <- (jsValue \ "serialized").validate[String]
        description <- (jsValue \ "description").validate[String]
        paymentHash <- (jsValue \ "paymentHash").validate[Sha256Digest]
        expiry <- (jsValue \ "expiry").validate[Long]
      } yield InvoiceResult(prefix,
                            timestamp,
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
    Reads { jsValue =>
      for {
        signature <- (jsValue \ "signature").validate[ECDigitalSignature]
        chainHash <- (jsValue \ "chainHash").validate[DoubleSha256Digest]
        shortChannelId <- (jsValue \ "shortChannelId").validate[ShortChannelId]
        timestamp <- (jsValue \ "timestamp")
          .validate[Instant](instantReadsSeconds)
        messageFlags <- (jsValue \ "messageFlags").validate[Int]
        channelFlags <- (jsValue \ "channelFlags").validate[Int]
        cltvExpiryDelta <- (jsValue \ "cltvExpiryDelta").validate[Int]
        htlcMinimumMsat <- (jsValue \ "htlcMinimumMsat").validate[MilliSatoshis]
        feeProportionalMillionths <- (jsValue \ "feeProportionalMillionths")
          .validate[FeeProportionalMillionths]
        htlcMaximumMsat <- (jsValue \ "htlcMaximumMsat")
          .validateOpt[MilliSatoshis]
        feeBaseMsat <- (jsValue \ "feeBaseMsat").validate[MilliSatoshis]
      } yield ChannelUpdate(
        signature,
        chainHash,
        shortChannelId,
        timestamp,
        messageFlags,
        channelFlags,
        cltvExpiryDelta,
        htlcMinimumMsat,
        feeProportionalMillionths,
        htlcMaximumMsat,
        feeBaseMsat
      )
    }
  }

  implicit val paymentIdReads: Reads[PaymentId] = Reads { jsValue =>
    SerializerUtil.processJsString(s => PaymentId(UUID.fromString(s)))(jsValue)
  }

  implicit val sendToRouteResultReads: Reads[SendToRouteResult] =
    Json.reads[SendToRouteResult]

  //don't make this implicit so we don't accidentally read the wrong time unit
  val finiteDurationReadsMilliseconds: Reads[FiniteDuration] =
    Reads { js =>
      SerializerUtil.processJsNumberBigInt(_.longValue.millis)(js)
    }

  //don't make this implicit so we don't accidentally read the wrong time unit
  val finiteDurationReadsSeconds: Reads[FiniteDuration] = Reads { js =>
    SerializerUtil.processJsNumberBigInt(_.longValue.seconds)(js)
  }

  val instantReadsMilliseconds: Reads[Instant] =
    Reads { js =>
      SerializerUtil.processJsNumberBigInt(x =>
        Instant.ofEpochMilli(x.longValue))(js)
    }

  val instantReadsSeconds: Reads[Instant] = Reads { js =>
    SerializerUtil.processJsNumberBigInt(x =>
      Instant.ofEpochSecond(x.longValue))(js)
  }

  implicit val paymentTypeReads: Reads[PaymentType] = Reads { jsValue =>
    SerializerUtil.processJsString(PaymentType.fromString)(jsValue)
  }

  implicit val paymentReceivedReads: Reads[IncomingPaymentStatus.Received] =
    Json.reads[IncomingPaymentStatus.Received]
  implicit val hopReads: Reads[Hop] =
    Json.reads[Hop]
  implicit val paymentSentReads: Reads[OutgoingPaymentStatus.Succeeded] =
    Reads { js =>
      for {
        preimage <- (js \ "paymentPreimage").validate[PaymentPreimage]
        feesPaid <- (js \ "feesPaid").validate[MilliSatoshis]
        route <- (js \ "route").validate[Seq[Hop]]
        completed <- (js \ "completedAt")
          .validate[Instant](instantReadsMilliseconds)
      } yield OutgoingPaymentStatus.Succeeded(paymentPreimage = preimage,
                                              feesPaid = feesPaid,
                                              route = route,
                                              completedAt = completed)
    }

  implicit val paymentFailureTypeReads: Reads[PaymentFailure.Type] = Reads {
    jsValue =>
      (jsValue \ "name")
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

  implicit val paymentRequestReads: Reads[PaymentRequest] = Reads { js =>
    for {
      prefix <- (js \ "prefix").validate[LnHumanReadablePart]
      timestamp <- (js \ "timestamp")
        .validate[Instant](instantReadsSeconds)
      nodeId <- (js \ "nodeId").validate[NodeId]
      serialized <- (js \ "serialized").validate[String]
      description <- (js \ "serialized").validate[String]
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      expiry <- (js \ "expiry")
        .validate[FiniteDuration](finiteDurationReadsSeconds)
      amount <- (js \ "amount").validateOpt[MilliSatoshis]
    } yield PaymentRequest(prefix,
                           timestamp,
                           nodeId,
                           serialized,
                           description,
                           paymentHash,
                           expiry,
                           amount)
  }

  implicit val paymentSucceededReads: Reads[OutgoingPayment] = Reads { js =>
    for {
      id <- (js \ "id").validate[PaymentId]
      parentId <- (js \ "parentId").validate[PaymentId]
      externalId <- (js \ "externalId").validateOpt[String]
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      paymentType <- (js \ "paymentType").validate[PaymentType]
      amount <- (js \ "amount").validate[MilliSatoshis]
      recipientAmount <- (js \ "recipientAmount").validate[MilliSatoshis]
      recipientNodeId <- (js \ "recipientNodeId").validate[NodeId]
      createdAt <- (js \ "createdAt")
        .validate[Instant](instantReadsMilliseconds)
      paymentRequest <- (js \ "paymentRequest").validateOpt[PaymentRequest]
      status <- (js \ "status").validate[OutgoingPaymentStatus]
    } yield OutgoingPayment(id,
                            parentId,
                            externalId,
                            paymentHash,
                            paymentType,
                            amount,
                            recipientAmount,
                            recipientNodeId,
                            createdAt,
                            paymentRequest,
                            status)
  }

  implicit val receivedPaymentResultReads: Reads[IncomingPayment] = Reads {
    js =>
      for {
        paymentRequest <- (js \ "paymentRequest").validate[PaymentRequest]
        paymentPreimage <- (js \ "paymentPreimage").validate[PaymentPreimage]
        createdAt <- (js \ "createdAt")
          .validate[Instant](instantReadsMilliseconds)
        status <- (js \ "status").validate[IncomingPaymentStatus]
      } yield IncomingPayment(paymentRequest,
                              paymentPreimage,
                              createdAt,
                              status)
  }

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

  implicit val receivedPaymentPartReads: Reads[ReceivedPayment.Part] = Reads {
    js =>
      for {
        amount <- (js \ "amount").validate[MilliSatoshis]
        fromChannelId <- (js \ "fromChannelId").validate[FundedChannelId]
        timestamp <- (js \ "timestamp")
          .validate[Instant](instantReadsMilliseconds)
      } yield ReceivedPayment.Part(amount, fromChannelId, timestamp)
  }

  implicit val receivedPaymentReads: Reads[ReceivedPayment] =
    Json.reads[ReceivedPayment]

  implicit val sentPaymentPartReads: Reads[SentPayment.Part] = Reads { js =>
    for {
      id <- (js \ "id").validate[PaymentId]
      amount <- (js \ "amount").validate[MilliSatoshis]
      feesPaid <- (js \ "feesPaid").validate[MilliSatoshis]
      toChannelId <- (js \ "toChannelId").validate[FundedChannelId]
      timestamp <- (js \ "timestamp")
        .validate[Instant](instantReadsMilliseconds)
    } yield SentPayment.Part(id, amount, feesPaid, toChannelId, timestamp)
  }

  implicit val sentPaymentReads: Reads[SentPayment] = Json.reads[SentPayment]

  implicit val relayedPaymentReads: Reads[RelayedPayment] = Reads { js =>
    for {
      amountIn <- (js \ "amountIn").validate[MilliSatoshis]
      amountOut <- (js \ "amountOut").validate[MilliSatoshis]
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      fromChannelId <- (js \ "fromChannelId").validate[FundedChannelId]
      toChannelId <- (js \ "toChannelId").validate[FundedChannelId]
      timestamp <- (js \ "timestamp")
        .validate[Instant](instantReadsMilliseconds)
    } yield RelayedPayment(amountIn,
                           amountOut,
                           paymentHash,
                           fromChannelId,
                           toChannelId,
                           timestamp)
  }
  implicit val auditResultReads: Reads[AuditResult] = Json.reads[AuditResult]

  implicit val networkFeesResultReads: Reads[NetworkFeesResult] = Reads { js =>
    for {
      remoteNodeId <- (js \ "remoteNodeId").validate[NodeId]
      channelId <- (js \ "channelId").validate[FundedChannelId]
      txId <- (js \ "txId").validate[DoubleSha256DigestBE]
      fee <- (js \ "fee").validate[Satoshis]
      txType <- (js \ "txType").validate[String]
      timestamp <- (js \ "timestamp")
        .validate[Instant](instantReadsMilliseconds)
    } yield NetworkFeesResult(remoteNodeId,
                              channelId,
                              txId,
                              fee,
                              txType,
                              timestamp)
  }

  implicit val channelStatsReads: Reads[ChannelStats] =
    Json.reads[ChannelStats]

  implicit val usableBalancesResultReads: Reads[UsableBalancesResult] =
    Json.reads[UsableBalancesResult]

  implicit val paymentRelayedEventReads: Reads[WebSocketEvent.PaymentRelayed] =
    Reads { js =>
      for {
        amountIn <- (js \ "amountIn").validate[MilliSatoshis]
        amountOut <- (js \ "amountOut").validate[MilliSatoshis]
        paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
        fromChannelId <- (js \ "fromChannelId").validate[FundedChannelId]
        toChannelId <- (js \ "toChannelId").validate[FundedChannelId]
        timestamp <- (js \ "timestamp")
          .validate[Instant](instantReadsMilliseconds)
      } yield WebSocketEvent.PaymentRelayed(amountIn,
                                            amountOut,
                                            paymentHash,
                                            fromChannelId,
                                            toChannelId,
                                            timestamp)
    }

  implicit val paymentReceivedEventPartReads: Reads[
    WebSocketEvent.PaymentReceived.Part] = Reads { js =>
    for {
      amount <- (js \ "amount").validate[MilliSatoshis]
      fromChannelId <- (js \ "fromChannelId").validate[FundedChannelId]
      timestamp <- (js \ "timestamp")
        .validate[Instant](instantReadsMilliseconds)
    } yield WebSocketEvent.PaymentReceived.Part(amount,
                                                fromChannelId,
                                                timestamp)
  }

  implicit val paymentReceivedEventReads: Reads[
    WebSocketEvent.PaymentReceived] = Reads { js =>
    for {
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      parts <- (js \ "parts")
        .validate[Vector[WebSocketEvent.PaymentReceived.Part]]
    } yield WebSocketEvent.PaymentReceived(paymentHash, parts)
  }

  implicit val paymentFailedEventReads: Reads[WebSocketEvent.PaymentFailed] =
    Reads { js =>
      for {
        id <- (js \ "id").validate[PaymentId]
        paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
        failures <- (js \ "failures").validate[Vector[String]]
        timestamp <- (js \ "timestamp")
          .validate[Instant](instantReadsMilliseconds)
      } yield WebSocketEvent.PaymentFailed(id, paymentHash, failures, timestamp)
    }

  implicit val paymentSentEventPartReads: Reads[
    WebSocketEvent.PaymentSent.Part] = Reads { js =>
    for {
      id <- (js \ "id").validate[PaymentId]
      amount <- (js \ "amount").validate[MilliSatoshis]
      feesPaid <- (js \ "feesPaid").validate[MilliSatoshis]
      toChannelId <- (js \ "toChannelId").validate[FundedChannelId]
      timestamp <- (js \ "timestamp")
        .validate[Instant](instantReadsMilliseconds)
    } yield WebSocketEvent.PaymentSent.Part(id,
                                            amount,
                                            feesPaid,
                                            toChannelId,
                                            timestamp)
  }

  implicit val paymentSentEventReads: Reads[WebSocketEvent.PaymentSent] =
    Reads { js =>
      for {
        id <- (js \ "id").validate[PaymentId]
        paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
        paymentPreimage <- (js \ "paymentPreimage").validate[PaymentPreimage]
        parts <- (js \ "parts")
          .validate[Vector[WebSocketEvent.PaymentSent.Part]]
      } yield WebSocketEvent.PaymentSent(id,
                                         paymentHash,
                                         paymentPreimage,
                                         parts)
    }

  implicit val paymentSettlingOnchainEventReads: Reads[
    WebSocketEvent.PaymentSettlingOnchain] = Reads { js =>
    for {
      amount <- (js \ "amount").validate[MilliSatoshis]
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      timestamp <- (js \ "timestamp")
        .validate[Instant](instantReadsMilliseconds)
    } yield WebSocketEvent.PaymentSettlingOnchain(amount,
                                                  paymentHash,
                                                  timestamp)
  }

  implicit val webSocketEventReads: Reads[WebSocketEvent] =
    Reads { js =>
      (js \ "type")
        .validate[String]
        .flatMap {
          case "payment-relayed"  => js.validate[WebSocketEvent.PaymentRelayed]
          case "payment-received" => js.validate[WebSocketEvent.PaymentReceived]
          case "payment-failed" =>
            js.validate[WebSocketEvent.PaymentFailed]
          case "payment-sent" =>
            js.validate[WebSocketEvent.PaymentSent]
          case "payment-settling-onchain" =>
            js.validate[WebSocketEvent.PaymentSettlingOnchain]
        }
    }

}
