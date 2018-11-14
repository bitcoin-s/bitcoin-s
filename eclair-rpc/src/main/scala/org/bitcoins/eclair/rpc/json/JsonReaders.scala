package org.bitcoins.eclair.rpc.json

import org.bitcoins.core.protocol.ln.{ LnHumanReadablePart, LnInvoice, LnInvoiceSignature }
import org.bitcoins.core.protocol.ln.channel.{ ChannelState, FundedChannelId }
import org.bitcoins.core.protocol.ln.currency.{ MilliSatoshis, PicoBitcoins }
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.eclair.rpc.network.PeerState
import org.bitcoins.rpc.serializers.SerializerUtil
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.util.{ Failure, Success }

object JsonReaders {
  import org.bitcoins.rpc.serializers.JsonReaders._
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  implicit val channelStateReads: Reads[ChannelState] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsStringOpt(ChannelState.fromString)(jsValue)
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
      SerializerUtil.processJsStringOpt(LnHumanReadablePart.fromString(_).toOption)(jsValue)
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

  implicit val nodeInfoReads: Reads[NodeInfo] = {
    Json.reads[NodeInfo]
  }

  implicit val fundedChannelIdReads: Reads[FundedChannelId] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsString(FundedChannelId.fromHex)(jsValue)
    }
  }

  implicit val channelDescReads: Reads[ChannelDesc] = {
    Json.reads[ChannelDesc]
  }

  implicit val channelInfoReads: Reads[ChannelInfo] = {
    Json.reads[ChannelInfo]
  }

  implicit val channelUpdateReads: Reads[ChannelUpdate] = {
    Json.reads[ChannelUpdate]
  }

  implicit val paymentRequestReads: Reads[PaymentRequest] = {
    Json.reads[PaymentRequest]
  }

  /*  private def parsePaymentReq(obj: JsObject): JsResult[PaymentRequest] = {
    val prefix = obj("prefix").validate[String].get
    val amountOpt = obj("amount") match {
      case o: JsObject => o("amount").validate[MilliSatoshis].asOpt
      case x @ (_: JsArray | _: JsNumber | _: JsString | _: JsBoolean | JsNull) => None
    }
    val timestamp = obj("timestamp").validate[Long].get
    val nodeId = obj("nodeId").validate[NodeId].get
    val signature = obj("signature").validate[String].get
    val tags = obj("tags").validate[Vector[JsObject]].get

    val descriptionObj: JsObject = tags.find(t => t.keys.exists(_ == "description")).get

    val description = descriptionObj("description").validate[String].get

    JsSuccess(PaymentRequest(
      prefix = prefix,
      amount = amountOpt,
      timestamp = timestamp,
      nodeId = nodeId,
      signature = signature,
      tags = tags,
      description = description))
  }*/

  implicit val paymentSucceededReads: Reads[PaymentSucceeded] = {
    Json.reads[PaymentSucceeded]
  }

  implicit val paymentFailedReads: Reads[PaymentFailed] = {
    Json.reads[PaymentFailed]
  }

  implicit val paymentResultReads: Reads[PaymentResult] = {
    Reads[PaymentResult] { jsValue =>
      val sendResult = jsValue.validate[PaymentSucceeded]
      sendResult match {
        case p: JsSuccess[PaymentSucceeded] => p
        case err1: JsError =>
          val pFailedResult = jsValue.validate[PaymentFailed]
          pFailedResult match {
            case s: JsSuccess[PaymentFailed] => s
            case err2: JsError =>
              JsError.merge(err1, err2)
          }
      }
    }
  }

  implicit val channelResultReads: Reads[ChannelResult] =
    Json.reads[ChannelResult]

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
}
