package org.bitcoins.eclair.rpc.json

import org.bitcoins.core.crypto.{ DoubleSha256Digest, ECDigitalSignature }
import org.bitcoins.core.protocol.ln.PicoBitcoins
import org.bitcoins.core.protocol.ln.channel.FundedChannelId
import org.bitcoins.eclair.rpc.network.{ NodeId, PeerState }
import org.bitcoins.rpc.serializers.SerializerUtil
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.util.Try

object JsonReaders {

  import org.bitcoins.rpc.serializers.JsonReaders._

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  implicit val picoBitcoins: Reads[PicoBitcoins] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsNumberBigInt(PicoBitcoins.apply)(jsValue)
    }
  }

  implicit val peerState: Reads[PeerState] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsStringOpt(PeerState.fromString)(jsValue)
    }
  }

  implicit val nodeId: Reads[NodeId] = {
    Reads { jsValue: JsValue =>
      SerializerUtil.processJsString(NodeId.fromHex)(jsValue)
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

  implicit val paymentSucceededReads: Reads[PaymentSucceeded] = {
    Json.reads[PaymentSucceeded]
  }

  implicit val sendResultReads: Reads[SendResult] = {
    Reads[SendResult](_.validate[PaymentSucceeded])
  }

  implicit val channelResultReads: Reads[ChannelResult] =
    Json.reads[ChannelResult]
}
