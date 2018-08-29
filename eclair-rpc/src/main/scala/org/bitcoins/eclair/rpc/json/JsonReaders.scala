package org.bitcoins.eclair.rpc.json

import play.api.libs.json.{ Json, Reads }

object JsonReaders {

  implicit val getInfoResultReads: Reads[GetInfoResult] = {
    Json.reads[GetInfoResult]
  }

  implicit val peerInfoReads: Reads[PeerInfo] = {
    Json.reads[PeerInfo]
  }

  implicit val nodeInfoReads: Reads[NodeInfo] = {
    Json.reads[NodeInfo]
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
