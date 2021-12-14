package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.ws.{WalletNotification, WalletWsType}
import org.bitcoins.core.serializers.PicklerKeys
import upickle.default._

object WsPicklers {

  implicit val walletWsTypePickler: ReadWriter[WalletWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => WalletWsType.fromString(str.str))
  }

  private def writeWalletNotification(
      notification: WalletNotification): ujson.Obj = {
    val payloadObj = ujson.Obj(
      PicklerKeys.typeKey -> writeJs(notification.`type`),
      PicklerKeys.payloadKey -> notification.payload)

    payloadObj
  }

  private def readWalletNotification(obj: ujson.Obj): WalletNotification = {
    val typeObj = read[WalletWsType](obj(PicklerKeys.typeKey))
    val payloadObj = obj(PicklerKeys.payloadKey).obj
    WalletNotification(typeObj, payloadObj)
  }

  implicit val walletNotificationPickler: ReadWriter[WalletNotification] = {
    readwriter[ujson.Obj].bimap(writeWalletNotification, readWalletNotification)
  }
}
