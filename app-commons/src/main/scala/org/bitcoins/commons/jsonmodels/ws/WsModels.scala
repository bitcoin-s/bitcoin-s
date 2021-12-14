package org.bitcoins.commons.jsonmodels.ws

import org.bitcoins.crypto.StringFactory

sealed trait WsType

object WsType extends StringFactory[WsType] {

  override def fromString(string: String): WsType = {
    WalletWsType.fromString(string)
  }
}

sealed trait WalletWsType extends WsType

object WalletWsType extends StringFactory[WalletWsType] {
  case object TxProcessed extends WalletWsType
  case object TxBroadcast extends WalletWsType
  case object ReservedUtxos extends WalletWsType
  case object NewAddress extends WalletWsType

  private val all = Vector(TxProcessed, TxBroadcast, ReservedUtxos, NewAddress)

  override def fromStringOpt(string: String): Option[WalletWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): WalletWsType = {
    fromStringOpt(string)
      .getOrElse(sys.error(s"Cannot find wallet ws type for string=$string"))
  }
}

sealed trait WsPushNotification[T] {
  def `type`: WsType
  def payload: T
}

case class WalletNotification(`type`: WalletWsType, payload: ujson.Value)
    extends WsPushNotification[ujson.Value]
