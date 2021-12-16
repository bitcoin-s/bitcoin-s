package org.bitcoins.commons.jsonmodels.ws

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.Transaction
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
  case object BlockProcessed extends WalletWsType

  private val all =
    Vector(TxProcessed, TxBroadcast, ReservedUtxos, NewAddress, BlockProcessed)

  override def fromStringOpt(string: String): Option[WalletWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): WalletWsType = {
    fromStringOpt(string)
      .getOrElse(sys.error(s"Cannot find wallet ws type for string=$string"))
  }
}

sealed trait WsPushNotification[T] {
  def `type`: WalletWsType
  def payload: T
}

sealed trait WalletNotification[T] extends WsPushNotification[T] {
  override def `type`: WalletWsType
}

object WalletNotification {

  case class NewAddressNotification(payload: BitcoinAddress)
      extends WalletNotification[BitcoinAddress] {
    override val `type`: WalletWsType = WalletWsType.NewAddress
  }

  case class TxProcessedNotification(payload: Transaction)
      extends WalletNotification[Transaction] {
    override val `type`: WalletWsType = WalletWsType.TxProcessed
  }

  case class TxBroadcastNotification(payload: Transaction)
      extends WalletNotification[Transaction] {
    override val `type`: WalletWsType = WalletWsType.TxBroadcast
  }

  case class ReservedUtxosNotification(payload: Vector[SpendingInfoDb])
      extends WalletNotification[Vector[SpendingInfoDb]] {
    override val `type`: WalletWsType = WalletWsType.ReservedUtxos
  }

  case class BlockProcessedNotification(payload: GetBlockHeaderResult)
      extends WalletNotification[GetBlockHeaderResult] {
    override val `type`: WalletWsType = WalletWsType.BlockProcessed
  }
}
