package org.bitcoins.commons.jsonmodels.ws

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.StringFactory

/** The event type being sent over the websocket. An example is [[WalletWsType.BlockProcessed]] */
sealed trait WsType

object WsType extends StringFactory[WsType] {

  override def fromString(string: String): WsType = {
    ChainWsType.fromStringOpt(string) match {
      case Some(t) => t
      case None =>
        WalletWsType.fromString(string)
    }
  }
}

sealed trait WalletWsType extends WsType
sealed trait ChainWsType extends WsType

object WalletWsType extends StringFactory[WalletWsType] {
  case object TxProcessed extends WalletWsType
  case object TxBroadcast extends WalletWsType
  case object ReservedUtxos extends WalletWsType
  case object NewAddress extends WalletWsType

  case object DLCStateChange extends WalletWsType

  private val all =
    Vector(TxProcessed, TxBroadcast, ReservedUtxos, NewAddress)

  override def fromStringOpt(string: String): Option[WalletWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): WalletWsType = {
    fromStringOpt(string)
      .getOrElse(sys.error(s"Cannot find wallet ws type for string=$string"))
  }
}

object ChainWsType extends StringFactory[ChainWsType] {
  case object BlockProcessed extends ChainWsType

  private val all: Vector[ChainWsType] = Vector(BlockProcessed)

  override def fromStringOpt(string: String): Option[ChainWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): ChainWsType = {
    fromStringOpt(string)
      .getOrElse(sys.error(s"Cannot find chain ws type for string=$string"))
  }
}

/** A notification that we send over the websocket.
  * The type of the notification is indicated by [[WsType]].
  * An example is [[org.bitcoins.commons.jsonmodels.ws.WalletNotification.NewAddressNotification]]
  * This sends a notification that the wallet generated a new address
  */
sealed trait WsNotification[T] {
  def `type`: WsType
  def payload: T
}

sealed trait ChainNotification[T] extends WsNotification[T] {
  override def `type`: ChainWsType
}

sealed trait WalletNotification[T] extends WsNotification[T] {
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

  case class DLCStateChangeNotification(payload: DLCStatus)
      extends WalletNotification[DLCStatus] {
    override val `type`: WalletWsType = WalletWsType.DLCStateChange
  }
}

object ChainNotification {

  case class BlockProcessedNotification(payload: GetBlockHeaderResult)
      extends ChainNotification[GetBlockHeaderResult] {
    override val `type`: ChainWsType = ChainWsType.BlockProcessed
  }
}
