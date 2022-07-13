package org.bitcoins.commons.jsonmodels.ws

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{Sha256Digest, StringFactory}

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
sealed trait TorWsType extends WsType

object WalletWsType extends StringFactory[WalletWsType] {
  case object TxProcessed extends WalletWsType
  case object TxBroadcast extends WalletWsType
  case object ReservedUtxos extends WalletWsType
  case object NewAddress extends WalletWsType

  case object DLCStateChange extends WalletWsType
  case object DLCOfferAdd extends WalletWsType
  case object DLCOfferRemove extends WalletWsType
  case object RescanComplete extends WalletWsType

  private val all =
    Vector(TxProcessed,
           TxBroadcast,
           ReservedUtxos,
           NewAddress,
           DLCStateChange,
           DLCOfferAdd,
           DLCOfferRemove,
           RescanComplete)

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
  case object SyncFlagChanged extends ChainWsType

  private val all: Vector[ChainWsType] = Vector(BlockProcessed, SyncFlagChanged)

  override def fromStringOpt(string: String): Option[ChainWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): ChainWsType = {
    fromStringOpt(string)
      .getOrElse(sys.error(s"Cannot find chain ws type for string=$string"))
  }
}

object TorWsType extends StringFactory[TorWsType] {
  case object TorStarted extends TorWsType

  private val all = Vector(TorStarted)

  override def fromStringOpt(string: String): Option[TorWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): TorWsType = {
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

sealed trait TorNotification[T] extends WsNotification[T] {
  override def `type`: TorWsType
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

  case class DLCOfferAddNotification(payload: IncomingDLCOfferDb)
      extends WalletNotification[IncomingDLCOfferDb] {
    override val `type`: WalletWsType = WalletWsType.DLCOfferAdd
  }

  case class DLCOfferRemoveNotification(payload: Sha256Digest)
      extends WalletNotification[Sha256Digest] {
    override val `type`: WalletWsType = WalletWsType.DLCOfferRemove
  }

  case class RescanComplete(payload: String)
      extends WalletNotification[String] {
    override val `type`: WalletWsType = WalletWsType.RescanComplete
  }
}

object ChainNotification {

  case class BlockProcessedNotification(payload: GetBlockHeaderResult)
      extends ChainNotification[GetBlockHeaderResult] {
    override val `type`: ChainWsType = ChainWsType.BlockProcessed
  }

  case class SyncFlagChangedNotification(payload: Boolean)
      extends ChainNotification[Boolean] {
    override val `type`: ChainWsType = ChainWsType.SyncFlagChanged
  }
}

object TorNotification {

  case object TorStartedNotification extends TorNotification[Unit] {
    override val `type`: TorWsType = TorWsType.TorStarted
    override val payload: Unit = ()
  }
}