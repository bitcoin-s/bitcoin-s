package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.ws.ChainNotification.{
  BlockProcessedNotification,
  SyncFlagChangedNotification
}
import org.bitcoins.commons.jsonmodels.ws.DLCNodeNotification.{
  DLCNodeConnectionEstablished,
  DLCNodeConnectionFailed,
  DLCNodeConnectionInitiated
}
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.{
  DLCOfferAddNotification,
  DLCOfferRemoveNotification,
  DLCStateChangeNotification,
  FeeRateChange,
  NewAddressNotification,
  RescanComplete,
  ReservedUtxosNotification,
  TxBroadcastNotification,
  TxProcessedNotification
}
import org.bitcoins.commons.jsonmodels.ws.{
  ChainNotification,
  ChainWsType,
  DLCNodeNotification,
  DLCNodeWsType,
  TorNotification,
  TorWsType,
  WalletNotification,
  WalletWsType
}
import org.bitcoins.core.config.DLC
import org.bitcoins.core.serializers.PicklerKeys
import org.bitcoins.core.util.NetworkUtil
import upickle.default._

import java.net.InetSocketAddress

object WsPicklers {

  implicit val chainWsTypePickler: ReadWriter[ChainWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => ChainWsType.fromString(str.str))
  }

  implicit val walletWsTypePickler: ReadWriter[WalletWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => WalletWsType.fromString(str.str))
  }

  implicit val torWsTypePickler: ReadWriter[TorWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => TorWsType.fromString(str.str))
  }

  implicit val dlcNodeWsTypePickler: ReadWriter[DLCNodeWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => DLCNodeWsType.fromString(str.str))
  }

  private def writeChainNotification(
      notification: ChainNotification[_]): ujson.Obj = {
    val payloadJson: ujson.Value = notification match {
      case BlockProcessedNotification(block) =>
        upickle.default.writeJs(block)(Picklers.getBlockHeaderResultPickler)
      case SyncFlagChangedNotification(syncing) =>
        upickle.default.writeJs(syncing)
    }
    val notificationObj = ujson.Obj(
      PicklerKeys.typeKey -> writeJs(notification.`type`),
      PicklerKeys.payloadKey -> payloadJson
    )
    notificationObj
  }

  private def readChainNotification(obj: ujson.Obj): ChainNotification[_] = {
    val typeObj = read[ChainWsType](obj(PicklerKeys.typeKey))
    val payloadObj = obj(PicklerKeys.payloadKey)

    typeObj match {
      case ChainWsType.BlockProcessed =>
        val block =
          upickle.default.read(payloadObj)(Picklers.getBlockHeaderResultPickler)
        BlockProcessedNotification(block)
      case ChainWsType.SyncFlagChanged =>
        val syncing = payloadObj.bool
        SyncFlagChangedNotification(syncing)
    }
  }

  private def writeWalletNotification(
      notification: WalletNotification[_]): ujson.Obj = {
    val payloadJson: ujson.Value = notification match {
      case TxBroadcastNotification(tx) =>
        upickle.default.writeJs(tx)(Picklers.transactionPickler)
      case TxProcessedNotification(tx) =>
        upickle.default.writeJs(tx)(Picklers.transactionPickler)
      case NewAddressNotification(address) =>
        upickle.default.writeJs(address)(Picklers.bitcoinAddressPickler)
      case ReservedUtxosNotification(utxos) =>
        val vec = utxos.map(u =>
          upickle.default.writeJs(u)(Picklers.spendingInfoDbPickler))
        ujson.Arr.from(vec)
      case DLCStateChangeNotification(status) =>
        upickle.default.writeJs(status)(Picklers.dlcStatusW)
      case DLCOfferAddNotification(offerDb) =>
        upickle.default.writeJs(offerDb)(Picklers.dlcOfferAddW)
      case DLCOfferRemoveNotification(offerHash) =>
        upickle.default.writeJs(offerHash)(Picklers.dlcOfferRemoveW)
      case r: RescanComplete =>
        upickle.default.writeJs(r)(Picklers.rescanComplete)
      case FeeRateChange(feeRate) =>
        upickle.default.writeJs(feeRate)(Picklers.feeUnit)
    }

    val notificationObj = ujson.Obj(
      PicklerKeys.typeKey -> writeJs(notification.`type`),
      PicklerKeys.payloadKey -> payloadJson
    )
    notificationObj
  }

  private def readWalletNotification(obj: ujson.Obj): WalletNotification[_] = {
    val typeObj = read[WalletWsType](obj(PicklerKeys.typeKey))
    val payloadObj = obj(PicklerKeys.payloadKey)
    typeObj match {
      case WalletWsType.TxBroadcast =>
        val tx = upickle.default.read(payloadObj)(Picklers.transactionPickler)
        TxBroadcastNotification(tx)
      case WalletWsType.TxProcessed =>
        val tx = upickle.default.read(payloadObj)(Picklers.transactionPickler)
        TxProcessedNotification(tx)
      case WalletWsType.NewAddress =>
        val address =
          upickle.default.read(payloadObj)(Picklers.bitcoinAddressPickler)
        NewAddressNotification(address)
      case WalletWsType.ReservedUtxos =>
        val utxos = obj(PicklerKeys.payloadKey).arr.toVector.map { utxoJson =>
          upickle.default.read(utxoJson)(Picklers.spendingInfoDbPickler)
        }
        ReservedUtxosNotification(utxos)
      case WalletWsType.DLCStateChange =>
        val status = upickle.default.read(payloadObj)(Picklers.dlcStatusR)
        DLCStateChangeNotification(status)
      case WalletWsType.DLCOfferAdd =>
        val offerDb = upickle.default.read(payloadObj)(Picklers.dlcOfferAddR)
        DLCOfferAddNotification(offerDb)
      case WalletWsType.DLCOfferRemove =>
        val offerHash =
          upickle.default.read(payloadObj)(Picklers.dlcOfferRemoveR)
        DLCOfferRemoveNotification(offerHash)
      case WalletWsType.RescanComplete =>
        val complete = upickle.default.read(payloadObj)(Picklers.rescanComplete)
        complete
      case WalletWsType.FeeRateChange =>
        FeeRateChange(upickle.default.read(payloadObj)(Picklers.feeUnit))
    }
  }

  private def writeTorNotification(
      notification: TorNotification[_]): ujson.Obj = {
    val payloadJson = notification.`type` match {
      case TorWsType.TorStarted =>
        ujson.Null
    }

    val notificationObj = ujson.Obj(
      PicklerKeys.typeKey -> writeJs(notification.`type`),
      PicklerKeys.payloadKey -> payloadJson
    )
    notificationObj
  }

  private def readTorNotification(obj: ujson.Obj): TorNotification[_] = {
    val typeObj = read[TorWsType](obj(PicklerKeys.typeKey))
    typeObj match {
      case TorWsType.TorStarted =>
        TorNotification.TorStartedNotification
    }
  }

  private def writeDLCNodeNotification(
      notification: DLCNodeNotification[_]): ujson.Obj = {
    def addr2str(address: InetSocketAddress) =
      address.getHostName + ":" + address.getPort
    val payloadJson: ujson.Value = notification match {
      case DLCNodeConnectionInitiated(address) =>
        upickle.default.writeJs(addr2str(address))
      case DLCNodeConnectionEstablished(address) =>
        upickle.default.writeJs(addr2str(address))
      case DLCNodeConnectionFailed(address) =>
        upickle.default.writeJs(addr2str(address))
    }
    val notificationObj = ujson.Obj(
      PicklerKeys.typeKey -> writeJs(notification.`type`),
      PicklerKeys.payloadKey -> payloadJson
    )
    notificationObj
  }

  private def readDLCNodeNotification(
      obj: ujson.Obj): DLCNodeNotification[_] = {
    val typeObj = read[DLCNodeWsType](obj(PicklerKeys.typeKey))
    val payloadObj = obj(PicklerKeys.payloadKey)

    typeObj match {
      case DLCNodeWsType.DLCConnectionInitiated =>
        val address: InetSocketAddress =
          NetworkUtil.parseInetSocketAddress(payloadObj.str, DLC.DefaultPort)
        DLCNodeConnectionInitiated(address)
      case DLCNodeWsType.DLCConnectionEstablished =>
        val address: InetSocketAddress =
          NetworkUtil.parseInetSocketAddress(payloadObj.str, DLC.DefaultPort)
        DLCNodeConnectionEstablished(address)
      case DLCNodeWsType.DLCConnectionFailed =>
        val address: InetSocketAddress =
          NetworkUtil.parseInetSocketAddress(payloadObj.str, DLC.DefaultPort)
        DLCNodeConnectionFailed(address)
    }
  }

  implicit val newAddressPickler: ReadWriter[NewAddressNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[NewAddressNotification])
  }

  implicit val txProcessedPickler: ReadWriter[TxProcessedNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[TxProcessedNotification])
  }

  implicit val txBroadcastPickler: ReadWriter[TxBroadcastNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[TxBroadcastNotification])
  }

  implicit val reservedUtxosPickler: ReadWriter[ReservedUtxosNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[ReservedUtxosNotification])
  }

  implicit val rescanPickler: ReadWriter[RescanComplete] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[RescanComplete]
    )
  }

  implicit val feeRatePickler: ReadWriter[FeeRateChange] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[FeeRateChange]
    )
  }

  implicit val walletNotificationPickler: ReadWriter[WalletNotification[_]] = {
    readwriter[ujson.Obj].bimap(writeWalletNotification, readWalletNotification)
  }

  implicit val chainNotificationPickler: ReadWriter[ChainNotification[_]] = {
    readwriter[ujson.Obj].bimap(writeChainNotification, readChainNotification)
  }

  implicit val blockProcessedPickler: ReadWriter[BlockProcessedNotification] = {
    readwriter[ujson.Obj].bimap(
      writeChainNotification(_),
      readChainNotification(_).asInstanceOf[BlockProcessedNotification]
    )
  }

  implicit val syncFlagChangedPickler: ReadWriter[
    SyncFlagChangedNotification] = {
    readwriter[ujson.Obj].bimap(
      writeChainNotification(_),
      readChainNotification(_).asInstanceOf[SyncFlagChangedNotification]
    )
  }

  implicit val dlcStateChangePickler: ReadWriter[DLCStateChangeNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[DLCStateChangeNotification])
  }

  implicit val dlcOfferAddPickler: ReadWriter[DLCOfferAddNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[DLCOfferAddNotification])
  }

  implicit val dlcOfferRemovePickler: ReadWriter[DLCOfferRemoveNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[DLCOfferRemoveNotification])
  }

  implicit val torStartedPickler: ReadWriter[
    TorNotification.TorStartedNotification.type] = {
    readwriter[ujson.Obj].bimap(
      writeTorNotification(_),
      readTorNotification(_)
        .asInstanceOf[TorNotification.TorStartedNotification.type])
  }

  implicit val dlcNodeConnectionInitiatedPickler: ReadWriter[
    DLCNodeConnectionInitiated] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCNodeConnectionInitiated])
  }

  implicit val dlcNodeConnectionFailedPickler: ReadWriter[
    DLCNodeConnectionFailed] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCNodeConnectionFailed])
  }

  implicit val dlcNodeConnectionEstablishedPickler: ReadWriter[
    DLCNodeConnectionEstablished] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCNodeConnectionEstablished])
  }

}
