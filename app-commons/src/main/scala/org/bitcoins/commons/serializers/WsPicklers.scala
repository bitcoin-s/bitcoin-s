package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.ws.ChainNotification.BlockProcessedNotification
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.{
  DLCStateChangeNotification,
  NewAddressNotification,
  ReservedUtxosNotification,
  TxBroadcastNotification,
  TxProcessedNotification
}
import org.bitcoins.commons.jsonmodels.ws.{
  ChainNotification,
  ChainWsType,
  WalletNotification,
  WalletWsType
}
import org.bitcoins.core.serializers.PicklerKeys
import upickle.default._

object WsPicklers {

  implicit val chainWsTypePickler: ReadWriter[ChainWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => ChainWsType.fromString(str.str))
  }

  implicit val walletWsTypePickler: ReadWriter[WalletWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => WalletWsType.fromString(str.str))
  }

  private def writeChainNotification(
      notification: ChainNotification[_]): ujson.Obj = {
    val payloadJson: ujson.Value = notification match {
      case BlockProcessedNotification(block) =>
        upickle.default.writeJs(block)(Picklers.getBlockHeaderResultPickler)
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

  implicit val dlcStateChangePickler: ReadWriter[DLCStateChangeNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[DLCStateChangeNotification])
  }
}
