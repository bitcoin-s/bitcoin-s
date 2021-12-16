package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.ws.WalletNotification.{
  BlockProcessedNotification,
  NewAddressNotification,
  ReservedUtxosNotification,
  TxBroadcastNotification,
  TxProcessedNotification
}
import org.bitcoins.commons.jsonmodels.ws.{WalletNotification, WalletWsType}
import org.bitcoins.core.serializers.PicklerKeys
import upickle.default._

object WsPicklers {

  implicit val walletWsTypePickler: ReadWriter[WalletWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => WalletWsType.fromString(str.str))
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
      case BlockProcessedNotification(block) =>
        upickle.default.writeJs(block)(Picklers.getBlockHeaderResultPickler)
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
        val utxos = obj.arr.toVector.map { utxoJson =>
          upickle.default.read(utxoJson)(Picklers.spendingInfoDbPickler)
        }
        ReservedUtxosNotification(utxos)
      case WalletWsType.BlockProcessed =>
        val block =
          upickle.default.read(payloadObj)(Picklers.getBlockHeaderResultPickler)
        BlockProcessedNotification(block)
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

  implicit val blockProcessedPickler: ReadWriter[BlockProcessedNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[BlockProcessedNotification]
    )
  }
}
