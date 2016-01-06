package org.scalacoin.marshallers.wallet

import org.scalacoin.protocol.wallet.{WalletInfo, WalletInfoImpl}
import spray.json._

/**
 * Created by Tom on 1/6/2016.
 */
object WalletMarshaller extends DefaultJsonProtocol {
  val walletVersionKey = "walletversion"
  val balanceKey = "balance"
  val unconfirmedBalanceKey = "unconfirmed_balance"
  val immatureBalanceKey = "immature_balance"
  val txCountKey = "txcount"
  val keyPoolOldestKey = "keypoololdest"
  val keyPoolSizeKey = "keypoolsize"


  implicit object WalletFormatter extends RootJsonFormat[WalletInfo] {

    override def read(value: JsValue): WalletInfo = {
      val obj = value.asJsObject
      val walletVersion = obj.fields(walletVersionKey).convertTo[Int]
      val balance = obj.fields(balanceKey).convertTo[Double]
      val unconfirmedBalance = obj.fields(unconfirmedBalanceKey).convertTo[Double]
      val immatureBalance = obj.fields(immatureBalanceKey).convertTo[Double]
      val txCount = obj.fields(txCountKey).convertTo[Int]
      val keyPoolOldest = obj.fields(keyPoolOldestKey).convertTo[Long]
      val keyPoolSize = obj.fields(keyPoolSizeKey).convertTo[Int]
      WalletInfoImpl(walletVersion, balance, unconfirmedBalance, immatureBalance, txCount, keyPoolOldest, keyPoolSize)
    }

    override def write(wallet : WalletInfo) : JsValue = {
      val m : Map[String,JsValue] = Map (
        walletVersionKey -> JsNumber(wallet.walletVersion),
        balanceKey -> JsNumber(wallet.balance),
        unconfirmedBalanceKey -> JsNumber(wallet.unconfirmedBalance),
        immatureBalanceKey -> JsNumber(wallet.immatureBalance),
        txCountKey -> JsNumber(wallet.txCount),
        keyPoolOldestKey -> JsNumber(wallet.keyPoolOldest),
        keyPoolSizeKey -> JsNumber(wallet.keyPoolSize)
      )
      JsObject(m)
    }
  }
}