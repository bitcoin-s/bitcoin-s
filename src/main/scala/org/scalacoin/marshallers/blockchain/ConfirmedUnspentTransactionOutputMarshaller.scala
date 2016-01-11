package org.scalacoin.marshallers.blockchain

import org.scalacoin.protocol.blockchain.{ConfirmedUnspentTransactionOutput, ConfirmedUnspentTransactionOutputImpl}
import spray.json._

/**
 * Created by Tom on 1/11/2016.
 */
object ConfirmedUnspentTransactionOutputMarshaller extends DefaultJsonProtocol {
  val heightKey = "height"
  val bestBlockKey = "bestblock"
  val transactionsKey = "transactions"
  val txOutsKey = "txouts"
  val bytesSerializedKey = "bytes_serialized"
  val hashSerializedKey = "hash_serialized"
  val totalAmountKey = "total_amount"

  implicit object ConfirmedUnspentTransactionOutputFormatter extends RootJsonFormat[ConfirmedUnspentTransactionOutput] {
    override def read (value : JsValue) : ConfirmedUnspentTransactionOutput = {
      val obj = value.asJsObject
      val height = obj.fields(heightKey).convertTo[Int]
      val bestBlock = obj.fields(bestBlockKey).convertTo[String]
      val transactions = obj.fields(transactionsKey).convertTo[Int]
      val txOuts = obj.fields(txOutsKey).convertTo[Int]
      val bytesSerialized = obj.fields(bytesSerializedKey).convertTo[Int]
      val hashSerialized = obj.fields(hashSerializedKey).convertTo[String]
      val totalAmount = obj.fields(totalAmountKey).convertTo[Double]
      ConfirmedUnspentTransactionOutputImpl(height, bestBlock, transactions, txOuts, bytesSerialized, hashSerialized, totalAmount)
    }
    override def write (utxos : ConfirmedUnspentTransactionOutput) : JsValue = {
      val m : Map[String, JsValue] = Map (
        heightKey -> JsNumber(utxos.height),
        bestBlockKey -> JsString(utxos.bestBlock),
        transactionsKey -> JsNumber(utxos.transactions),
        txOutsKey -> JsNumber(utxos.txOuts),
        bytesSerializedKey -> JsNumber(utxos.bytesSerialized),
        hashSerializedKey -> JsString(utxos.hashSerialized),
        totalAmountKey -> JsNumber(utxos.totalAmount)
      )
      JsObject(m)
    }
  }
}
