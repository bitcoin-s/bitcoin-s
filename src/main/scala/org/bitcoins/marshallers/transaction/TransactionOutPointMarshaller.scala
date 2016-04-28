package org.bitcoins.marshallers.transaction

import org.bitcoins.protocol.transaction.{TransactionOutPointImpl, TransactionOutPoint}
import spray.json._

/**
 * Created by chris on 12/27/15.
 */
object TransactionOutPointMarshaller extends DefaultJsonProtocol {

  val txIdKey = "txid"
  val voutKey = "vout"
  implicit object TransactionOutPointFormatter extends RootJsonFormat[TransactionOutPoint] {

    override def read(value : JsValue) : TransactionOutPoint = {
      val obj = value.asJsObject
      val txId = obj.fields(txIdKey)
      val vout = obj.fields(voutKey)
      TransactionOutPointImpl(txId.convertTo[String], vout.convertTo[Int])
    }

    override def write(outPoint : TransactionOutPoint) : JsValue = {
      val m : Map[String,JsValue] = Map(
        txIdKey -> JsString(outPoint.txId),
        voutKey -> JsNumber(outPoint.vout)
      )
      JsObject(m)
    }
  }
}
