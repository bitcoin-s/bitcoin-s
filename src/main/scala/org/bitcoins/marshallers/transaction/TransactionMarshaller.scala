package org.bitcoins.marshallers.transaction

import org.bitcoins.marshallers.MarshallerUtil
import org.bitcoins.protocol.transaction.{TransactionImpl, TransactionOutput, TransactionInput, Transaction}
import spray.json._
import DefaultJsonProtocol._

/**
 * Created by chris on 12/28/15.
 */
object TransactionMarshaller extends DefaultJsonProtocol with MarshallerUtil {
  val txIdKey = "txid"
  val versionKey = "version"
  val lockTimeKey = "locktime"
  val vinKey = "vin"
  val voutKey = "vout"


  implicit object TransactionFormatter extends RootJsonFormat[Transaction] {


    override def read(value : JsValue) : Transaction = {
      val obj = value.asJsObject
      val version = obj.fields(versionKey).convertTo[Int]
      val lockTime = obj.fields(lockTimeKey).convertTo[Long]
      val inputs : Seq[TransactionInput] = convertToTransactionInputList(obj.fields(vinKey))
      val outputs : Seq[TransactionOutput] = convertToTransactionOutputList(obj.fields(voutKey))
      TransactionImpl(version,inputs,outputs,lockTime)

    }

    override def write(tx : Transaction) : JsValue = {
      import TransactionInputMarshaller._
      import TransactionOutputMarshaller._
      val inputs : JsArray = convertToJsArray(tx.inputs)
      val outputs : JsArray = convertToJsArray(tx.outputs)
      val m : Map[String,JsValue] = Map(
        txIdKey -> JsString(tx.txId),
        versionKey -> JsNumber(tx.version),
        vinKey -> inputs,
        voutKey -> outputs,
        lockTimeKey -> JsNumber(tx.lockTime)
      )

      JsObject(m)
    }

  }
}
