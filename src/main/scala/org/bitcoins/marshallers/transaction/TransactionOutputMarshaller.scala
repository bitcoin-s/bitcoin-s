package org.bitcoins.marshallers.transaction

import org.bitcoins.currency.{CurrencyUnits, Bitcoins}
import org.bitcoins.marshallers.script.ScriptPubKeyMarshaller
import org.bitcoins.protocol.script.ScriptPubKey
import org.bitcoins.protocol.transaction.{TransactionOutputImpl, TransactionOutput}
import spray.json._

/**
 * Created by chris on 12/27/15.
 */
object TransactionOutputMarshaller extends DefaultJsonProtocol {
  val valueKey = "value"


  implicit object TransactionOutputFormatter extends RootJsonFormat[TransactionOutput] {

    override def read(value : JsValue) : TransactionOutput = {
      val obj = value.asJsObject
      val bitcoins = Bitcoins(obj.fields(valueKey).convertTo[Double])
      val scriptPubKey : ScriptPubKey = ScriptPubKeyMarshaller.ScriptPubKeyFormatter.read(obj.fields(ScriptPubKeyMarshaller.scriptPubKeyKey))
      TransactionOutputImpl(bitcoins, scriptPubKey)
    }

    override def write(output : TransactionOutput) : JsValue = {
      val valueInBitcoins = CurrencyUnits.sataoshisToBitcoin(CurrencyUnits.toSatoshis(output.value))
      val m : Map[String,JsValue] = Map(
        valueKey -> JsNumber(valueInBitcoins.value),
        ScriptPubKeyMarshaller.scriptPubKeyKey -> ScriptPubKeyMarshaller.ScriptPubKeyFormatter.write(output.scriptPubKey)
      )
      JsObject(m)
    }
  }
}
