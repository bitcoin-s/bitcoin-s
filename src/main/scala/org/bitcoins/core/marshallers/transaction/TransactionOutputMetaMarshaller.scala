package org.bitcoins.core.marshallers.transaction


import org.bitcoins.core.currency.{CurrencyUnits, Bitcoins}
import org.bitcoins.core.marshallers.script.ScriptPubKeyMarshaller
import org.bitcoins.core.marshallers.transaction.TransactionOutputMarshaller.TransactionOutputFormatter
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{TransactionOutput, TransactionOutputMetaImpl, TransactionOutputMeta}
import spray.json
import spray.json._


/**
 * Created by Tom on 1/12/2016.
 */
object TransactionOutputMetaMarshaller extends DefaultJsonProtocol {
  val bestBlockKey = "bestblock"
  val confirmationsKey = "confirmations"
  val versionKey = "version"
  val coinbaseKey = "coinbase"


  implicit object TransactionOutputMetaFormatter extends RootJsonFormat[TransactionOutputMeta] {
    override def read (value : JsValue) : TransactionOutputMeta = {
      val obj = value.asJsObject
      val bestBlock = obj.fields(bestBlockKey).convertTo[String]
      val confirmations = obj.fields(confirmationsKey).convertTo[Int]
      val version = obj.fields(versionKey).convertTo[Int]
      val coinbase = obj.fields(coinbaseKey).convertTo[Boolean]
      val bitcoins = Bitcoins(obj.fields(TransactionOutputMarshaller.valueKey).convertTo[Double])
      val scriptPubKey : ScriptPubKey = ScriptPubKeyMarshaller.ScriptPubKeyFormatter.read(obj.fields(ScriptPubKeyMarshaller.scriptPubKeyKey))
      TransactionOutputMetaImpl(bestBlock, confirmations, version, coinbase, bitcoins, scriptPubKey)
    }

    override def write (meta : TransactionOutputMeta) : JsValue = {
      val valueInBitcoins = CurrencyUnits.sataoshisToBitcoin(CurrencyUnits.toSatoshis(meta.value))
      val m : Map[String, JsValue] = Map (
        bestBlockKey -> JsString(meta.bestBlock),
        confirmationsKey -> JsNumber(meta.confirmations),
        versionKey -> JsNumber(meta.version),
        coinbaseKey -> JsBoolean(meta.coinbase),
        ScriptPubKeyMarshaller.scriptPubKeyKey -> ScriptPubKeyMarshaller.ScriptPubKeyFormatter.write(meta.scriptPubKey),
        TransactionOutputMarshaller.valueKey -> JsNumber(valueInBitcoins.value)
      )
      JsObject(m)
    }
  }
}
