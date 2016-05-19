package org.bitcoins.core.serializers.script

import org.bitcoins.core.serializers.MarshallerUtil
import org.bitcoins.core.protocol.script.{ScriptPubKey}
import spray.json._
import DefaultJsonProtocol._

/**
 * Created by chris on 12/27/15.
 */
object ScriptPubKeySerializer extends DefaultJsonProtocol with MarshallerUtil {
  val reqSigsKey = "reqSigs"
  val typeKey = "type"
  val addressesKey = "addresses"
  val scriptPubKeyKey = "scriptPubKey"
  implicit object ScriptPubKeyFormatter extends RootJsonFormat[ScriptPubKey] {

    override def read(value : JsValue) : ScriptPubKey = {
      val obj = value.asJsObject
      val asm = obj.fields(ScriptSignatureSerializer.asmKey)
      ScriptPubKey.fromAsm(ScriptParser.fromString(asm.convertTo[String]))
    }

    override def write(scriptPubKey : ScriptPubKey) : JsValue = {
      import org.bitcoins.core.serializers.BitcoinAddressProtocol._
      val m : Map[String,JsValue] = Map(
        ScriptSignatureSerializer.asmKey -> JsString(scriptPubKey.asm.toString),
        ScriptSignatureSerializer.hexKey -> JsString(scriptPubKey.hex),
        reqSigsKey -> JsNumber(-1)
      )
      JsObject(m)
    }

  }
}
