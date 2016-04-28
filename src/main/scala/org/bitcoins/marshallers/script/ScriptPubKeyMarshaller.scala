package org.bitcoins.marshallers.script

import org.bitcoins.marshallers.MarshallerUtil
import org.bitcoins.protocol.script.{ScriptPubKey}
import spray.json._
import DefaultJsonProtocol._

/**
 * Created by chris on 12/27/15.
 */
object ScriptPubKeyMarshaller extends DefaultJsonProtocol with MarshallerUtil {
  val reqSigsKey = "reqSigs"
  val typeKey = "type"
  val addressesKey = "addresses"
  val scriptPubKeyKey = "scriptPubKey"
  implicit object ScriptPubKeyFormatter extends RootJsonFormat[ScriptPubKey] {

    override def read(value : JsValue) : ScriptPubKey = {
      val obj = value.asJsObject
      val asm = obj.fields(ScriptSignatureMarshaller.asmKey)
      ScriptPubKey.fromAsm(ScriptParser.fromString(asm.convertTo[String]))
    }

    override def write(scriptPubKey : ScriptPubKey) : JsValue = {
      import org.bitcoins.marshallers.BitcoinAddressProtocol._
      val m : Map[String,JsValue] = Map(
        ScriptSignatureMarshaller.asmKey -> JsString(scriptPubKey.asm.toString),
        ScriptSignatureMarshaller.hexKey -> JsString(scriptPubKey.hex),
        reqSigsKey -> JsNumber(-1)
      )
      JsObject(m)
    }

  }
}
