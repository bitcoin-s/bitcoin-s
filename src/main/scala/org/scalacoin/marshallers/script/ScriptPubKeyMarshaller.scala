package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.MarshallerUtil
import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKey}
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
      ScriptPubKeyFactory.fromAsm(ScriptParser.fromString(asm.convertTo[String]))
    }

    override def write(scriptPubKey : ScriptPubKey) : JsValue = {
      import org.scalacoin.marshallers.BitcoinAddressProtocol._
      val addressList = convertToJsArray(scriptPubKey.addresses)
      val m : Map[String,JsValue] = Map(
        ScriptSignatureMarshaller.asmKey -> JsString(scriptPubKey.asm.toString),
        ScriptSignatureMarshaller.hexKey -> JsString(scriptPubKey.hex),
        reqSigsKey -> JsNumber(-1),
        typeKey -> JsString(scriptPubKey.scriptType.toString),
        addressesKey -> addressList
      )
      JsObject(m)
    }

  }
}
