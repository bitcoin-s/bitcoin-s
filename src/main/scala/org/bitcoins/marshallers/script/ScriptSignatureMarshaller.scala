package org.bitcoins.marshallers.script

import org.bitcoins.protocol.script.{ScriptSignature, ScriptSignatureImpl}
import spray.json._

/**
 * Created by chris on 12/27/15.
 */
object ScriptSignatureMarshaller extends DefaultJsonProtocol {
  val scriptSigKey = "scriptSig"
  val asmKey = "asm"
  val hexKey = "hex"
  implicit object ScriptSignatureFormatter extends RootJsonFormat[ScriptSignature] {

    override def read(value : JsValue) : ScriptSignature = {
      val obj = value.asJsObject
      val asm = obj.fields(asmKey)
      val hex = obj.fields(hexKey)
      ScriptSignatureImpl(asm.convertTo[String], hex.convertTo[String])
    }

    override def write(scriptSig : ScriptSignature) : JsValue = {
      val m : Map[String,JsValue] = Map(
        asmKey -> JsString(scriptSig.asm),
        hexKey -> JsString(scriptSig.hex)
      )

      JsObject(m)
    }
  }
}
