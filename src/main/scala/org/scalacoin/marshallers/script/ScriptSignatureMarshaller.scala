package org.scalacoin.marshallers.script

import org.scalacoin.protocol.script.{ScriptSignature, ScriptSignatureImpl}
import org.slf4j.LoggerFactory
import spray.json._

/**
 * Created by chris on 12/27/15.
 */
object ScriptSignatureMarshaller extends DefaultJsonProtocol {
  val scriptSigKey = "scriptSig"
  val asmKey = "asm"
  val hexKey = "hex"
  private lazy val logger = LoggerFactory.getLogger(this.getClass().toString)
  implicit object ScriptSignatureFormatter extends RootJsonFormat[ScriptSignature] {

    override def read(value : JsValue) : ScriptSignature = {
      logger.debug(this.getClass().toString + " is marshalling json value: " + value)
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
