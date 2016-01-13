package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.MarshallerUtil
import org.scalacoin.protocol.script.{ScriptPubKeyImpl, ScriptPubKey}
import spray.json._
import DefaultJsonProtocol._

/**
 * Created by chris on 12/27/15.
 */
object ScriptPubKeyMarshaller extends DefaultJsonProtocol with MarshallerUtil with ScriptParser {
  val reqSigsKey = "reqSigs"
  val typeKey = "type"
  val addressesKey = "addresses"
  val scriptPubKeyKey = "scriptPubKey"
  implicit object ScriptPubKeyFormatter extends RootJsonFormat[ScriptPubKey] {

    override def read(value : JsValue) : ScriptPubKey = {
      val obj = value.asJsObject
      val asm = parse(obj.fields(ScriptSignatureMarshaller.asmKey).convertTo[String])
      val hex = obj.fields(ScriptSignatureMarshaller.hexKey)
      val addresses = convertToAddressList(obj.fields(addressesKey))
      ScriptPubKeyImpl(asm, hex.convertTo[String], addresses)
    }

    override def write(scriptPubKey : ScriptPubKey) : JsValue = {
      import org.scalacoin.marshallers.BitcoinAddressProtocol._
      val addressList = convertToJsArray(scriptPubKey.addresses)
      val m : Map[String,JsValue] = Map(
        ScriptSignatureMarshaller.asmKey -> JsString(scriptPubKey.asm.toString),
        ScriptSignatureMarshaller.hexKey -> JsString(scriptPubKey.hex),
        reqSigsKey -> JsNumber(scriptPubKey.reqSigs.get),
        typeKey -> JsString(scriptPubKey.addressType.toString),
        addressesKey -> addressList
      )

      JsObject(m)
    }

  }
}
