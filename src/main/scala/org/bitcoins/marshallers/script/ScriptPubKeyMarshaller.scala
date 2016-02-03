package org.bitcoins.marshallers.script

import org.bitcoins.marshallers.MarshallerUtil
import org.bitcoins.protocol.BitcoinAddress
import org.bitcoins.protocol.script.{ScriptPubKeyImpl, ScriptPubKey}
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
      val hex = obj.fields(ScriptSignatureMarshaller.hexKey)
      val reqSigs = obj.fields(reqSigsKey)
      val addressType = obj.fields(typeKey)
      val addresses = convertToAddressList(obj.fields(addressesKey))

      ScriptPubKeyImpl(asm.convertTo[String], hex.convertTo[String], reqSigs.convertTo[Int],
        addressType.convertTo[String], addresses)
    }

    override def write(scriptPubKey : ScriptPubKey) : JsValue = {
      import org.bitcoins.marshallers.BitcoinAddressProtocol._
      val addressList = convertToJsArray(scriptPubKey.addresses)
      val m : Map[String,JsValue] = Map(
        ScriptSignatureMarshaller.asmKey -> JsString(scriptPubKey.asm),
        ScriptSignatureMarshaller.hexKey -> JsString(scriptPubKey.hex),
        reqSigsKey -> JsNumber(scriptPubKey.reqSigs),
        typeKey -> JsString(scriptPubKey.addressType),
        addressesKey -> addressList
      )

      JsObject(m)
    }

  }
}
