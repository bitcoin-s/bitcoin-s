package org.scalacoin.marshallers.transaction

import org.scalacoin.marshallers.script.ScriptSignatureMarshaller
import org.scalacoin.marshallers.transaction.TransactionOutPointMarshaller.TransactionOutPointFormatter
import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.protocol.transaction.{TransactionInputImpl, TransactionOutPoint, TransactionInput}
import org.scalacoin.util.ScalacoinUtil
import spray.json._

/**
 * Created by chris on 12/27/15.
 */
object TransactionInputMarshaller extends DefaultJsonProtocol {

  val sequenceKey = "sequence"
  implicit object TransactionInputFormatter extends RootJsonFormat[TransactionInput] {

    override def read(value : JsValue) : TransactionInput = {

      val obj = value.asJsObject
      val scriptSig : ScriptSignature =
        ScriptSignatureMarshaller.ScriptSignatureFormatter.read(obj.fields(ScriptSignatureMarshaller.scriptSigKey))
      val outPoint : TransactionOutPoint =
        TransactionOutPointMarshaller.TransactionOutPointFormatter.read(obj)
      val sequence = obj.fields(sequenceKey)
      val scriptSigBytes : Seq[Byte] = ScalacoinUtil.decodeHex(scriptSig.hex)
      val varIntSize = ScalacoinUtil.parseVarIntSize(scriptSigBytes.head)
      val varInt = ScalacoinUtil.parseVarInt(scriptSigBytes.slice(0,varIntSize.toInt))
      TransactionInputImpl(outPoint, varInt, scriptSig, sequence.convertTo[Long])
    }

    override def write(input : TransactionInput) : JsValue = {
      val m : Map[String,JsValue] = Map(
        ScriptSignatureMarshaller.scriptSigKey -> ScriptSignatureMarshaller.ScriptSignatureFormatter.write(input.scriptSignature),
        TransactionOutPointMarshaller.txIdKey -> JsString(input.previousOutput.txId),
        TransactionOutPointMarshaller.voutKey -> JsNumber(input.previousOutput.vout),
        sequenceKey -> JsNumber(input.sequence)
      )
      JsObject(m)

    }

  }

}
