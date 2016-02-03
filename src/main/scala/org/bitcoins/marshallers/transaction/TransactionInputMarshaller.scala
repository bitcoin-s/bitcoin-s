package org.bitcoins.marshallers.transaction

import org.bitcoins.marshallers.script.ScriptSignatureMarshaller
import org.bitcoins.marshallers.transaction.TransactionOutPointMarshaller.TransactionOutPointFormatter
import org.bitcoins.protocol.script.ScriptSignature
import org.bitcoins.protocol.transaction.{TransactionInputImpl, TransactionOutPoint, TransactionInput}
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

      TransactionInputImpl(outPoint, scriptSig, sequence.convertTo[Long])
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
