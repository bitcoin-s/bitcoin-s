package org.bitcoins.marshallers.transaction

import org.bitcoins.marshallers.script.ScriptSignatureMarshaller
import org.bitcoins.marshallers.transaction.TransactionOutPointMarshaller.TransactionOutPointFormatter
import org.bitcoins.protocol.script.ScriptSignature
import org.bitcoins.protocol.transaction.{TransactionInputImpl, TransactionOutPoint, TransactionInput}
import org.bitcoins.util.{BitcoinSUtil}
import spray.json._

/**
 * Created by chris on 12/27/15.

 */
//TODO:  Note that TransactionInput's read from JSON format cannot be reserialized to the correct bitcoin hex format. This is because the scriptSig's are missing their VarInt that indicates how long
//the script signatures are.

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
      val scriptSigBytes : Seq[Byte] = BitcoinSUtil.decodeHex(scriptSig.hex)
      val varIntSize = BitcoinSUtil.parseCompactSizeUIntSize(scriptSigBytes.head)
      val varInt = BitcoinSUtil.parseCompactSizeUInt(scriptSigBytes.slice(0,varIntSize.toInt))
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
