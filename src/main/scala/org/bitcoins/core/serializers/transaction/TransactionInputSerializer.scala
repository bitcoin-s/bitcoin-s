package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.serializers.script.ScriptSignatureSerializer
import org.bitcoins.core.serializers.transaction.TransactionOutPointSerializer.TransactionOutPointFormatter
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, TransactionInput}
import org.bitcoins.core.util.{BitcoinSUtil}
import spray.json._

/**
 * Created by chris on 12/27/15.
 *
 */
//TODO:  Note that TransactionInput's read from JSON format cannot be reserialized to the correct bitcoin hex format. This is because the scriptSig's are missing their VarInt that indicates how long
//the script signatures are.

object TransactionInputSerializer extends DefaultJsonProtocol {

  val sequenceKey = "sequence"
  implicit object TransactionInputFormatter extends RootJsonFormat[TransactionInput] {

    override def read(value : JsValue) : TransactionInput = {

      val obj = value.asJsObject
      val scriptSig : ScriptSignature =
        ScriptSignatureSerializer.ScriptSignatureFormatter.read(obj.fields(ScriptSignatureSerializer.scriptSigKey))
      val outPoint : TransactionOutPoint =
        TransactionOutPointSerializer.TransactionOutPointFormatter.read(obj)
      val sequence = obj.fields(sequenceKey)
      val scriptSigBytes : Seq[Byte] = BitcoinSUtil.decodeHex(scriptSig.hex)
      val varIntSize = BitcoinSUtil.parseCompactSizeUIntSize(scriptSigBytes.head)
      val varInt = BitcoinSUtil.parseCompactSizeUInt(scriptSigBytes.slice(0,varIntSize.toInt))
      TransactionInput(outPoint, scriptSig, sequence.convertTo[Long])
    }

    override def write(input : TransactionInput) : JsValue = {
      val m : Map[String,JsValue] = Map(
        ScriptSignatureSerializer.scriptSigKey -> ScriptSignatureSerializer.ScriptSignatureFormatter.write(input.scriptSignature),
        TransactionOutPointSerializer.txIdKey -> JsString(input.previousOutput.txId),
        TransactionOutPointSerializer.voutKey -> JsNumber(input.previousOutput.vout),
        sequenceKey -> JsNumber(input.sequence)
      )
      JsObject(m)
    }
  }
}
