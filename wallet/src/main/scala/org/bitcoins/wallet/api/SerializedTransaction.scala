package org.bitcoins.wallet.api

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{
  EmptyScriptWitness,
  P2WPKHWitnessV0,
  P2WSHWitnessV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  NonWitnessTransaction,
  Transaction,
  WitnessTransaction
}
import org.bitcoins.core.script.constant.{
  ScriptConstant,
  ScriptNumberOperation,
  ScriptToken
}
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  ECDigitalSignature,
  ECPublicKey
}
import play.api.libs.json.{JsNumber, JsString, Json, Writes}
import scodec.bits.ByteVector

case class SerializedTransaction(
    txid: DoubleSha256DigestBE,
    wtxid: Option[DoubleSha256DigestBE],
    version: Int32,
    size: Long,
    vsize: Long,
    weight: Long,
    locktime: UInt32,
    vin: Vector[SerializedTransactionInput],
    vout: Vector[SerializedTransactionOutput])

case class SerializedTransactionInput(
    txid: DoubleSha256DigestBE,
    hex: String,
    vout: UInt32,
    scriptSig: Vector[ScriptToken],
    txinwitness: Option[SerializedTransactionWitness],
    sequence: UInt32
)

case class SerializedTransactionWitness(
    hex: String,
    scriptType: Option[String],
    script: Option[Vector[ScriptToken]],
    pubKey: Option[ECPublicKey],
    signature: Option[ECDigitalSignature],
    stack: Option[Vector[ByteVector]])

case class SerializedTransactionOutput(
    value: BigDecimal,
    n: UInt32,
    scriptPubKey: ScriptPubKey,
    hex: String
)

object SerializedTransaction {

  private def tokenToString(token: ScriptToken): String = {
    token match {
      case numOp: ScriptNumberOperation => numOp.toString
      case constOp: ScriptConstant      => constOp.bytes.toString
      case otherOp: ScriptToken         => otherOp.toString
    }
  }

  def decodeRawTransaction(tx: Transaction): String = {
    val inputs = tx.inputs.toVector.zipWithIndex.map {
      case (input, index) =>
        val witnessOpt = tx match {
          case _: NonWitnessTransaction => None
          case wtx: WitnessTransaction =>
            wtx.witness.witnesses(index) match {
              case EmptyScriptWitness => None
              case p2wpkh: P2WPKHWitnessV0 =>
                Some(
                  SerializedTransactionWitness(hex = p2wpkh.hex,
                                               scriptType = Some("P2WPKH"),
                                               script = None,
                                               pubKey = Some(p2wpkh.pubKey),
                                               signature =
                                                 Some(p2wpkh.signature),
                                               stack = None))
              case p2wsh: P2WSHWitnessV0 =>
                Some(
                  SerializedTransactionWitness(
                    hex = p2wsh.hex,
                    scriptType = Some("P2WSH"),
                    script = Some(p2wsh.redeemScript.asm.toVector),
                    pubKey = None,
                    signature = None,
                    stack = Some(p2wsh.stack.toVector.tail)))
            }
        }

        SerializedTransactionInput(
          txid = input.previousOutput.txIdBE,
          hex = input.hex,
          vout = input.previousOutput.vout,
          scriptSig = input.scriptSignature.asm.toVector,
          txinwitness = witnessOpt,
          sequence = input.sequence
        )
    }

    val outputs = tx.outputs.toVector.zipWithIndex.map {
      case (output, index) =>
        SerializedTransactionOutput(value = output.value.toBigDecimal,
                                    n = UInt32(index),
                                    scriptPubKey = output.scriptPubKey,
                                    hex = output.hex)
    }

    val wtxidOpt = tx match {
      case _: NonWitnessTransaction => None
      case wtx: WitnessTransaction  => Some(wtx.wTxIdBE)
    }

    val serializedTx = SerializedTransaction(txid = tx.txIdBE,
                                             wtxid = wtxidOpt,
                                             version = tx.version,
                                             size = tx.byteSize,
                                             vsize = tx.vsize,
                                             weight = tx.weight,
                                             locktime = tx.lockTime,
                                             vin = inputs,
                                             vout = outputs)

    val json = Json.toJson(serializedTx)
    val strWithEscaped = Json.prettyPrint(json)

    var escaped: Boolean = false

    strWithEscaped
      .map {
        case '\\' =>
          escaped = true
          '\\'
        case 'n' if escaped =>
          escaped = false
          '\n'
        case char =>
          escaped = false
          char
      }
      .filterNot(_ == '\\')
  }

  implicit val byteVectorWrites: Writes[ByteVector] =
    Writes[ByteVector](bytes => JsString(bytes.toHex))
  implicit val ecDigitalSignatureWrites: Writes[ECDigitalSignature] =
    Writes[ECDigitalSignature](sig => JsString(sig.hex))
  implicit val ecPublicKeyWrites: Writes[ECPublicKey] =
    Writes[ECPublicKey](pubKey => JsString(pubKey.hex))
  implicit val scriptTokenWrites: Writes[ScriptToken] =
    Writes[ScriptToken](token => JsString(tokenToString(token)))
  implicit val doubleSha256DigestBEWrites: Writes[DoubleSha256DigestBE] =
    Writes[DoubleSha256DigestBE](hash => JsString(hash.hex))
  implicit val uInt32Writes: Writes[UInt32] =
    Writes[UInt32](num => JsNumber(num.toLong))
  implicit val int32Writes: Writes[Int32] =
    Writes[Int32](num => JsNumber(num.toLong))
  implicit val scriptPubKeyWrites: Writes[ScriptPubKey] =
    Writes[ScriptPubKey](spk => Json.toJson(spk.asm))

  implicit val serializedTransactionWitnessWrites: Writes[
    SerializedTransactionWitness] = Json.writes[SerializedTransactionWitness]
  implicit val serializedTransactionInputWrites: Writes[
    SerializedTransactionInput] = Json.writes[SerializedTransactionInput]
  implicit val serializedTransactionOutputWrites: Writes[
    SerializedTransactionOutput] = Json.writes[SerializedTransactionOutput]
  implicit val serializedTransactionWrites: Writes[SerializedTransaction] =
    Json.writes[SerializedTransaction]
}
