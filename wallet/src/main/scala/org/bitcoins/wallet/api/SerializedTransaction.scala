package org.bitcoins.wallet.api

import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECDigitalSignature,
  ECPublicKey
}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{
  EmptyScriptWitness,
  P2WPKHWitnessV0,
  P2WSHWitnessV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  WitnessTransaction
}
import org.bitcoins.core.script.constant.{
  ScriptConstant,
  ScriptNumberOperation,
  ScriptToken
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

  def decodeRawTransaction(tx: Transaction): String = {
    def tokenToString(token: ScriptToken): String = {
      token match {
        case numOp: ScriptNumberOperation => numOp.toString
        case constOp: ScriptConstant      => constOp.bytes.toString
        case otherOp                      => otherOp.toString
      }
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

    val inputs = tx.inputs.toVector.zipWithIndex.map {
      case (input, index) =>
        val witnessOpt = tx match {
          case _: BaseTransaction => None
          case wtx: WitnessTransaction =>
            wtx.witness.witnesses(index) match {
              case EmptyScriptWitness => None
              case p2wpkh: P2WPKHWitnessV0 =>
                Some(
                  SerializedTransactionWitness(p2wpkh.hex,
                                               Some("P2WPKH"),
                                               None,
                                               Some(p2wpkh.pubKey),
                                               Some(p2wpkh.signature),
                                               None))
              case p2wsh: P2WSHWitnessV0 =>
                Some(
                  SerializedTransactionWitness(
                    p2wsh.hex,
                    Some("P2WSH"),
                    Some(p2wsh.redeemScript.asm.toVector),
                    None,
                    None,
                    Some(p2wsh.stack.toVector.tail)))
            }
        }

        SerializedTransactionInput(input.previousOutput.txIdBE,
                                   input.hex,
                                   input.previousOutput.vout,
                                   input.scriptSignature.asm.toVector,
                                   witnessOpt,
                                   input.sequence)
    }

    val outputs = tx.outputs.toVector.zipWithIndex.map {
      case (output, index) =>
        SerializedTransactionOutput(output.value.toBigDecimal,
                                    UInt32(index),
                                    output.scriptPubKey,
                                    output.hex)
    }

    val wtxidOpt = tx match {
      case _: BaseTransaction      => None
      case wtx: WitnessTransaction => Some(wtx.wTxIdBE)
    }

    val serializedTx = SerializedTransaction(tx.txIdBE,
                                             wtxidOpt,
                                             tx.version,
                                             tx.size,
                                             tx.vsize,
                                             tx.weight,
                                             tx.lockTime,
                                             inputs,
                                             outputs)

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
}
