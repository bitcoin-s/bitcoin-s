package org.bitcoins.commons.jsonmodels

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.script.constant.{
  ScriptConstant,
  ScriptNumberOperation,
  ScriptToken
}
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  ECDigitalSignature,
  ECPublicKeyBytes
}
import play.api.libs.json._
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
    vout: Vector[SerializedTransactionOutput]) {
  val toJson: JsValue = Json.toJson(this)
}

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
    pubKey: Option[ECPublicKeyBytes],
    signature: Option[ECDigitalSignature],
    stack: Option[Vector[ByteVector]])

case class SerializedTransactionOutput(
    value: BigDecimal,
    n: UInt32,
    scriptPubKey: Vector[ScriptToken],
    hex: String
)

object SerializedTransaction {

  def tokenToString(token: ScriptToken): String = {
    token match {
      case numOp: ScriptNumberOperation => numOp.toString
      case constOp: ScriptConstant      => constOp.bytes.toString
      case otherOp: ScriptToken         => otherOp.toString
    }
  }

  def decodeRawTransactionWitness(
      witness: ScriptWitness): Option[SerializedTransactionWitness] = {
    witness match {
      case EmptyScriptWitness => None
      case p2wpkh: P2WPKHWitnessV0 =>
        Some(
          SerializedTransactionWitness(hex = p2wpkh.hex,
                                       scriptType = Some("P2WPKH"),
                                       script = None,
                                       pubKey = Some(p2wpkh.pubKey),
                                       signature = Some(p2wpkh.signature),
                                       stack = None))
      case p2wsh: P2WSHWitnessV0 =>
        Some(
          SerializedTransactionWitness(hex = p2wsh.hex,
                                       scriptType = Some("P2WSH"),
                                       script =
                                         Some(p2wsh.redeemScript.asm.toVector),
                                       pubKey = None,
                                       signature = None,
                                       stack = Some(p2wsh.stack.toVector.tail)))
      case taprootWitness: TaprootWitness =>
        throw new UnsupportedOperationException(
          s"Taproot not supported, got=$taprootWitness")
    }
  }

  def decodeTransactionInput(
      input: TransactionInput,
      witnessOpt: Option[ScriptWitness]): SerializedTransactionInput = {
    val decodedWitnessOpt = witnessOpt.flatMap(decodeRawTransactionWitness)

    SerializedTransactionInput(
      txid = input.previousOutput.txIdBE,
      hex = input.hex,
      vout = input.previousOutput.vout,
      scriptSig = input.scriptSignature.asm.toVector,
      txinwitness = decodedWitnessOpt,
      sequence = input.sequence
    )
  }

  def decodeTransactionOutput(
      output: TransactionOutput,
      index: Int): SerializedTransactionOutput = {
    SerializedTransactionOutput(value = output.value.toBigDecimal,
                                n = UInt32(index),
                                scriptPubKey = output.scriptPubKey.asm.toVector,
                                hex = output.hex)
  }

  def decodeRawTransaction(tx: Transaction): SerializedTransaction = {
    val inputs = tx.inputs.toVector.zipWithIndex.map { case (input, index) =>
      val witnessOpt = tx match {
        case _: NonWitnessTransaction => None
        case wtx: WitnessTransaction =>
          Some(wtx.witness.witnesses(index))
      }

      decodeTransactionInput(input, witnessOpt)
    }

    val outputs = tx.outputs.toVector.zipWithIndex.map { case (output, index) =>
      decodeTransactionOutput(output, index)
    }

    val wtxIdOpt = tx match {
      case _: NonWitnessTransaction => None
      case wtx: WitnessTransaction  => Some(wtx.wTxIdBE)
    }

    SerializedTransaction(txid = tx.txIdBE,
                          wtxid = wtxIdOpt,
                          version = tx.version,
                          size = tx.byteSize,
                          vsize = tx.vsize,
                          weight = tx.weight,
                          locktime = tx.lockTime,
                          vin = inputs,
                          vout = outputs)
  }
}
