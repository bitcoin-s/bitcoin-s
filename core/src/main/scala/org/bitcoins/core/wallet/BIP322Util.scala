package org.bitcoins.core.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.crypto._

/** @see https://github.com/bitcoin/bips/blob/master/bip-0322.mediawiki */
trait BIP322Util {

  private def createToSpendTransaction(
      messageHash: Sha256Digest,
      messageChallenge: ScriptPubKey): Transaction = {
    val outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.max)
    val asm =
      Vector(OP_0, BytesToPushOntoStack(32), ScriptConstant(messageHash.bytes))
    val scriptSig = ScriptSignature.fromAsm(asm)
    val input = TransactionInput(outPoint, scriptSig, UInt32.zero)

    val output = TransactionOutput(Satoshis.zero, messageChallenge)

    BaseTransaction(Int32.zero, Vector(input), Vector(output), UInt32.zero)
  }

  def createToSignTransaction(
      message: String,
      messageChallenge: ScriptPubKey,
      version: Int32 = Int32.zero,
      lockTime: UInt32 = UInt32.zero,
      additionalInputs: Vector[TransactionInput] =
        Vector.empty): Transaction = {
    val messageHash = CryptoUtil.taggedSha256(message, "BIP0322-signed-message")
    val toSpend = createToSpendTransaction(messageHash, messageChallenge)

    val outPoint = TransactionOutPoint(toSpend.txId, UInt32.zero)
    val input = TransactionInput(outPoint, EmptyScriptSignature, UInt32.zero)

    val output =
      TransactionOutput(Satoshis.zero, ScriptPubKey.fromAsm(Vector(OP_RETURN)))

    BaseTransaction(version,
                    Vector(input) ++ additionalInputs,
                    Vector(output),
                    lockTime)
  }

  def createProofOfFundsTx(
      inputs: Vector[TransactionInput],
      message: String = "",
      version: Int32 = Int32.zero,
      lockTime: UInt32 = UInt32.zero): Transaction = {
    val challenge = ScriptPubKey.fromAsm(Vector(OP_TRUE))
    createToSignTransaction(message = message,
                            messageChallenge = challenge,
                            version = version,
                            lockTime = lockTime,
                            additionalInputs = inputs)
  }
}

object BIP322Util extends BIP322Util
