package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{ECPublicKey, Sha256Digest, Sha256Hash160Digest}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, CryptoUtil}

/**
  * Created by chris on 11/10/16.
  * The version of the [[WitnessScriptPubKey]], this indicates how a [[ScriptWitness]] is rebuilt
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#witness-program]]
  */
sealed trait WitnessVersion extends BitcoinSLogger {
  /** Rebuilds the full script from the given witness and [[ScriptPubKey]]
    * Either returns the stack and the [[ScriptPubKey]] it needs to be executed against or
    * the [[ScriptError]] that was encountered while rebuilding the witness*/
  def rebuild(scriptWitness: ScriptWitness, witnessProgram: Seq[ScriptToken]): Either[(Seq[ScriptToken], ScriptPubKey),ScriptError]
}

case object WitnessVersion0 extends WitnessVersion {

  /** Rebuilds a witness version 0 program, see BIP141 */
  override def rebuild(scriptWitness: ScriptWitness, witnessProgram: Seq[ScriptToken]): Either[(Seq[ScriptToken], ScriptPubKey),ScriptError] = {
    val programBytes = witnessProgram.flatMap(_.bytes)
    programBytes.size match {
      case 20 =>
        //p2wpkh
        if (scriptWitness.stack.size != 2) Right(ScriptErrorWitnessProgramMisMatch)
        else {
          val hash = Sha256Hash160Digest(programBytes)
          Left((scriptWitness.stack.map(ScriptConstant(_)), P2PKHScriptPubKey(hash)))
        }
      case 32 =>
        logger.info("Script witness stack: " + scriptWitness)
        //p2wsh
        if (scriptWitness.stack.isEmpty) Right(ScriptErrorWitnessProgramWitnessEmpty)
        else {
          //need to check if the hashes match
          val stackTop = scriptWitness.stack.head
          val stackHash = CryptoUtil.sha256(stackTop)
          logger.debug("Stack top: " + BitcoinSUtil.encodeHex(stackTop))
          logger.debug("Stack hash: " + stackHash)
          logger.debug("Witness program: " + witnessProgram)
          if (stackHash != Sha256Digest(witnessProgram.head.bytes)) Right(ScriptErrorWitnessProgramMisMatch)
          else {
            val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(stackTop)
            val scriptPubKey = ScriptPubKey(compactSizeUInt.bytes ++ stackTop)
            logger.debug("Script pub key for p2wsh: " + scriptPubKey.asm)
            val stack = scriptWitness.stack.tail.map(ScriptConstant(_))
            Left(stack, scriptPubKey)
          }
        }
      case _ =>
        logger.error("Invalid witness program length for witness version 0, got: " + programBytes.size)
        logger.error("Witness: " + scriptWitness)
        logger.error("Witness program: " + witnessProgram)
        //witness version 0 programs need to be 20 bytes or 32 bytes in size
        Right(ScriptErrorWitnessProgramWrongLength)
    }
  }
}

/** The witness version that represents all witnesses that have not been allocated yet */
case object UnassignedWitness extends WitnessVersion {
  override def rebuild(scriptWitness: ScriptWitness, witnessProgram: Seq[ScriptToken]): Either[(Seq[ScriptToken], ScriptPubKey),ScriptError] =
    Right(ScriptErrorDiscourageUpgradeableWitnessProgram)
}

object WitnessVersion {

  def apply(scriptNumberOp: ScriptNumberOperation): WitnessVersion = scriptNumberOp match {
    case OP_0 | OP_FALSE => WitnessVersion0
    case OP_1 | OP_TRUE | OP_2 | OP_3 | OP_4 | OP_5 | OP_6 | OP_7 | OP_8
      | OP_9 | OP_10 | OP_11 | OP_12 | OP_13 | OP_14 | OP_15 | OP_16  => UnassignedWitness
    case OP_1NEGATE => throw new IllegalArgumentException("OP_1NEGATE is not a valid witness version")
  }

  def apply(token: ScriptToken): WitnessVersion = token match {
    case scriptNumberOp : ScriptNumberOperation => WitnessVersion(scriptNumberOp)
    case _ : ScriptConstant | _ : ScriptNumber | _ : ScriptOperation =>
      throw new IllegalArgumentException("We can only have witness version that is a script number operation, i.e OP_0 through OP_16")
  }
}
