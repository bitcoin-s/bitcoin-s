package org.bitcoins.core.protocol.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant._
import org.bitcoins.core.util.{BitcoinSLogger, BytesUtil}
import org.bitcoins.crypto.{CryptoUtil, Sha256Digest, Sha256Hash160Digest}

import scala.util.{Failure, Success, Try}

/** Created by chris on 11/10/16.
  * The version of the [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]],
  * this indicates how a [[org.bitcoins.core.protocol.script.ScriptWitness ScriptWitness]] is rebuilt.
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#witness-program BIP141]]
  */
sealed trait WitnessVersion extends BitcoinSLogger {

  /** Rebuilds the full script from the given witness and [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
    * Either returns the [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
    * it needs to be executed against or the failure that was encountered while rebuilding the witness
    */
  def rebuild(
      scriptWitness: ScriptWitness,
      witnessProgram: Seq[ScriptToken]): Try[ScriptPubKey]

  def version: ScriptNumberOperation
}

case object WitnessVersion0 extends WitnessVersion {

  /** Rebuilds a witness version 0 SPK program, see BIP141 */
  override def rebuild(
      scriptWitness: ScriptWitness,
      witnessProgram: Seq[ScriptToken]): Try[ScriptPubKey] = {
    val programBytes = BytesUtil.toByteVector(witnessProgram)
    programBytes.size match {
      case 20 =>
        //p2wpkh
        val hash = Sha256Hash160Digest(programBytes)
        Success(P2PKHScriptPubKey(hash))
      case 32 =>
        //p2wsh
        if (scriptWitness.stack.isEmpty)
          Failure(
            new IllegalArgumentException(
              "P2WSH cannot be rebuilt without redeem script"))
        else {
          //need to check if the hashes match
          val stackTop = scriptWitness.stack.head
          val stackHash = CryptoUtil.sha256(stackTop)
          val witnessHash = Sha256Digest(witnessProgram.head.bytes)
          if (stackHash != witnessHash) {
            logger.debug(
              "Witness hashes did not match Stack hash: " + stackHash)
            logger.debug("Witness program: " + witnessProgram)
            Failure(new IllegalArgumentException(
              s"Witness hash $witnessHash did not match stack hash $stackHash"))
          } else {
            val compactSizeUInt =
              CompactSizeUInt.calculateCompactSizeUInt(stackTop)
            val scriptPubKey = ScriptPubKey(compactSizeUInt.bytes ++ stackTop)
            Success(scriptPubKey)
          }
        }
      case _ =>
        logger.error(
          "Invalid witness program length for witness version 0, got: " + programBytes.size)
        logger.error("Witness: " + scriptWitness)
        logger.error("Witness program: " + witnessProgram)
        //witness version 0 programs need to be 20 bytes or 32 bytes in size
        Failure(new IllegalArgumentException(
          s"Witness program had invalid length (${programBytes.length}) for version 0, must be 20 or 30: $witnessProgram"))
    }
  }

  override def version = OP_0
}

/** The witness version that represents all witnesses that have not been allocated yet */
case class UnassignedWitness(version: ScriptNumberOperation)
    extends WitnessVersion {
  require(
    WitnessScriptPubKey.unassignedWitVersions.contains(version),
    "Cannot created an unassigend witness version from one that is assigned already, got: " + version
  )

  override def rebuild(
      scriptWitness: ScriptWitness,
      witnessProgram: Seq[ScriptToken]): Try[ScriptPubKey] =
    Failure(
      new UnsupportedOperationException(
        s"Rebuilding is not defined for version $version yet."))
}

object WitnessVersion {

  def apply(scriptNumberOp: ScriptNumberOperation): WitnessVersion =
    scriptNumberOp match {
      case OP_0 | OP_FALSE => WitnessVersion0
      case x @ (OP_1 | OP_TRUE | OP_2 | OP_3 | OP_4 | OP_5 | OP_6 | OP_7 |
          OP_8 | OP_9 | OP_10 | OP_11 | OP_12 | OP_13 | OP_14 | OP_15 |
          OP_16) =>
        UnassignedWitness(x)
      case OP_1NEGATE =>
        throw new IllegalArgumentException(
          "OP_1NEGATE is not a valid witness version")
    }

  def apply(token: ScriptToken): WitnessVersion =
    token match {
      case scriptNumberOp: ScriptNumberOperation =>
        WitnessVersion(scriptNumberOp)
      case _: ScriptConstant | _: ScriptNumber | _: ScriptOperation =>
        throw new IllegalArgumentException(
          "We can only have witness version that is a script number operation, i.e OP_0 through OP_16")
    }

  def apply(int: Int): Option[WitnessVersion] =
    ScriptNumberOperation.fromNumber(int).map(WitnessVersion(_))

}
