package org.bitcoins.core.protocol.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto.{CryptoUtil, Sha256Digest, Sha256Hash160Digest}

/** Created by chris on 11/10/16.
  * The version of the [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]],
  * this indicates how a [[org.bitcoins.core.protocol.script.ScriptWitness ScriptWitness]] is rebuilt.
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#witness-program BIP141]]
  */
sealed trait WitnessVersion {

  /** Rebuilds the full script from the given witness and [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
    * Either returns the [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
    * it needs to be executed against or the [[ScriptError]] that was encountered when
    * building the rebuild the scriptpubkey from the witness
    */
  def rebuild(
      scriptWitness: ScriptWitness,
      witnessSPK: WitnessScriptPubKey): Either[ScriptError, ScriptPubKey]

  def version: ScriptNumberOperation
}

case object WitnessVersion0 extends WitnessVersion {

  /** Rebuilds a witness version 0 SPK program, see BIP141 */
  override def rebuild(
      scriptWitness: ScriptWitness,
      witnessSPK: WitnessScriptPubKey): Either[ScriptError, ScriptPubKey] = {
    val witnessProgram = witnessSPK.witnessProgram
    val programBytes = BytesUtil.toByteVector(witnessProgram)
    programBytes.size match {
      case 20 =>
        //p2wpkh
        val hash = Sha256Hash160Digest(programBytes)
        Right(P2PKHScriptPubKey(hash))
      case 32 =>
        //p2wsh
        if (scriptWitness.stack.isEmpty) {
          Left(ScriptErrorWitnessProgramWitnessEmpty)
        } else {
          //need to check if the hashes match
          val stackTop = scriptWitness.stack.head
          val stackHash = CryptoUtil.sha256(stackTop)
          val witnessHash = Sha256Digest(witnessProgram.head.bytes)
          if (stackHash != witnessHash) {
            Left(ScriptErrorWitnessProgramMisMatch)
          } else {
            val compactSizeUInt =
              CompactSizeUInt.calculateCompactSizeUInt(stackTop)
            val scriptPubKey = ScriptPubKey(compactSizeUInt.bytes ++ stackTop)
            Right(scriptPubKey)
          }
        }
      case _ =>
        //witness version 0 programs need to be 20 bytes or 32 bytes in size
        Left(ScriptErrorWitnessProgramWrongLength)
    }
  }

  override val version: OP_0.type = OP_0
}

case object WitnessVersion1 extends WitnessVersion {

  override def rebuild(
      scriptWitness: ScriptWitness,
      witnessSPK: WitnessScriptPubKey): Either[ScriptError, ScriptPubKey] = {
    require(witnessSPK.isInstanceOf[TaprootScriptPubKey],
            s"WitnessScriptPubKey must be a taproot spk, got=${witnessSPK}")
    val taprootSPK = witnessSPK.asInstanceOf[TaprootScriptPubKey]
    val witnessProgram = taprootSPK.witnessProgram
    val programBytes = BytesUtil.toByteVector(witnessProgram)
    programBytes.size match {
      case 32 =>
        //p2tr
        if (scriptWitness.stack.isEmpty) {
          Left(ScriptErrorWitnessProgramWitnessEmpty)
        } else {
          val rebuiltSPK = scriptWitness match {
            case _: TaprootKeyPath =>
              Right(witnessSPK)
            case sp: TaprootScriptPath =>
              Right(sp.script)
            case _: TaprootUnknownPath =>
              Right(witnessSPK)
            case w @ (EmptyScriptWitness | _: P2WPKHWitnessV0 |
                _: P2WSHWitnessV0) =>
              sys.error(
                s"Cannot rebuild witnessv1 with a non v1 witness, got=$w")
          }
          rebuiltSPK
        }
      case _ =>
        //witness version 1 programs need to be 32 bytes in size
        //this is technically wrong as this is dependent on a policy flag
        //this should only error when the DISCOURAGE_UPGRADABLE_WITNESS policy flag is set
        //else it should succeed as to maintain future soft fork compatability
        //this will get addressed on a future PR as I implement test cases
        //in the interpreter
        Left(ScriptErrorDiscourageUpgradeableWitnessProgram)
    }
  }

  override val version: OP_1.type = OP_1
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
      witnessSPK: WitnessScriptPubKey): Either[ScriptError, ScriptPubKey] = {
    Left(ScriptErrorDiscourageUpgradeableWitnessProgram)
  }
}

object WitnessVersion {

  def apply(scriptNumberOp: ScriptNumberOperation): WitnessVersion =
    scriptNumberOp match {
      case OP_0 | OP_FALSE => WitnessVersion0
      case OP_1 | OP_TRUE  => WitnessVersion1
      case x @ (OP_2 | OP_3 | OP_4 | OP_5 | OP_6 | OP_7 | OP_8 | OP_9 | OP_10 |
          OP_11 | OP_12 | OP_13 | OP_14 | OP_15 | OP_16) =>
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
