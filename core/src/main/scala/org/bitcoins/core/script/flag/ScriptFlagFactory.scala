package org.bitcoins.core.script.flag

import org.bitcoins.crypto.StringFactory

/** Created by chris on 3/23/16.
  * Trait used to create a script flag used to evaluate scripts in a
  * certain way
  */
trait ScriptFlagFactory extends StringFactory[ScriptFlag] {

  /** All the [[ScriptFlag]]s found inside of bitcoin core
    * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.h#L31.
    */
  private val flags: Vector[ScriptFlag] =
    Vector(
      ScriptVerifyNone,
      ScriptVerifyP2SH,
      ScriptVerifyStrictEnc,
      ScriptVerifyDerSig,
      ScriptVerifyLowS,
      ScriptVerifySigPushOnly,
      ScriptVerifyMinimalData,
      ScriptVerifyNullDummy,
      ScriptVerifyDiscourageUpgradableNOPs,
      ScriptVerifyCleanStack,
      ScriptVerifyCheckLocktimeVerify,
      ScriptVerifyCheckSequenceVerify,
      ScriptVerifyWitness,
      ScriptVerifyDiscourageUpgradableWitnessProgram,
      ScriptVerifyMinimalIf,
      ScriptVerifyNullFail,
      ScriptVerifyWitnessPubKeyType,
      ScriptVerifyTaproot,
      ScriptVerifyDiscourageUpgradableTaprootVersion,
      ScriptVerifyDiscourageOpSuccess,
      ScriptVerifyDiscourageUpgradablePubKeyType
    )

  /** Takes in a string and tries to match it with a [[ScriptFlag]]. */
  override def fromStringOpt(str: String): Option[ScriptFlag] = {
    flags.find(_.name == str)
  }

  override def fromString(str: String): ScriptFlag = {
    fromStringOpt(str) match {
      case Some(flag) => flag
      case None       => sys.error(s"Could not find ScriptFlag for string=${str}")
    }
  }

  /** Parses the given list into[[ScriptFlag]]s
    * the strings that do not match a [[ScriptFlag]] are discarded.
    */
  def fromList(list: Seq[String]): Seq[ScriptFlag] = {
    list.flatMap(fromStringOpt(_))
  }

  /** Parses a list of [[ScriptFlag]]s that is separated by commas. */
  def fromList(str: String): Seq[ScriptFlag] = {
    fromList(str.split(",").toList)
  }

  /** Empty script flag. */
  def empty: Seq[ScriptFlag] = Nil
}

object ScriptFlagFactory extends ScriptFlagFactory
