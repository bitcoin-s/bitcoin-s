package org.bitcoins.core.script.flag

/**
  * Created by chris on 3/23/16.
  * Trait used to create a script flag used to evaluate scripts in a
  * certain way
  */
trait ScriptFlagFactory {

  /**
    * All the [[ScriptFlag]]s found inside of bitcoin core
    * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.h#L31.
    */
  private def flags =
    Seq(
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
      ScriptVerifyWitnessPubKeyType
    )

  /** Takes in a string and tries to match it with a [[ScriptFlag]]. */
  def fromString(str: String): Option[ScriptFlag] = {
    flags.find(_.name == str)
  }

  /**
    * Parses the given list into[[ScriptFlag]]s
    * the strings that do not match a [[ScriptFlag]] are discarded.
    */
  def fromList(list: Seq[String]): Seq[ScriptFlag] = {
    list.flatMap(fromString(_))
  }

  /** Parses a list of [[ScriptFlag]]s that is separated by commas. */
  def fromList(str: String): Seq[ScriptFlag] = {
    fromList(str.split(",").toList)
  }

  /** Empty script flag. */
  def empty: Seq[ScriptFlag] = Nil
}

object ScriptFlagFactory extends ScriptFlagFactory
