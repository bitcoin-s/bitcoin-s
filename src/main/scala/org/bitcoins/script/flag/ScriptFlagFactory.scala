package org.bitcoins.script.flag

/**
 * Created by chris on 3/23/16.
 * Trait used to create a script flag used to evaluate scripts in a
 * certain way
 */
trait ScriptFlagFactory {

  /**
   * All the script flags found inside of bitcoin core
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.h#L31
   * @return
   */
  private def flags = Seq(ScriptVerifyNone, ScriptVerifyP2SH, ScriptVerifyStrictEnc,
    ScriptVerifyDerSig, ScriptVerifyLowS, ScriptVerifySigPushOnly, ScriptVerifyMinimalData,
    ScriptVerifyNullDummy, ScriptVerifyDiscourageUpgradableNOPs, ScriptVerifyCleanStack,
    ScriptVerifyCheckLocktimeVerify, ScriptVerifyCheckSequenceVerify)

  /**
   * Takes in a string and tries to match it with a script flag
   * @param str the string to try and match with a script flag
   * @return the script flag if one is found else none
   */
  def fromString(str : String) : Option[ScriptFlag] = {
    flags.find(_.name == str)
  }


  /**
   * Parses the given list into script flags
   * the strings that do not match a script flag are discarded
   * @param list the list of strings to parse into script flags
   * @return the sequence of script flags
   */
  def fromList(list : Seq[String]) : Seq[ScriptFlag] = {
    list.map(fromString(_)).flatten
  }

  /**
   * Parses a list of script flags that is separated by commas
   * @param str a string in the format flag1,flag2,...,flagN
   * @return the sequence of script flags
   */
  def fromList(str : String) : Seq[ScriptFlag] = {
    fromList(str.split(","))
  }

  /**
   * Empty script flag
   * @return
   */
  def empty : Seq[ScriptFlag] = Seq()
}

object ScriptFlagFactory extends ScriptFlagFactory