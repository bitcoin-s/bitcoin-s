package org.scalacoin.script.flag

/**
 * Created by chris on 3/23/16.
 */
trait ScriptFlagFactory {

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
}

object ScriptFlagFactory extends ScriptFlagFactory