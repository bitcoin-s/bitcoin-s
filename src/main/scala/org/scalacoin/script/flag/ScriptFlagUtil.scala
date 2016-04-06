package org.scalacoin.script.flag

/**
 * Created by chris on 4/6/16.
 */
trait ScriptFlagUtil {

  /**
   * Checks if the strict encoding is required in the set of flags
   * given to us
   * @param flags
   * @return
   */
  def requiresStrictDerEncoding(flags : Seq[ScriptFlag]) : Boolean = {
    flags.contains(ScriptVerifyDerSig) || flags.contains(ScriptVerifyStrictEnc)
  }
}


object ScriptFlagUtil extends ScriptFlagUtil
