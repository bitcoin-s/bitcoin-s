package org.bitcoins.script.flag

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

  /**
   * Checks if we are required to check for strict encoding
   * @param flags
   * @return
   */
  def requireStrictEncoding(flags : Seq[ScriptFlag]) : Boolean = flags.contains(ScriptVerifyStrictEnc)

  /**
   * Checks if the script flag for checklocktimeverify is enabled
   * @param flags
   * @return
   */
  def checkLockTimeVerifyEnabled(flags : Seq[ScriptFlag]) : Boolean = {
    flags.contains(ScriptVerifyCheckLocktimeVerify)
  }

  /**
   * Checks to see if the script flag is set to discourage NOPs that are not in use
   * NOPs are used by soft forks to repurpose NOPs to actual functionality such as checklocktimeverify
   * See BIP65 for an example of this
   * @param flags
   * @return
   */
  def discourageUpgradableNOPs(flags : Seq[ScriptFlag]) : Boolean = {
    flags.contains(ScriptVerifyDiscourageUpgradableNOPs)
  }

  /**
   * Checks to see if the script flag is set to require minimal push operations
   * see BIP62
   * @param flags
   * @return
   */
  def requireMinimalData(flags : Seq[ScriptFlag]) : Boolean = flags.contains(ScriptVerifyMinimalData)
}


object ScriptFlagUtil extends ScriptFlagUtil
