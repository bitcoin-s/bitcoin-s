package org.bitcoins.core.script.flag

/** Created by chris on 4/6/16.
  */
trait ScriptFlagUtil {

  /** Checks if the strict encoding is required in the set of flags
    * given to us
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#DER_encoding
    * @param flags
    * @return
    */
  def requiresStrictDerEncoding(flags: Seq[ScriptFlag]): Boolean = {
    flags.contains(ScriptVerifyDerSig) || flags.contains(ScriptVerifyStrictEnc)
  }

  /** Checks if we are required to check for strict encoding
    * @param flags
    * @return
    */
  def requireStrictEncoding(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyStrictEnc)

  /** Checks if the script flag for checklocktimeverify is enabled
    * @param flags
    * @return
    */
  def checkLockTimeVerifyEnabled(flags: Seq[ScriptFlag]): Boolean = {
    flags.contains(ScriptVerifyCheckLocktimeVerify)
  }

  /** Checks if the p2sh flag is enabled
    * @param flags
    * @return
    */
  def p2shEnabled(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyP2SH)

  /** Checks if the script flag for checksequenceverify is enabled
    * @param flags
    * @return
    */
  def checkSequenceVerifyEnabled(flags: Seq[ScriptFlag]): Boolean = {
    flags.contains(ScriptVerifyCheckSequenceVerify)
  }

  /** Checks to see if the script flag is set to discourage NOPs that are not in use
    * NOPs are used by soft forks to repurpose NOPs to actual functionality such as checklocktimeverify
    * See BIP65 for an example of this seq
    * @param flags
    * @return
    */
  def discourageUpgradableNOPs(flags: Seq[ScriptFlag]): Boolean = {
    flags.contains(ScriptVerifyDiscourageUpgradableNOPs)
  }

  /** Checks to see if the script flag is set to require minimal push operations
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Push_operators
    * @param flags
    * @return
    */
  def requireMinimalData(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyMinimalData)

  /** Checks to see if the script flag is set to require low s values in digital signatures
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
    * @param flags
    * @return
    */
  def requireLowSValue(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyLowS)

  /** Checks to see if the script flag is set to require we only have push operations inside of a scriptSig
    * @param flags
    * @return
    */
  def requirePushOnly(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifySigPushOnly)

  /** Checks to see if the script flag is set to require that we need a NULLDUMMY to be OP_0 for
    * OP_CHECKMULTISIG operations
    * @param flags
    * @return
    */
  def requireNullDummy(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyNullDummy)

  /** Checks to see if we have segwit enabled */
  def segWitEnabled(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyWitness)

  def discourageUpgradableWitnessProgram(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyDiscourageUpgradableWitnessProgram)

  def requireScriptVerifyNullFail(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyNullFail)

  def requireScriptVerifyWitnessPubKeyType(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyWitnessPubKeyType)

  /** Requires that the argument to OP_IF/OP_NOTIF be minimally encoded
    * See: https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2016-August/013014.html
    */
  def minimalIfEnabled(flags: Seq[ScriptFlag]): Boolean =
    flags.contains(ScriptVerifyMinimalIf)

  def taprootEnabled(flags: Seq[ScriptFlag]): Boolean = {
    flags.contains(ScriptVerifyTaproot)
  }

  def discourageUpgradablePublicKey(flags: Seq[ScriptFlag]): Boolean = {
    flags.contains(ScriptVerifyDiscourageUpgradablePubKeyType)
  }

  def discourageOpSuccess(flags: Seq[ScriptFlag]): Boolean = {
    flags.contains(ScriptVerifyDiscourageOpSuccess)
  }
}

object ScriptFlagUtil extends ScriptFlagUtil
