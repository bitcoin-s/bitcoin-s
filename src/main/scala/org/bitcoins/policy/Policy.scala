package org.bitcoins.policy

import org.bitcoins.script.flag._

/**
 * Created by chris on 4/6/16.
 * Mimics the policy files found in bitcoin core
 * https://github.com/bitcoin/bitcoin/blob/master/src/policy/policy.h
 */
trait Policy {


  /**
   * Mandatory script verification flags that all new blocks must comply with for
   * them to be valid. (but old blocks may not comply with) Currently just P2SH,
   * but in the future other flags may be added, such as a soft-fork to enforce
   * strict DER encoding.
   *
   * Failing one of these tests may trigger a DoS ban - see CheckInputs() for
   * details.
   */
  def mandatoryScriptVerifyFlags  : Seq[ScriptFlag] = Seq(ScriptVerifyP2SH)
  /**
   * The default script verify flags used to validate the blockchain
   * and bitcoin transactions
 *
   * @return
   */
  def standardScriptVerifyFlags : Seq[ScriptFlag] = Seq(ScriptVerifyDerSig, ScriptVerifyStrictEnc,
    ScriptVerifyMinimalData, ScriptVerifyNullDummy, ScriptVerifyDiscourageUpgradableNOPs,
    ScriptVerifyCleanStack, ScriptVerifyCheckLocktimeVerify, ScriptVerifyCheckSequenceVerify,
    ScriptVerifyLowS)


  /**
   * Returns the non mandatory script verify flags
 *
   * @return
   */
  def standardNotMandatoryScriptVerifyFlags : Seq[ScriptFlag] = {
    (standardScriptVerifyFlags.diff(mandatoryScriptVerifyFlags))
  }
}

object Policy extends Policy
