package org.bitcoins.core.policy

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.script.flag._

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
  def mandatoryScriptVerifyFlags: Seq[ScriptFlag] = Seq(ScriptVerifyP2SH)

  /** The default script verify flags used to validate the blockchain
   * and bitcoin transactions */
  def standardScriptVerifyFlags : Seq[ScriptFlag] = mandatoryScriptVerifyFlags ++ Seq(ScriptVerifyDerSig, ScriptVerifyStrictEnc,
    ScriptVerifyMinimalData, ScriptVerifyNullDummy, ScriptVerifyDiscourageUpgradableNOPs,
    ScriptVerifyCleanStack, ScriptVerifyCheckLocktimeVerify, ScriptVerifyCheckSequenceVerify,
    ScriptVerifyLowS, ScriptVerifyWitness, ScriptVerifyMinimalIf, ScriptVerifyNullFail,
    ScriptVerifyNullDummy, ScriptVerifyWitnessPubKeyType, ScriptVerifyDiscourageUpgradableWitnessProgram)

  /** The number of confirmations for a payment to be considered as accepted */
  def confirmations: Long = 6

  def minPaymentChannelAmount: CurrencyUnit = Satoshis(Int64(10000))

}

object Policy extends Policy
