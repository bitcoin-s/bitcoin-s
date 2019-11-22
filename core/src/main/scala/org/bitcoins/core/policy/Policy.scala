package org.bitcoins.core.policy

import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.script.flag._
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}

/**
  * Created by chris on 4/6/16.
  * Mimics the policy files found in
  * [[https://github.com/bitcoin/bitcoin/blob/master/src/policy/policy.h Bitcoin Core]]
  */
sealed abstract class Policy {

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

  /**
    * The default script verify flags used to validate the blockchain
    * and bitcoin transactions
    */
  def standardScriptVerifyFlags: Seq[ScriptFlag] =
    mandatoryScriptVerifyFlags ++ Seq(
      ScriptVerifyDerSig,
      ScriptVerifyStrictEnc,
      ScriptVerifyMinimalData,
      ScriptVerifyDiscourageUpgradableNOPs,
      ScriptVerifyCleanStack,
      ScriptVerifyCheckLocktimeVerify,
      ScriptVerifyCheckSequenceVerify,
      ScriptVerifyLowS,
      ScriptVerifyWitness,
      ScriptVerifyMinimalIf,
      ScriptVerifyNullFail,
      ScriptVerifyNullDummy,
      ScriptVerifyWitnessPubKeyType,
      ScriptVerifyDiscourageUpgradableWitnessProgram
    )

  def standardFlags = standardScriptVerifyFlags

  /** The number of confirmations for a payment to be considered as accepted */
  def confirmations: Long = 6

  /**
    * Minimum amount of [[org.bitcoins.core.currency.CurrencyUnit CurrencyUnit]]
    * lock in a Channel
    * Currently set to 1 mBTC
    *
    * TODO: Remove this?
    */
  def minChannelAmount: CurrencyUnit = CurrencyUnits.oneMBTC

  /** The minimum amount of satoshis we can spend to an output */
  def dustThreshold: CurrencyUnit = Satoshis(1000)

  /** A default fee to use per byte on the bitcoin network */
  def defaultFee: CurrencyUnit = Satoshis(50)

  /** A default fee to use per byte on the bitcoin network */
  def defaultFeeRate: FeeUnit = SatoshisPerVirtualByte(defaultFee)

  /** Max fee for a transaction is set to 10 mBTC right now */
  def maxFee: CurrencyUnit = Satoshis(10) * CurrencyUnits.oneMBTC

  def sequence: UInt32 = UInt32.zero
}

object Policy extends Policy
