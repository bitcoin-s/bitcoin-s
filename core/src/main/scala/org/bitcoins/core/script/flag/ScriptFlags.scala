package org.bitcoins.core.script.flag

/** Created by chris on 3/23/16.
  * This represents all of the script flags found inside of
  * https://github.com/bitcoin/bitcoin/blob/986003aff93c099c400c9285b4a2ed63f4b3f180/src/script/interpreter.h#L42
  * these flags indicate how to evaluate a certain script
  */
sealed trait ScriptFlag {

  /** The flag's representation represented as an integer. */
  def flag: Int

  /** The name of the flag as found in bitcoin core. */
  def name: String
}

case object ScriptVerifyNone extends ScriptFlag {
  override def flag = 0
  override def name = "NONE"
}

/** Evaluate P2SH subscripts (softfork safe, BIP16). */
case object ScriptVerifyP2SH extends ScriptFlag {
  override def flag = 1
  override def name = "P2SH"
}

/** Passing a non-strict-DER signature or one with undefined hashtype to a checksig operation causes script failure.
  * Evaluating a pubkey that is not (0x04 + 64 bytes) or (0x02 or 0x03 + 32 bytes) by checksig causes script failure.
  * (softfork safe, but not used or intended as a consensus rule).
  */
case object ScriptVerifyStrictEnc extends ScriptFlag {
  override def flag = 1 << 1
  override def name = "STRICTENC"
}

/** Passing a non-strict-DER signature to a checksig operation causes script failure (softfork safe, BIP62 rule 1). */
case object ScriptVerifyDerSig extends ScriptFlag {
  override def flag = 1 << 2
  override def name = "DERSIG"
}

/** Passing a non-strict-DER signature or one with S > order/2 to a checksig operation causes script failure
  * (softfork safe, BIP62 rule 5).
  */
case object ScriptVerifyLowS extends ScriptFlag {
  override def flag = 1 << 3
  override def name = "LOW_S"
}

/** Verify dummy stack item consumed by CHECKMULTISIG is of zero-length (softfork safe, BIP62 rule 7). */
case object ScriptVerifyNullDummy extends ScriptFlag {
  override def flag = 1 << 4
  override def name = "NULLDUMMY"
}

/** Using a non-push operator in the scriptSig causes script failure (softfork safe, BIP62 rule 2). */
case object ScriptVerifySigPushOnly extends ScriptFlag {
  override def flag = 1 << 5
  override def name = "SIGPUSHONLY"
}

/** Require minimal encodings for all push operations (OP_0... OP_16, OP_1NEGATE where possible, direct
  * pushes up to 75 bytes, OP_PUSHDATA up to 255 bytes, OP_PUSHDATA2 for anything larger). Evaluating
  * any other push causes the script to fail (BIP62 rule 3).
  * In addition, whenever a stack element is interpreted as a number, it must be of minimal length (BIP62 rule 4).
  * (softfork safe).
  */
case object ScriptVerifyMinimalData extends ScriptFlag {
  override def flag = 1 << 6
  override def name = "MINIMALDATA"
}

/** Discourage use of NOPs reserved for upgrades (NOP1-10)
  * Provided so that nodes can avoid accepting or mining transactions
  * containing executed NOP's whose meaning may change after a soft-fork,
  * thus rendering the script invalid; with this flag set executing
  * discouraged NOPs fails the script. This verification flag will never be
  * a mandatory flag applied to scripts in a block. NOPs that are not
  * executed, e.g.  within an unexecuted IF ENDIF block, are *not* rejected.
  */
case object ScriptVerifyDiscourageUpgradableNOPs extends ScriptFlag {
  override def flag = 1 << 7
  override def name = "DISCOURAGE_UPGRADABLE_NOPS"
}

/** Require that only a single stack element remains after evaluation. This changes the success criterion from
  * "At least one stack element must remain, and when interpreted as a boolean, it must be true" to
  * "Exactly one stack element must remain, and when interpreted as a boolean, it must be true".
  * (softfork safe, BIP62 rule 6)
  * Note: CLEANSTACK should never be used without P2SH.
  */
case object ScriptVerifyCleanStack extends ScriptFlag {
  override def flag = 1 << 8
  override def name = "CLEANSTACK"
}

/** See [[https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki]] for details. */
case object ScriptVerifyCheckLocktimeVerify extends ScriptFlag {
  override def flag = 1 << 9
  override def name = "CHECKLOCKTIMEVERIFY"
}

/** See https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki for details. */
case object ScriptVerifyCheckSequenceVerify extends ScriptFlag {
  override def flag = 1 << 10
  override def name = "CHECKSEQUENCEVERIFY"
}

/** Support segregated witness. */
case object ScriptVerifyWitness extends ScriptFlag {
  override def flag = 1 << 11
  override def name = "WITNESS"
}

/** Making v1-v16 witness program non-standard. */
case object ScriptVerifyDiscourageUpgradableWitnessProgram extends ScriptFlag {
  override def flag = 1 << 12
  override def name = "DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM"
}

/** Segwit script only: Require the argument of OP_IF/NOTIF to be exactly 0x01 or empty vector. */
case object ScriptVerifyMinimalIf extends ScriptFlag {
  override def flag = 1 << 13
  override def name = "MINIMALIF"
}

/** Signature(s) must be empty vector if an CHECK(MULTI)SIG operation failed. */
case object ScriptVerifyNullFail extends ScriptFlag {
  override def flag = 1 << 14
  override def name = "NULLFAIL"
}

/** Public keys in segregated witness scripts must be compressed. */
case object ScriptVerifyWitnessPubKeyType extends ScriptFlag {
  override def flag = 1 << 15
  override def name = "WITNESS_PUBKEYTYPE"
}

case object ScriptVerifyConstScriptCode extends ScriptFlag {
  override val flag: Int = 1 << 16
  override val name = "CONST_SCRIPTCODE" //not sure if this is right
}

case object ScriptVerifyTaproot extends ScriptFlag {
  override val flag: Int = 1 << 17
  override val name: String = "TAPROOT"
}

// Making unknown Taproot leaf versions non-standard
//
//SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_TAPROOT_VERSION = (1U << 18),
case object ScriptVerifyDiscourageUpgradableTaprootVersion extends ScriptFlag {
  override val flag: Int = 1 << 18
  override val name: String = "DISCOURAGE_UPGRADABLE_TAPROOT_VERSION"
}

// Making unknown OP_SUCCESS non-standard
//SCRIPT_VERIFY_DISCOURAGE_OP_SUCCESS = (1U << 19),
case object ScriptVerifyDiscourageOpSuccess extends ScriptFlag {
  override val flag: Int = 1 << 18
  override val name: String = "DISCOURAGE_OP_SUCCESS"
}

// Making unknown public key versions (in BIP 342 scripts) non-standard
//SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_PUBKEYTYPE = (1U << 20),
case object ScriptVerifyDiscourageUpgradablePubKeyType extends ScriptFlag {
  override val flag: Int = 1 << 19
  override val name: String = "DISCOURAGE_UPGRADABLE_PUBKEYTYPE"
}

object ScriptFlag {

  //what is this?
  //https://github.com/bitcoin/bitcoin/blob/3820090bd619ac85ab35eff376c03136fe4a9f04/src/test/script_tests.cpp#L1659
  val allConsensusFlags = Vector(
    ScriptVerifyP2SH,
    ScriptVerifyDerSig,
    ScriptVerifyNullDummy,
    ScriptVerifyCheckLocktimeVerify,
    ScriptVerifyCheckSequenceVerify,
    ScriptVerifyWitness,
    ScriptVerifyTaproot,
    ScriptVerifyDiscourageUpgradableTaprootVersion,
    ScriptVerifyDiscourageOpSuccess,
    ScriptVerifyDiscourageUpgradablePubKeyType
  )
}
