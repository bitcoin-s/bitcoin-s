package org.scalacoin.script.flag

import org.scalacoin.util.BitcoinSUtil

/**
 * Created by chris on 3/23/16.
 * This represents all of the script flags found inside of
 * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.h#L31
 * these flags indicate how to evaluate a certain script
 */
sealed trait ScriptFlag {
  def flag : Int
}


case object ScriptVerifyNone extends ScriptFlag {
  def flag = 0
}

/**
 * Evaluate P2SH subscripts (softfork safe, BIP16).
 */
case object ScriptVerifyP2SH extends ScriptFlag {
  def flag = 1
}

/**
 * Passing a non-strict-DER signature or one with undefined hashtype to a checksig operation causes script failure.
 * Evaluating a pubkey that is not (0x04 + 64 bytes) or (0x02 or 0x03 + 32 bytes) by checksig causes script failure.
 * (softfork safe, but not used or intended as a consensus rule).
 */
case object ScriptVerifyStrictEnc extends ScriptFlag {
  def flag = 1 << 1
}

/**
 * Passing a non-strict-DER signature to a checksig operation causes script failure (softfork safe, BIP62 rule 1)
 */
case object ScriptVerifyDerSig extends ScriptFlag {
  def flag = 1 << 2
}

/**
 * Passing a non-strict-DER signature or one with S > order/2 to a checksig operation causes script failure
 * (softfork safe, BIP62 rule 5).
 */
case object ScriptVerifyLowS extends ScriptFlag {
  def flag = 1 << 3
}

/**
 * Verify dummy stack item consumed by CHECKMULTISIG is of zero-length (softfork safe, BIP62 rule 7)
 */
case object ScriptVerifyNullDummy extends ScriptFlag {
  def flag = 1 << 4
}

/**
 * Using a non-push operator in the scriptSig causes script failure (softfork safe, BIP62 rule 2).
 */
case object ScriptVerifySigPushOnly extends ScriptFlag {
  def flag = 1 << 5
}

/**
 * Require minimal encodings for all push operations (OP_0... OP_16, OP_1NEGATE where possible, direct
 * pushes up to 75 bytes, OP_PUSHDATA up to 255 bytes, OP_PUSHDATA2 for anything larger). Evaluating
 * any other push causes the script to fail (BIP62 rule 3).
 * In addition, whenever a stack element is interpreted as a number, it must be of minimal length (BIP62 rule 4).
 * (softfork safe)
 */
case object ScriptVerifyMinimalData extends ScriptFlag {
  def flag = 1 << 6
}


/**
 * Discourage use of NOPs reserved for upgrades (NOP1-10)
 * Provided so that nodes can avoid accepting or mining transactions
 * containing executed NOP's whose meaning may change after a soft-fork,
 * thus rendering the script invalid; with this flag set executing
 * discouraged NOPs fails the script. This verification flag will never be
 * a mandatory flag applied to scripts in a block. NOPs that are not
 * executed, e.g.  within an unexecuted IF ENDIF block, are *not* rejected.
 */
case object ScriptVerifyDiscourageUpgradableNOPs extends ScriptFlag {
  def flag = 1 << 7
}

/**
 * Require that only a single stack element remains after evaluation. This changes the success criterion from
 * "At least one stack element must remain, and when interpreted as a boolean, it must be true" to
 * "Exactly one stack element must remain, and when interpreted as a boolean, it must be true".
 * (softfork safe, BIP62 rule 6)
 * Note: CLEANSTACK should never be used without P2SH.
 */
case object ScriptVerifyCleanStack extends ScriptFlag {
  def flag = 1 << 8
}

/**
 * See BIP65 for details
 */
case object ScriptVerifyCheckLocktimeVerify extends ScriptFlag {
  def flag = 1 << 9
}

/**
 * See BIP112 for details
 */
case object ScriptVerifyCheckSequenceVerify extends ScriptFlag {
  def flag = 1 << 10
}


