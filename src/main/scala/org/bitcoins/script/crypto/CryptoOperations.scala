package org.bitcoins.script.crypto

import org.bitcoins.script.ScriptOperationFactory
import org.bitcoins.script.constant.ScriptOperation

/**
 * Created by chris on 1/6/16.
 * Represents an operation where a cryptographic function is applied
 */
sealed trait CryptoOperation extends ScriptOperation

/**
 * Represents an operation where ECDSA signatures are evaluated
 */
sealed trait CryptoSignatureEvaluation extends CryptoOperation
/**
 * The input is hashed using RIPEMD-160.
 */
case object OP_RIPEMD160 extends CryptoOperation {
  override def opCode = 166
}

/**
 * The input is hashed using SHA-1.
 */
case object OP_SHA1 extends CryptoOperation {
  override def opCode = 167
}

/**
 * 	The input is hashed using SHA-256.
 */
case object OP_SHA256 extends CryptoOperation {
  override def opCode = 168
}

/**
 * The input is hashed twice: first with SHA-256 and then with RIPEMD-160.
 */
case object OP_HASH160 extends CryptoOperation {
  override def opCode = 169
}

/**
 * 	The input is hashed two times with SHA-256.
 */
case object OP_HASH256 extends CryptoOperation {
  override def opCode = 170
}

/**
 * All of the signature checking words will only match signatures to
 * the data after the most recently-executed OP_CODESEPARATOR.
 */
case object OP_CODESEPARATOR extends CryptoOperation {
  override def opCode = 171
}

/**
 * The entire transaction's outputs, inputs, and script
 * (from the most recently-executed OP_CODESEPARATOR to the end) are hashed.
 * The signature used by OP_CHECKSIG must be a valid signature for this hash and public key.
 * If it is, 1 is returned, 0 otherwise.
 */
case object OP_CHECKSIG extends CryptoSignatureEvaluation {
  override def opCode = 172
}

/**
 * Same as OP_CHECKSIG, but OP_VERIFY is executed afterward.
 */
case object OP_CHECKSIGVERIFY extends CryptoSignatureEvaluation {
  override def opCode = 173
}

/**
 * Compares the first signature against each public key until it finds an ECDSA match.
 * Starting with the subsequent public key, it compares the second signature against each remaining public key
 * until it finds an ECDSA match.
 * The process is repeated until all signatures have been checked or not enough public keys remain to produce a successful result.
 * All signatures need to match a public key.
 * Because public keys are not checked again if they fail any signature comparison,
 * signatures must be placed in the scriptSig using the same order as their corresponding public keys
 * were placed in the scriptPubKey or redeemScript. If all signatures are valid, 1 is returned, 0 otherwise.
 * Due to a bug, one extra unused value is removed from the stack.
 */
case object OP_CHECKMULTISIG extends CryptoSignatureEvaluation {
  override def opCode = 174
}

/**
 * 	Same as OP_CHECKMULTISIG, but OP_VERIFY is executed afterward.
 */
case object OP_CHECKMULTISIGVERIFY extends CryptoSignatureEvaluation {
  override def opCode = 175
}

object CryptoOperation extends ScriptOperationFactory[CryptoOperation] {
  override def operations = Seq(OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY, OP_CHECKSIG, OP_CHECKSIGVERIFY,
    OP_CODESEPARATOR, OP_HASH160, OP_HASH256, OP_RIPEMD160, OP_SHA1, OP_SHA256)
}