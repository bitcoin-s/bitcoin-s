package org.bitcoins.core.crypto

/**
 * The result type returned by checking a signature
 */
sealed trait TransactionSignatureCheckerResult {
  /**
   * Indicates if the transaction signature checker was successful or failed
   */
  def isValid: Boolean
}

/**
 * Represents the case that the signatures checked inside of the transaction were
 * all validly encoded as per the script verify flag & that the signatures
 * were valid when checked against the public keys
 */
case object SignatureValidationSuccess extends TransactionSignatureCheckerResult {
  def isValid = true
}

/** Indicates that there was an error when evaluating the signature of a transaction */
sealed trait SignatureValidationError extends TransactionSignatureCheckerResult {
  def isValid = false
}

/**
 * Signature validation failed because a signature was not encoded
 * per the BIP66 rules [[https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki#specification]]
 */
case object SignatureValidationErrorNotStrictDerEncoding extends SignatureValidationError

/**
 * Signature validation failed because there were not enough correct signatures for the transaction
 * we were given
 */
case object SignatureValidationErrorIncorrectSignatures extends SignatureValidationError

/**
 * This indicates that the signature validation failed because we have more signatures left to check
 * than public keys remaining to check them against
 * see [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L914-915]]
 */
case object SignatureValidationErrorSignatureCount extends SignatureValidationError

/**
 * This indicates that the public key was not encoded correctly according to this function
 * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L214-L223]]
 */
case object SignatureValidationErrorPubKeyEncoding extends SignatureValidationError
/**
 * This indicates that the digital signature did not have a Low S value as per BIP62
 * [[https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures]]
 */
case object SignatureValidationErrorHighSValue extends SignatureValidationError

/**
 * Fails the script if the hash type is not defined on a digital signature
 */
case object SignatureValidationErrorHashType extends SignatureValidationError

/**
 * Fails the script if the given public key was not compressed and the [[org.bitcoins.core.script.flag.ScriptVerifyWitnessPubKeyType]]
 * flag was set
 */
case object SignatureValidationErrorWitnessPubKeyType extends SignatureValidationError

/**
 * Fails the script if a an invalid signature is not an empty byte vector
 * See BIP146
 * [[https://github.com/bitcoin/bips/blob/master/bip-0146.mediawiki#nullfail]]
 */
case object SignatureValidationErrorNullFail extends SignatureValidationError
