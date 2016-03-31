package org.scalacoin.crypto


/**
 * The result type returned by checking a signature
 */
sealed trait TransactionSignatureCheckerResult {
  /**
   * Indicates if the transaction signature checker was successful or failed
   * @return
   */
  def isValid : Boolean
}

/**
 * Represents the case that the signatures checked inside of the transaction were
 * all validly encoded as per the script verify flag & that the signatures
 * were valid when checked against the public keys
 */
case object SignatureValidationSuccess extends TransactionSignatureCheckerResult {
  def isValid = true
}

/**
 * Signature validation failed because a signature was not encoded
 * per the BIP66 rules https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki#specification
 */
case object SignatureValidationFailureNotStrictDerEncoding extends TransactionSignatureCheckerResult {
  def isValid = false
}

/**
 * Signature validation failed because there were not enough correct signatures for the transaction
 * we were given
 */
case object SignatureValidationFailureIncorrectSignatures extends TransactionSignatureCheckerResult {
  def isValid = false
}
