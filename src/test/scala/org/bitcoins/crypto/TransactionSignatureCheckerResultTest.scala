package org.bitcoins.crypto

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/31/16.
 */
class TransactionSignatureCheckerResultTest extends FlatSpec with MustMatchers  {

  "TransactionSignatureCheckerResult" must "have isValid set correctly for the different outcomes of TransactionSignatureCheckerResult" in {
    SignatureValidationFailureIncorrectSignatures.isValid must be (false)
    SignatureValidationFailureNotStrictDerEncoding.isValid must be (false)
    SignatureValidationSuccess.isValid must be (true)
  }
}
