package org.bitcoins.core.crypto

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 3/31/16.
  */
class TransactionSignatureCheckerResultTest extends BitcoinSUnitTest {

  "TransactionSignatureCheckerResult" must "have isValid set correctly for the different outcomes of TransactionSignatureCheckerResult" in {
    SignatureValidationErrorIncorrectSignatures.isValid must be(false)
    SignatureValidationErrorNotStrictDerEncoding.isValid must be(false)
    SignatureValidationSuccess.isValid must be(true)
  }
}
