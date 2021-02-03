package org.bitcoins.core.script.crypto

import org.bitcoins.testkit.util.BitcoinSUnitTest

/** Created by chris on 4/2/16.
  */
class CryptoSignatureEvaluationFactoryTest extends BitcoinSUnitTest {

  "CryptoSignatureEvaluationFactory" must "have all of the Script operations that involve checking signatures" in {
    CryptoSignatureEvaluationFactory.operations must be(
      Seq(OP_CHECKMULTISIG,
          OP_CHECKMULTISIGVERIFY,
          OP_CHECKSIG,
          OP_CHECKSIGVERIFY))
  }
}
