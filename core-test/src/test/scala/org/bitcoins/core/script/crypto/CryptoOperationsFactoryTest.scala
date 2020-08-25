package org.bitcoins.core.script.crypto

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 1/8/16.
  */
class CryptoOperationsFactoryTest extends BitcoinSUnitTest {

  "CryptoOperationsFactory" must "match strings with crypto operations" in {
    CryptoOperation.fromStringOpt("OP_CHECKSIG") must be(Some(OP_CHECKSIG))
    CryptoOperation.fromStringOpt("OP_HASH160") must be(Some(OP_HASH160))
    CryptoOperation.fromStringOpt("OP_SHA256") must be(Some(OP_SHA256))
    CryptoOperation.fromStringOpt("RANDOM") must be(None)
  }
}
