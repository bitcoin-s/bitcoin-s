package org.bitcoins.script.crypto

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/8/16.
 */
class CryptoOperationsFactoryTest extends FlatSpec with MustMatchers {

  "CryptoOperationsFactory" must "match strings with crypto operations" in {
    CryptoOperation.fromString("OP_CHECKSIG") must be (Some(OP_CHECKSIG))
    CryptoOperation.fromString("OP_HASH160") must be (Some(OP_HASH160))
    CryptoOperation.fromString("OP_SHA256") must be (Some(OP_SHA256))
    CryptoOperation.fromString("RANDOM") must be (None)
  }
}
