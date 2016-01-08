package org.scalacoin.script.crypto

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/8/16.
 */
class CryptoOperationsFactoryTest extends FlatSpec with MustMatchers with CryptoOperationFactory {

  "CryptoOperationsFactory" must "match strings with crypto operations" in {
    fromString("OP_CHECKSIG") must be (Some(OP_CHECKSIG))
    fromString("OP_HASH160") must be (Some(OP_HASH160))
    fromString("OP_SHA256") must be (Some(OP_SHA256))
    fromString("RANDOM") must be (None)
  }
}
