package org.bitcoins.crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class KeysTest extends AnyFlatSpec with Matchers {

  it must "generate keys" in {
    val privkey = ECPrivateKey.freshPrivateKey
    assert(privkey != null)

    val pubkey = privkey.publicKey
    assert(pubkey != null)
  }

}
