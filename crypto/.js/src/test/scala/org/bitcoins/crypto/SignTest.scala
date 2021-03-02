package org.bitcoins.crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SignTest extends AnyFlatSpec with Matchers {

  it must "be able to sign and verify signatures" in {
    val privkey = ECPrivateKey.freshPrivateKey
    val pubkey = privkey.publicKey
    val msg = BCryptoCryptoRuntime.randomBytes(32)
    val sig = privkey.sign(msg)
    assert(pubkey.verify(msg, sig))
  }

}
