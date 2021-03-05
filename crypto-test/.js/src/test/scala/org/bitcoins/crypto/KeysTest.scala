package org.bitcoins.crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class KeysTest extends AnyFlatSpec with Matchers {

  it must "generate keys" in {
    val privkey = ECPrivateKey.freshPrivateKey
    assert(privkey != null)
    assert(BCryptoCryptoRuntime.secKeyVerify(privkey.bytes))

    val pubkey = privkey.publicKey
    assert(pubkey != null)
    assert(BCryptoCryptoRuntime.isValidPubKey(pubkey.bytes))

    assert(!BCryptoCryptoRuntime.secKeyVerify(pubkey.bytes))
    assert(!BCryptoCryptoRuntime.isValidPubKey(privkey.bytes))
  }

}
