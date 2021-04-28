package org.bitcoins.crypto

class KeysTest extends BitcoinSCryptoTest {

  it must "generate keys" in {
    val privkey = ECPrivateKey.freshPrivateKey
    assert(privkey != null)
    assert(BCryptoCryptoRuntime.secKeyVerify(privkey.bytes))

    val pubkey = privkey.publicKey
    assert(pubkey != null)

    assert(!BCryptoCryptoRuntime.secKeyVerify(pubkey.bytes))
  }

}
