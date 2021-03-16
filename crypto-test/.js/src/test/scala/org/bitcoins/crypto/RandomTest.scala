package org.bitcoins.crypto

class RandomTest extends BitcoinSCryptoTest {

  it should "generate random bytes" in {
    val rnd = 1.to(16).map(_ => BCryptoCryptoRuntime.randomBytes(32))
    assert(rnd.size == 16)
    assert(rnd.distinct.size == rnd.size)
  }

}
