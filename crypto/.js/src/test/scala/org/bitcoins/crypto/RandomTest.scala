package org.bitcoins.crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class RandomTest extends AnyFlatSpec with Matchers {

  it should "generate random bytes" in {
    val rnd = 1.to(16).map(_ => BCryptoCryptoRuntime.randomBytes(32))
    assert(rnd.size == 16)
    assert(rnd.distinct.size == rnd.size)
  }

}
