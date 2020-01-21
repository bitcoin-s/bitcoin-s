package org.bitcoins.core.protocol

import org.bitcoins.core.util.Base58
import org.bitcoins.testkit.util.{BitcoinSUnitTest, TestUtil}

/**
  * Created by chris on 3/30/16.
  */
class AddressFactoryTest extends BitcoinSUnitTest {

  "AddressFactory" must "create an address from a base58 encoded string" in {
    Address(TestUtil.bitcoinAddress.get.value) must be(TestUtil.bitcoinAddress)
  }

  it must "create an address from a sequence of bytes" in {
    val decoded = Base58.decode(TestUtil.bitcoinAddress.get.value)
    Address(decoded) must be(TestUtil.bitcoinAddress)
  }

  it must "throw an exception if we give a hex string to create a bitcoin address from" in {
    intercept[IllegalArgumentException] {
      throw Address.fromHex("01234567890abcdef").failed.get
    }
  }
}
