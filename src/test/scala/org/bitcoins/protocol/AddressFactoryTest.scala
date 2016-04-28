package org.bitcoins.protocol

import org.bitcoins.util.{BitcoinSUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class AddressFactoryTest extends FlatSpec with MustMatchers {

  "AddressFactory" must "create an address from a base58 encoded string" in {
    Address(TestUtil.bitcoinAddress.value) must be (TestUtil.bitcoinAddress)
  }

  it must "create an address from a sequence of bytes"  in {
    Address(BitcoinSUtil.decodeBase58(TestUtil.bitcoinAddress.value)) must be (TestUtil.bitcoinAddress)
  }


  it must "create an asset address from a base58 encoded string" in {
    Address(BitcoinSUtil.decodeBase58(TestUtil.assetAddress.value)) must be (TestUtil.assetAddress)
  }

  it must "throw an exception if the given string" in {
    intercept[RuntimeException] {
      Address("01234567890abcdef")
    }
  }

  it must "throw an exception if we give a hex string to create a bitcoin address from" in {
    intercept[RuntimeException] {
      Address.fromHex("01234567890abcdef")
    }
  }
}
