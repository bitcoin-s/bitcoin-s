package org.bitcoins.core.protocol

import org.bitcoins.core.util.{Base58, BitcoinSUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

import scala.util.Success

/**
 * Created by chris on 3/30/16.
 */
class AddressFactoryTest extends FlatSpec with MustMatchers {

  "AddressFactory" must "create an address from a base58 encoded string" in {
    Address(TestUtil.bitcoinAddress.value) must be (Success(TestUtil.bitcoinAddress))
  }

  it must "create an address from a sequence of bytes"  in {
    val decoded = Base58.decode(TestUtil.bitcoinAddress.value)
    Address(decoded) must be (Success(TestUtil.bitcoinAddress))
  }

  it must "throw an exception if we give a hex string to create a bitcoin address from" in {
    intercept[IllegalArgumentException] {
      throw Address.fromHex("01234567890abcdef").failed.get
    }
  }
}
