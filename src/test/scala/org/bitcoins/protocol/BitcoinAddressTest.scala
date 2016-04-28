package org.bitcoins.protocol

import org.scalatest.{FlatSpec, MustMatchers}

class BitcoinAddressTest extends FlatSpec with MustMatchers {

  "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy" must "be a valid bitcoin address" in {
    val address = "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy"
    BitcoinAddress(address).value must be(address)

  }

  "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy" must "be a valid p2sh address and not a valid p2pkh" in {
    val address = "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy"
    BitcoinAddress.p2shAddress(address) must be (true)
    BitcoinAddress.p2pkh(address) must be (false)
  }

  "17WN1kFw8D6w1eHzqvkh49xwjE3iPN925b" must "be a valid p2pkh" in {
    val address = "17WN1kFw8D6w1eHzqvkh49xwjE3iPN925b"
    BitcoinAddress.p2pkh(address) must be (true)
    BitcoinAddress.p2shAddress(address) must be (false)
  }

  "The empty string" must "not be a valid bitcoin address" in {
    intercept[IllegalArgumentException] {
      BitcoinAddress("")
    }
  }
  "A string that is 25 characters long" must "not be a valid bitcoin address" in {
    val address = "3J98t1WpEZ73CNmQviecrnyiW"
    intercept[IllegalArgumentException] {
      BitcoinAddress(address)
    }
  }

  "A string that is 36 characters long" must "not be a valid bitcoin address" in {
    val address = "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLyyy"
    intercept[IllegalArgumentException] {
      BitcoinAddress(address)
    }
  }

  "akJsoCcyh34FGPotxfEoSXGwFPCNAkyCgTA" must "be a valid asset address" in {
    val assetAddress = AssetAddress("akJsoCcyh34FGPotxfEoSXGwFPCNAkyCgTA")
    assetAddress.value must be ("akJsoCcyh34FGPotxfEoSXGwFPCNAkyCgTA")
  }

  "An asset address with the first character replaced" must "not be a valid asset address" in {
    //3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLyy
    intercept[IllegalArgumentException] {
      val assetAddress = AssetAddress("aJ98t1WpEZ73CNmQviecrnyiWrnqRhWNLyy")
    }
  }
}