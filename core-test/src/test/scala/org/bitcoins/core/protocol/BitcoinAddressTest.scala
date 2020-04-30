package org.bitcoins.core.protocol

import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  P2PKHScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  ScriptPubKey
}
import org.bitcoins.crypto.{ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.testkit.util.BitcoinSUnitTest

import scala.util.{Failure, Success, Try}

class BitcoinAddressTest extends BitcoinSUnitTest {

  "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy" must "be a valid bitcoin address" in {
    val address = "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy"
    BitcoinAddress(address).get.value must be(address)

  }

  "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy" must "be a valid p2sh address and not a valid p2pkh" in {
    val address = "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy"
    P2SHAddress.isValid(address) must be(true)
    P2PKHAddress.isValid(address) must be(false)
  }

  "17WN1kFw8D6w1eHzqvkh49xwjE3iPN925b" must "be a valid p2pkh" in {
    val address = "17WN1kFw8D6w1eHzqvkh49xwjE3iPN925b"
    P2PKHAddress.isValid(address) must be(true)
    P2SHAddress.isValid(address) must be(false)
  }

  "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq" must "be a valid Bech32 address" in {
    val address = "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq"
    Address.fromString(address) match {
      case Success(bech: Bech32Address) =>
        assert(bech.networkParameters == MainNet)
      case Success(other: BitcoinAddress) =>
        fail(s"Wrong address type! Got $other")
      case Failure(exception) => fail(exception.getMessage)
    }
  }

  "tb1q9yqzjcywvuy9lz2vuvv6xmkhe7zg9kkp35mdrn" must "be a valid Bech32 address" in {
    val address = "tb1q9yqzjcywvuy9lz2vuvv6xmkhe7zg9kkp35mdrn"
    Address.fromString(address) match {
      case Success(bech: Bech32Address) =>
        assert(bech.networkParameters == TestNet3)
      case Success(other: BitcoinAddress) =>
        fail(s"Wrong address type! Got $other")
      case Failure(exception) => fail(exception.getMessage)
    }
  }

  "bcrt1q03nrxf0s99sny47mp47a8grdvrcph2v4c78rvd" must "be a valid Bec32 addres" in {
    val address = "bcrt1q03nrxf0s99sny47mp47a8grdvrcph2v4c78rvd"
    Address.fromString(address) match {
      case Success(bech: Bech32Address) =>
        assert(bech.networkParameters == RegTest)
      case Success(other: BitcoinAddress) =>
        fail(s"Wrong address type! Got $other")
      case Failure(exception) => fail(exception.getMessage)
    }
  }

  "The empty string" must "not be a valid bitcoin address" in {
    BitcoinAddress.fromString("").isFailure must be(true)
    Try(BitcoinAddress.fromStringExn("")).isFailure must be(true)
  }

  "A string that is 25 characters long" must "not be a valid bitcoin address" in {
    val address = "3J98t1WpEZ73CNmQviecrnyiW"
    BitcoinAddress.fromString(address).isFailure must be(true)
  }

  "A string that is 36 characters long" must "not be a valid bitcoin address" in {
    val address = "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLyyy"
    BitcoinAddress.fromString(address).isFailure must be(true)
  }

  it must "encode a pubKeyHash to an address" in {
    //from https://stackoverflow.com/questions/19233053/hashing-from-a-public-key-to-a-bitcoin-address-in-php
    val hash = Sha256Hash160Digest("010966776006953d5567439e5e39f86a0d273bee")
    val address = Address("16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM").get
    P2PKHAddress(hash, MainNet) must be(address)
  }

  it must "encode a scriptPubKey to an address" in {
    //redeemScript from https://en.bitcoin.it/wiki/Pay_to_script_hash
    val hex =
      "455141042f90074d7a5bf30c72cf3a8dfd1381bdbd30407010e878f3a11269d5f74a58788505cdca22ea6eab7cfb40dc0e07aba200424ab0d79122a653ad0c7ec9896bdf51ae"
    val scriptPubKey = ScriptPubKey(hex)
    val addr = P2SHAddress(scriptPubKey, MainNet)
    addr must be(BitcoinAddress("3P14159f73E4gFr7JterCCQh9QjiTjiZrG").get)
  }

  it must "create a bech32 address from a WitnessScriptPubKey" in {
    val scriptPubKey = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)
    assert(Bech32Address.fromScriptPubKey(scriptPubKey, RegTest).isSuccess)
  }

  it must "fail to create a bech32 address from an invalid ScriptPubKey" in {
    assert(Bech32Address.fromScriptPubKey(EmptyScriptPubKey, RegTest).isFailure)
  }

  it must "create an address from a P2PKHScriptPubKey" in {
    val scriptPubKey = P2PKHScriptPubKey(ECPublicKey.freshPublicKey)
    assert(P2PKHAddress.fromScriptPubKey(scriptPubKey, RegTest).isSuccess)
  }

  it must "fail to create a P2PKHAddress address from an invalid ScriptPubKey" in {
    assert(P2PKHAddress.fromScriptPubKey(EmptyScriptPubKey, RegTest).isFailure)
  }

  it must "create an address from a P2SHScriptPubKey" in {
    val scriptPubKey = P2SHScriptPubKey(EmptyScriptPubKey)
    assert(P2SHAddress.fromScriptPubKey(scriptPubKey, RegTest).isSuccess)
  }

  it must "fail to create a P2SHScriptPubKey address from an invalid ScriptPubKey" in {
    assert(P2SHAddress.fromScriptPubKey(EmptyScriptPubKey, RegTest).isFailure)
  }

  it must "create an address from a ScriptPubKey" in {
    val scriptPubKey = P2SHScriptPubKey(EmptyScriptPubKey)
    assert(Address.fromScriptPubKey(scriptPubKey, RegTest).isSuccess)
  }
}
