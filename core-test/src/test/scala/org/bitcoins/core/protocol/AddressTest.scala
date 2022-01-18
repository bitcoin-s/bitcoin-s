package org.bitcoins.core.protocol

import org.bitcoins.core.config.MainNet
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.testkitcore.gen.AddressGenerator
import org.bitcoins.testkitcore.util.{BitcoinSUnitTest, TestUtil}

import scala.util.{Failure, Success}

class AddressTest extends BitcoinSUnitTest {

  behavior of "Address"

  it must "have serialization symmetry" in {
    forAll(AddressGenerator.address) { addr =>
      val fromSPK = Address
        .fromScriptPubKeyT(addr.scriptPubKey, addr.networkParameters)
      fromSPK match {
        case Success(newAddr)   => assert(newAddr.value == addr.value)
        case Failure(exception) => fail(exception.getMessage)
      }

      val fromStringT = Address.fromStringT(addr.value)
      fromStringT match {
        case Success(newAddr)   => assert(newAddr.value == addr.value)
        case Failure(exception) => fail(exception.getMessage)
      }
    }
  }

  it must "have different results when constructing compressed and decompressed p2pkh" in {
    val pubKey = ECPublicKey.freshPublicKey
    val compressed = P2PKHAddress(pubKey, MainNet)
    val decompressed = P2PKHAddress.fromDecompressedPubKey(pubKey, MainNet)
    assert(compressed.value != decompressed.value)
    assert(compressed != decompressed)
  }

  it must "serialize a bech32 address correctly" in {
    TestUtil.bech32Address.toString must be(
      "bcrt1qq6w6pu6zq90az9krn53zlkvgyzkyeglzukyepf")
  }

  it must "calculate the correct descriptor" in {
    forAll(AddressGenerator.address) { addr =>
      assert(addr.descriptor == s"addr(${addr.value})")
    }
  }
}
