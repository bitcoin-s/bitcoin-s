package org.bitcoins.core.protocol

import org.bitcoins.testkit.core.gen.AddressGenerator
import org.bitcoins.testkit.util.{BitcoinSUnitTest, TestUtil}

import scala.util.{Failure, Success}

class AddressTest extends BitcoinSUnitTest {

  behavior of "Address"

  it must "have serialization symmetry" in {
    forAll(AddressGenerator.address) { addr =>
      val fromSPK = Address
        .fromScriptPubKey(addr.scriptPubKey, addr.networkParameters)
      fromSPK match {
        case Success(newAddr)   => assert(newAddr.value == addr.value)
        case Failure(exception) => fail(exception.getMessage)
      }

      val fromStringT = Address.fromString(addr.value)
      fromStringT match {
        case Success(newAddr)   => assert(newAddr.value == addr.value)
        case Failure(exception) => fail(exception.getMessage)
      }
    }
  }

  it must "serialize a bech32 address correctly" in {
    TestUtil.bech32Address.toString must be(
      "bcrt1qq6w6pu6zq90az9krn53zlkvgyzkyeglzukyepf")
  }
}
