package org.bitcoins.core.protocol

import org.bitcoins.core.util.BitcoinSUnitTest
import org.bitcoins.testkit.core.gen.AddressGenerator

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
}
