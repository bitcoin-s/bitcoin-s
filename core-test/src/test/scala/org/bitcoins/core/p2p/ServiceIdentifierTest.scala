package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.P2PGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class ServiceIdentifierTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(P2PGenerator.serviceIdentifier) { id =>
      assert(ServiceIdentifier.fromBytes(id.bytes) == id)
    }
  }

  it must "parse NODE_NONE" in {
    assert(ServiceIdentifier.NODE_NONE.nodeNone)
  }

  it must "parse NODE_NETWORK" in {
    assert(ServiceIdentifier.NODE_NETWORK.nodeNetwork)
  }

  it must "parse NODE_GET_UTXO" in {
    assert(ServiceIdentifier.NODE_GET_UTXO.nodeGetUtxo)
  }

  it must "parse NODE_BLOOM" in {
    assert(ServiceIdentifier.NODE_BLOOM.nodeBloom)
  }

  it must "parse NODE_WITNESS" in {
    assert(ServiceIdentifier.NODE_WITNESS.nodeWitness)
  }

  it must "parse NODE_XTHIN" in {
    assert(ServiceIdentifier.NODE_XTHIN.nodeXthin)
  }

  it must "parse NODE_NETWORK_LIMITED" in {
    assert(ServiceIdentifier.NODE_NETWORK_LIMITED.nodeNetworkLimited)
  }

  it must "correctly get a ServiceIdentifier from string" in {
    assert(
      ServiceIdentifier.fromString("NETWORK") == ServiceIdentifier.NODE_NETWORK)
    assert(ServiceIdentifier
      .fromString("NETWORK_LIMITED") == ServiceIdentifier.NODE_NETWORK_LIMITED)
    assert(
      ServiceIdentifier.fromString("WITNESS") == ServiceIdentifier.NODE_WITNESS)
    assert(
      ServiceIdentifier.fromString("BLOOM") == ServiceIdentifier.NODE_BLOOM)
    assert(
      ServiceIdentifier
        .fromString("GETUTXO") == ServiceIdentifier.NODE_GET_UTXO)
    assert(ServiceIdentifier
      .fromString("COMPACT_FILTERS") == ServiceIdentifier.NODE_COMPACT_FILTERS)
    assert(
      ServiceIdentifier.fromString("XTHIN") == ServiceIdentifier.NODE_XTHIN)
    assertThrows[IllegalArgumentException](
      ServiceIdentifier.fromString("this is invalid"))
  }
}
