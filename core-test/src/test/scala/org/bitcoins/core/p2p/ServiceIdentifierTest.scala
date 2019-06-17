package org.bitcoins.core.p2p

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.testkit.core.gen.p2p.P2PGenerator

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
}
