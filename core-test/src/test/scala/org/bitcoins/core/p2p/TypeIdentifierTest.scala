package org.bitcoins.core.p2p

import org.bitcoins.core.p2p.TypeIdentifier._
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TypeIdentifierTest extends BitcoinSUnitTest {

  "MsgTx" must "serialize to 01000000" in {
    MsgTx.hex must be("01000000")
  }

  "MsgWitnessTx" must "serialize to 01000040" in {
    MsgWitnessTx.hex must be("01000040")
  }

  "MsgBlock" must "serialize to 02000000" in {
    MsgBlock.hex must be("02000000")
  }

  "MsgWitnessBlock" must "serialize to 02000040" in {
    MsgWitnessBlock.hex must be("02000040")
  }

  "MsgFilteredBlock" must "serialize to 03000000" in {
    MsgFilteredBlock.hex must be("03000000")
  }

  "MsgFilteredWitnessBlock" must "serialize to 03000040" in {
    MsgFilteredWitnessBlock.hex must be("03000040")
  }

}
