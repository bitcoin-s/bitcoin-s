package org.bitcoins.core.p2p

import org.bitcoins.core.p2p.TypeIdentifier.{MsgBlock, MsgFilteredBlock, MsgTx}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TypeIdentifierTest extends BitcoinSUnitTest {

  "MsgTx" must "serialize to 01000000" in {
    MsgTx.hex must be("01000000")
  }

  "MsgBlock" must "serialize to 02000000" in {
    MsgBlock.hex must be("02000000")
  }

  "MsgFilteredBlock" must "serialize to 03000000" in {
    MsgFilteredBlock.hex must be("03000000")
  }
}
