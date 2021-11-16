package org.bitcoins.core.p2p

import org.bitcoins.core.p2p.TypeIdentifier._
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class TypeIdentifierTest extends BitcoinSUnitTest {

  it must "Create a MsgUnassigned" in {
    assert(TypeIdentifier(100000).isInstanceOf[MsgUnassigned])
  }

  "MsgTx" must "serialize to 01000000" in {
    MsgTx.hex must be("01000000")
    assert(TypeIdentifier.fromBytes(MsgTx.bytes) == MsgTx)
  }

  "MsgWitnessTx" must "serialize to 01000040" in {
    MsgWitnessTx.hex must be("01000040")
    assert(TypeIdentifier.fromBytes(MsgWitnessTx.bytes) == MsgWitnessTx)
  }

  "MsgBlock" must "serialize to 02000000" in {
    MsgBlock.hex must be("02000000")
    assert(TypeIdentifier.fromBytes(MsgBlock.bytes) == MsgBlock)
  }

  "MsgWitnessBlock" must "serialize to 02000040" in {
    MsgWitnessBlock.hex must be("02000040")
    assert(TypeIdentifier.fromBytes(MsgWitnessBlock.bytes) == MsgWitnessBlock)
  }

  "MsgFilteredBlock" must "serialize to 03000000" in {
    MsgFilteredBlock.hex must be("03000000")
    assert(TypeIdentifier.fromBytes(MsgFilteredBlock.bytes) == MsgFilteredBlock)
  }

  "MsgFilteredWitnessBlock" must "serialize to 03000040" in {
    MsgFilteredWitnessBlock.hex must be("03000040")
    assert(
      TypeIdentifier.fromBytes(
        MsgFilteredWitnessBlock.bytes) == MsgFilteredWitnessBlock)
  }

  "MsgCompactBlock" must "serialize to 04000000" in {
    MsgCompactBlock.hex must be("04000000")
    assert(TypeIdentifier.fromBytes(MsgCompactBlock.bytes) == MsgCompactBlock)
  }

}
