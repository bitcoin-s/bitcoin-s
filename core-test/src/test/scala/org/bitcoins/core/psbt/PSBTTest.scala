package org.bitcoins.core.psbt

import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class PSBTTest extends BitcoinSUnitTest {

  it must "successfully combine two PSBTs" in {
    // PSBT with 2 inputs, and empty outputs
    val expected = PSBT.fromBytes(
      hex"70736274ff0100a00200000002ab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40000000000feffffffab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40100000000feffffff02603bea0b000000001976a914768a40bbd740cbe81d988e71de2a4d5c71396b1d88ac8e240000000000001976a9146f4620b553fa095e721b9ee0efe9fa039cca459788ac000000000001076a47304402204759661797c01b036b25928948686218347d89864b719e1f7fcf57d1e511658702205309eabf56aa4d8891ffd111fdf1336f3a29da866d7f8486d75546ceedaf93190121035cdc61fc7ba971c0b501a646a2a83b102cb43881217ca682dc86e2d73fa882920001012000e1f5050000000017a9143545e6e33b832c47050f24d3eeb93c9c03948bc787010416001485d13537f2e265405a34dbafa9e3dda01fb82308000000")
    val psbt1 =
      PSBT(expected.globalMap,
           Vector(expected.inputMaps.head, InputPSBTMap(Vector.empty)),
           expected.outputMaps)
    val psbt2 =
      PSBT(expected.globalMap,
           Vector(InputPSBTMap(Vector.empty), expected.inputMaps.last),
           expected.outputMaps)

    assert(psbt1.combinePSBT(psbt2) == expected)
    assert(psbt2.combinePSBT(psbt1) == expected)
    assert(psbt1.combinePSBT(psbt1) != expected)
    assert(psbt2.combinePSBT(psbt2) != expected)
  }

  it must "successfully combine two PSBTs with unknown types" in {
    val psbt1 = PSBT(
      "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a0100000000000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f00")
    val psbt2 = PSBT(
      "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a0100000000000a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708100f0102030405060708090a0b0c0d0e0f00")
    val expected = PSBT(
      "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a0100000000000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0a0f0102030405060708100f0102030405060708090a0b0c0d0e0f00")

    assert(psbt1.combinePSBT(psbt2) == expected)
  }
}
