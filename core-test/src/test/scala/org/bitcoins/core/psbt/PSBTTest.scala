package org.bitcoins.core.psbt

import org.bitcoins.core.crypto.{ECPublicKey, Sha256Digest, Sign}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.script.{
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0,
  ScriptPubKey
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.utxo.ConditionalPath
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

import scala.annotation.tailrec

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

  private def getDummySigners(size: Int): Seq[Sign] = {
    @tailrec
    def loop(i: Int, accum: Seq[Sign]): Seq[Sign] = {
      if (i <= 0) {
        accum
      } else {
        loop(i - 1, accum :+ Sign.dummySign(ECPublicKey.freshPublicKey))
      }
    }
    loop(size, Nil)
  }

  it must "create a valid UTXOSpendingInfo" in {
    // PSBT with one P2WSH input of a 2-of-2 multisig. witnessScript, keypaths, and global xpubs are available. Contains no signatures. Outputs filled.
    val psbt = PSBT(
      "70736274ff01005202000000019dfc6628c26c5899fe1bd3dc338665bfd55d7ada10f6220973df2d386dec12760100000000ffffffff01f03dcd1d000000001600147b3a00bfdc14d27795c2b74901d09da6ef133579000000004f01043587cf02da3fd0088000000097048b1ad0445b1ec8275517727c87b4e4ebc18a203ffa0f94c01566bd38e9000351b743887ee1d40dc32a6043724f2d6459b3b5a4d73daec8fbae0472f3bc43e20cd90c6a4fae000080000000804f01043587cf02da3fd00880000001b90452427139cd78c2cff2444be353cd58605e3e513285e528b407fae3f6173503d30a5e97c8adbc557dac2ad9a7e39c1722ebac69e668b6f2667cc1d671c83cab0cd90c6a4fae000080010000800001012b0065cd1d000000002200202c5486126c4978079a814e13715d65f36459e4d6ccaded266d0508645bafa6320105475221029da12cdb5b235692b91536afefe5c91c3ab9473d8e43b533836ab456299c88712103372b34234ed7cf9c1fea5d05d441557927be9542b162eb02e1ab2ce80224c00b52ae2206029da12cdb5b235692b91536afefe5c91c3ab9473d8e43b533836ab456299c887110d90c6a4fae0000800000008000000000220603372b34234ed7cf9c1fea5d05d441557927be9542b162eb02e1ab2ce80224c00b10d90c6a4fae0000800100008000000000002202039eff1f547a1d5f92dfa2ba7af6ac971a4bd03ba4a734b03156a256b8ad3a1ef910ede45cc500000080000000800100008000")
    val dummySigners = getDummySigners(2)
    val spendingInfo = psbt.getUTXOSpendingInfo(index = 0, dummySigners)

    assert(spendingInfo.outPoint == psbt.transaction.inputs.head.previousOutput)
    assert(spendingInfo.amount == Satoshis(500000000))
    assert(
      spendingInfo.scriptPubKey == P2WSHWitnessSPKV0.fromHash(Sha256Digest(
        "2c5486126c4978079a814e13715d65f36459e4d6ccaded266d0508645bafa632")))
    assert(spendingInfo.signers == dummySigners)
    assert(spendingInfo.hashType == HashType.sigHashAll)
    assert(spendingInfo.redeemScriptOpt.isEmpty)
    assert(
      spendingInfo.scriptWitnessOpt.contains(
        P2WSHWitnessV0(ScriptPubKey.fromAsmHex(
          "5221029da12cdb5b235692b91536afefe5c91c3ab9473d8e43b533836ab456299c88712103372b34234ed7cf9c1fea5d05d441557927be9542b162eb02e1ab2ce80224c00b52ae"))))
    assert(spendingInfo.conditionalPath == ConditionalPath.NoConditionsLeft)
  }

  it must "fail to create a valid UTXOSpendingInfo" in {

    val psbt = PSBT(
      "70736274ff01005202000000019dfc6628c26c5899fe1bd3dc338665bfd55d7ada10f6220973df2d386dec12760100000000ffffffff01f03dcd1d000000001600147b3a00bfdc14d27795c2b74901d09da6ef133579000000004f01043587cf02da3fd0088000000097048b1ad0445b1ec8275517727c87b4e4ebc18a203ffa0f94c01566bd38e9000351b743887ee1d40dc32a6043724f2d6459b3b5a4d73daec8fbae0472f3bc43e20cd90c6a4fae000080000000804f01043587cf02da3fd00880000001b90452427139cd78c2cff2444be353cd58605e3e513285e528b407fae3f6173503d30a5e97c8adbc557dac2ad9a7e39c1722ebac69e668b6f2667cc1d671c83cab0cd90c6a4fae000080010000800001012b0065cd1d000000002200202c5486126c4978079a814e13715d65f36459e4d6ccaded266d0508645bafa6320105475221029da12cdb5b235692b91536afefe5c91c3ab9473d8e43b533836ab456299c88712103372b34234ed7cf9c1fea5d05d441557927be9542b162eb02e1ab2ce80224c00b52ae2206029da12cdb5b235692b91536afefe5c91c3ab9473d8e43b533836ab456299c887110d90c6a4fae0000800000008000000000220603372b34234ed7cf9c1fea5d05d441557927be9542b162eb02e1ab2ce80224c00b10d90c6a4fae0000800100008000000000002202039eff1f547a1d5f92dfa2ba7af6ac971a4bd03ba4a734b03156a256b8ad3a1ef910ede45cc500000080000000800100008000")

    assertThrows[IllegalArgumentException](
      psbt.getUTXOSpendingInfo(index = -1, getDummySigners(2)))

    val psbt1 = PSBT(
      "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a010000000000000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0000")
    assertThrows[UnsupportedOperationException](
      psbt1.getUTXOSpendingInfo(index = 0, getDummySigners(1)))
  }
}
