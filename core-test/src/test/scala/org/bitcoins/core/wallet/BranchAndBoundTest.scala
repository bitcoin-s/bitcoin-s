package org.bitcoins.core.wallet

import org.bitcoins.core.api.wallet._
import org.bitcoins.core.currency._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee._
import org.bitcoins.crypto._
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.util.Random

class BranchAndBoundTest extends BitcoinSUnitTest {

  behavior of "CoinSelector"

  val rand = new Random(System.currentTimeMillis())

  // tests copied from https://github.com/bitcoindevkit/bdk/blob/6bae52e6f2843a371ec2a6e293ea7ab0ba96aff4/src/wallet/coin_selection.rs#L1322

  private def buildTestUtxo(value: CurrencyUnit, idx: Int): CoinSelectorUtxo = {
    val output = TransactionOutput(
      value = value,
      scriptPubKey = P2WPKHWitnessSPKV0(ECPublicKey.dummy)
    )
    val prevOut = TransactionOutPoint(DoubleSha256Digest.empty, UInt32(idx))
    CoinSelectorUtxo(output, prevOut, None, None)
  }

  val testUtxos: Vector[CoinSelectorUtxo] = {
    Vector(buildTestUtxo(Satoshis(100000), 0),
           buildTestUtxo(Satoshis(10), 1),
           buildTestUtxo(Satoshis(200000), 2))
  }

  def generateSameValueUtxos(
      value: CurrencyUnit,
      numUtxos: Int): Vector[CoinSelectorUtxo] = {
    0.until(numUtxos).toVector.map { int =>
      buildTestUtxo(value, int)
    }
  }

  def generateRandomUtxos(numUtxos: Int): Vector[CoinSelectorUtxo] = {
    0.until(numUtxos).toVector.map { int =>
      val value = Satoshis(Math.abs(rand.nextLong() % 200000000L))
      buildTestUtxo(value, int)
    }
  }

  def sumRandomUtxos(utxos: Vector[CoinSelectorUtxo]): CurrencyUnit = {
    // select between 2 and utxos.size/2 utxos
    val num = Math.max(2, rand.nextInt(utxos.size / 2))
    rand.shuffle(utxos).take(num).map(_.value).sum
  }

  it must "correctly select exact coins" in {
    0.until(200).foreach { _ =>
      val utxos = generateRandomUtxos(16)
      val target = sumRandomUtxos(utxos)
      val targetUtxo = TransactionOutput(target, EmptyScriptPubKey)

      val selection = CoinSelector.branchAndBound(utxos,
                                                  Vector(targetUtxo),
                                                  Satoshis.zero,
                                                  SatoshisPerVirtualByte.zero)

      val selectedAmt = selection.map(_.value).sum
      assert(selectedAmt == target)
    }
  }

  it must "correctly select exact coins with more utxos" in {
    0.until(200).foreach { _ =>
      val utxos = generateRandomUtxos(40)
      val target = Vector(utxos(3).prevOut, utxos(23).prevOut)

      val selection = CoinSelector.branchAndBound(utxos,
                                                  target,
                                                  Satoshis.zero,
                                                  SatoshisPerVirtualByte.zero)

      val selectedAmt = selection.map(_.value).sum
      assert(selectedAmt == target.map(_.value).sum)
    }
  }

  it must "have an exact match with fees" in {
    val feeRate = SatoshisPerVirtualByte.fromLong(1)
    val utxos = generateSameValueUtxos(Satoshis(50000), 10)

    val changeCost = feeRate * 31
    val dummyUtxo = TransactionOutput(Satoshis.zero, EmptyScriptPubKey)

    val nonInputFees = feeRate * (dummyUtxo.byteSize + 10)

    // 2*(value of 1 utxo)  - 2*(1 utxo fees with 1.0sat/vbyte fee rate) -
    // changeCost + 5 - output cost
    val targetAmount =
      (2 * 50000) - (2 * 67) - changeCost.satoshis.toLong + 5 - nonInputFees.satoshis.toLong

    val target = TransactionOutput(Satoshis(targetAmount), EmptyScriptPubKey)

    val selection = CoinSelector
      .branchAndBound(utxos, Vector(target), changeCost, feeRate)

    val selectedAmt = selection.map(_.value).sum
    assert(selectedAmt == Satoshis(100000))
  }

  it must "fail to find a match" in {
    val feeRate = SatoshisPerVirtualByte.fromLong(10)
    val target = buildTestUtxo(Satoshis(20000), 0)
    val changeCost = feeRate * 31

    val error = intercept[RuntimeException](
      CoinSelector
        .branchAndBound(testUtxos, Vector(target.prevOut), changeCost, feeRate))

    assert(error.getMessage == "No solution found for the given parameters")
  }

  it must "have max tries exceeded" in {
    val feeRate = SatoshisPerVirtualByte.fromLong(10)
    val utxos = generateSameValueUtxos(Satoshis(100000), 100000)
    val target = buildTestUtxo(Satoshis(20000), 0)
    val changeCost = feeRate * 31

    val error = intercept[RuntimeException](
      CoinSelector
        .branchAndBound(utxos, Vector(target.prevOut), changeCost, feeRate))

    assert(error.getMessage == "Could not find a bnb solution")
  }
}
