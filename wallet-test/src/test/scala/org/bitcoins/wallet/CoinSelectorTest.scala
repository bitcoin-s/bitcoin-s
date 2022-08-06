package org.bitcoins.wallet

import org.bitcoins.core.api.wallet.{CoinSelector, CoinSelectorUtxo}
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerByte}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.{TransactionGenerators, WitnessGenerators}
import org.scalatest.FutureOutcome

class CoinSelectorTest extends BitcoinSWalletTest {

  case class CoinSelectionFixture(
      output: TransactionOutput,
      feeRate: FeeUnit,
      utxo1: CoinSelectorUtxo,
      utxo2: CoinSelectorUtxo,
      utxo3: CoinSelectorUtxo) {

    val utxoSet: Vector[CoinSelectorUtxo] = Vector(utxo1, utxo2, utxo3)

  }

  override type FixtureParam = CoinSelectionFixture

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val output = TransactionOutput(99.sats, ScriptPubKey.empty)
    val feeRate = SatoshisPerByte(CurrencyUnits.zero)

    val outpoint1 = TransactionGenerators.outPoint.sampleSome
    val utxo1 = CoinSelectorUtxo(
      outPoint = outpoint1,
      prevOut = TransactionOutput(10.sats, ScriptPubKey.empty),
      scriptWitnessOpt = Some(WitnessGenerators.scriptWitness.sampleSome),
      redeemScriptOpt = None
    )
    val outPoint2 = TransactionGenerators.outPoint.sampleSome
    val utxo2 = CoinSelectorUtxo(
      outPoint = outPoint2,
      prevOut = TransactionOutput(90.sats, ScriptPubKey.empty),
      scriptWitnessOpt = Some(WitnessGenerators.scriptWitness.sampleSome),
      redeemScriptOpt = None
    )

    val outPoint3 = TransactionGenerators.outPoint.sampleSome
    val utxo3 = CoinSelectorUtxo(
      outPoint = outPoint3,
      prevOut = TransactionOutput(20.sats, ScriptPubKey.empty),
      scriptWitnessOpt = Some(WitnessGenerators.scriptWitness.sampleSome),
      redeemScriptOpt = None
    )

    test(CoinSelectionFixture(output, feeRate, utxo1, utxo2, utxo3))
  }

  behavior of "CoinSelector"

  it must "accumulate largest outputs" in { fixture =>
    val selection =
      CoinSelector.accumulateLargest(walletUtxos = fixture.utxoSet,
                                     outputs = Vector(fixture.output),
                                     feeRate = fixture.feeRate)

    assert(selection == Vector(fixture.utxo2, fixture.utxo3))
  }

  it must "accumulate smallest outputs" in { fixture =>
    val selection =
      CoinSelector.accumulateSmallestViable(walletUtxos = fixture.utxoSet,
                                            outputs = Vector(fixture.output),
                                            feeRate = fixture.feeRate)

    assert(selection == Vector(fixture.utxo1, fixture.utxo3, fixture.utxo2))
  }

  it must "accumulate outputs in order" in { fixture =>
    val selection = CoinSelector.accumulate(walletUtxos = fixture.utxoSet,
                                            outputs = Vector(fixture.output),
                                            feeRate = fixture.feeRate)

    assert(selection == Vector(fixture.utxo1, fixture.utxo2))
  }

  it must "accumulate random outputs" in { fixture =>
    val first = CoinSelector.randomSelection(walletUtxos = fixture.utxoSet,
                                             outputs = Vector(fixture.output),
                                             feeRate = fixture.feeRate)

    val selections = Vector.fill(20)(
      CoinSelector.randomSelection(walletUtxos = fixture.utxoSet,
                                   outputs = Vector(fixture.output),
                                   feeRate = fixture.feeRate))

    // it should not get the same thing every time
    assert(selections.exists(_ != first))
  }

  it must "select by branch and bound" in { fixture =>
    val selection = CoinSelector.branchAndBound(walletUtxos = fixture.utxoSet,
                                                outputs =
                                                  Vector(fixture.output),
                                                feeRate = fixture.feeRate,
                                                changeCost = Satoshis(1))

    val expected = Vector(fixture.utxo1, fixture.utxo2).sortBy(_.value)

    assert(selection.sortBy(_.value) == expected)
  }

  it must "select the least wasteful outputs" in { fixture =>
    val selection =
      CoinSelector.selectByLeastWaste(
        walletUtxos = fixture.utxoSet,
        outputs = Vector(fixture.output),
        feeRate = fixture.feeRate,
        changeCost = Satoshis.one,
        longTermFeeRate = SatoshisPerByte.fromLong(10)
      )

    // Need to sort as ordering will be different sometimes
    val sortedSelection = selection.sortBy(_.outPoint.hex)
    val sortedExpected =
      Vector(fixture.utxo2, fixture.utxo1, fixture.utxo3).sortBy(_.outPoint.hex)

    assert(sortedSelection == sortedExpected)
  }

  it must "correctly approximate transaction input size" in { fixture =>
    val expected1 =
      32 + 4 + 1 + 4 + fixture.utxo1.scriptWitnessOpt.get.bytes.length
    val expected2 =
      32 + 4 + 1 + 4 + fixture.utxo2.scriptWitnessOpt.get.bytes.length
    val expected3 =
      32 + 4 + 1 + 4 + fixture.utxo3.scriptWitnessOpt.get.bytes.length

    assert(CoinSelector.approximateUtxoBytesSize(fixture.utxo1) == expected1)
    assert(CoinSelector.approximateUtxoBytesSize(fixture.utxo2) == expected2)
    assert(CoinSelector.approximateUtxoBytesSize(fixture.utxo3) == expected3)
  }
}
