package org.bitcoins.wallet.api

import org.bitcoins.core.currency._
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerByte}
import org.bitcoins.testkit.core.gen.{TransactionGenerators, WitnessGenerators}
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.scalatest.FutureOutcome
import org.bitcoins.wallet.models.SpendingInfoDb
import org.bitcoins.wallet.models.SegwitV0SpendingInfo

class CoinSelectorTest extends BitcoinSWalletTest {
  case class CoinSelectionFixture(
      output: TransactionOutput,
      feeRate: FeeUnit,
      utxo1: SpendingInfoDb,
      utxo2: SpendingInfoDb,
      utxo3: SpendingInfoDb) {
    val utxoSet: Vector[SpendingInfoDb] = Vector(utxo1, utxo2, utxo3)
  }

  override type FixtureParam = CoinSelectionFixture

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val output = TransactionOutput(99.sats, ScriptPubKey.empty)
    val feeRate = SatoshisPerByte(CurrencyUnits.zero)

    val utxo1 = SegwitV0SpendingInfo(
      id = Some(1),
      outPoint = TransactionGenerators.outPoint.sample.get,
      output = TransactionOutput(10.sats, ScriptPubKey.empty),
      privKeyPath = WalletTestUtil.sampleSegwitPath,
      scriptWitness = WitnessGenerators.scriptWitness.sample.get
    )
    val utxo2 = SegwitV0SpendingInfo(
      id = Some(2),
      outPoint = TransactionGenerators.outPoint.sample.get,
      output = TransactionOutput(90.sats, ScriptPubKey.empty),
      privKeyPath = WalletTestUtil.sampleSegwitPath,
      scriptWitness = WitnessGenerators.scriptWitness.sample.get
    )
    val utxo3 = SegwitV0SpendingInfo(
      id = Some(3),
      outPoint = TransactionGenerators.outPoint.sample.get,
      output = TransactionOutput(20.sats, ScriptPubKey.empty),
      privKeyPath = WalletTestUtil.sampleSegwitPath,
      scriptWitness = WitnessGenerators.scriptWitness.sample.get
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

  it must "correctly approximate transaction input size" in { fixture =>
    val expected1 = 32 + 4 + 1 + 4 + fixture.utxo1.scriptWitnessOpt.get.bytes.length
    val expected2 = 32 + 4 + 1 + 4 + fixture.utxo2.scriptWitnessOpt.get.bytes.length
    val expected3 = 32 + 4 + 1 + 4 + fixture.utxo3.scriptWitnessOpt.get.bytes.length

    assert(CoinSelector.approximateUtxoSize(fixture.utxo1) == expected1)
    assert(CoinSelector.approximateUtxoSize(fixture.utxo2) == expected2)
    assert(CoinSelector.approximateUtxoSize(fixture.utxo3) == expected3)
  }
}
