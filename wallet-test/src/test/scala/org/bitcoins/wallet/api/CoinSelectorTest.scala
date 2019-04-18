package org.bitcoins.wallet.api

import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerByte}
import org.bitcoins.testkit.core.gen.{TransactionGenerators, WitnessGenerators}
import org.bitcoins.wallet.models.{
  NativeV0UTXOSpendingInfoDb,
  UTXOSpendingInfoDb
}
import org.bitcoins.wallet.util.{BitcoinSWalletTest, WalletTestUtil}
import org.scalatest.FutureOutcome

class CoinSelectorTest extends BitcoinSWalletTest {
  case class CoinSelectionFixture(
      output: TransactionOutput,
      feeRate: FeeUnit,
      utxo1: UTXOSpendingInfoDb,
      utxo2: UTXOSpendingInfoDb,
      utxo3: UTXOSpendingInfoDb) {
    val utxoSet: Vector[UTXOSpendingInfoDb] = Vector(utxo1, utxo2, utxo3)
  }

  override type FixtureParam = CoinSelectionFixture

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val output = TransactionOutput(Satoshis(Int64(99L)), ScriptPubKey.empty)
    val feeRate = SatoshisPerByte(CurrencyUnits.zero)

    val utxo1 = NativeV0UTXOSpendingInfoDb(
      id = Some(1),
      outPoint = TransactionGenerators.outPoint.sample.get,
      output = TransactionOutput(Satoshis(Int64(10)), ScriptPubKey.empty),
      privKeyPath = WalletTestUtil.sampleSegwitPath,
      scriptWitness = WitnessGenerators.scriptWitness.sample.get
    )
    val utxo2 = NativeV0UTXOSpendingInfoDb(
      id = Some(2),
      outPoint = TransactionGenerators.outPoint.sample.get,
      output = TransactionOutput(Satoshis(Int64(90)), ScriptPubKey.empty),
      privKeyPath = WalletTestUtil.sampleSegwitPath,
      scriptWitness = WitnessGenerators.scriptWitness.sample.get
    )
    val utxo3 = NativeV0UTXOSpendingInfoDb(
      id = Some(3),
      outPoint = TransactionGenerators.outPoint.sample.get,
      output = TransactionOutput(Satoshis(Int64(20)), ScriptPubKey.empty),
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
