package org.bitcoins.core.wallet

import org.bitcoins.core.api.wallet.CoinSelector
import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee._
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.testkitcore.gen.FeeUnitGen
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class CoinSelectorTest extends BitcoinSUnitTest {

  behavior of "CoinSelector"

  val utxos: Vector[SpendingInfoDb] =
    createSpendingInfoDbs(Vector(Bitcoins(1), Bitcoins(2)))
  val inAmt: CurrencyUnit = utxos.map(_.output.value).sum
  val target: Bitcoins = Bitcoins(2)
  val changeCost: Satoshis = Satoshis.one

  it must "calculate waste correctly" in {
    forAll(FeeUnitGen.satsPerVirtualByte, FeeUnitGen.satsPerVirtualByte) {
      case (feeRateA, feeRateB) =>
        val (feeRate, longTermFeeRate) = correctFeeRates(feeRateA, feeRateB)

        val utxosFee =
          utxos.map(u => CoinSelector.calculateUtxoFee(u, feeRate)).sum

        // difference of fee rate between feeRate and longTermFeeRate
        val feeRateDiff =
          SatoshisPerVirtualByte.fromLong(
            feeRate.toLong - longTermFeeRate.toLong)

        // difference fees it costs to spend our utxos between feeRate and longTermFeeRate
        val utxoFeeDiffs =
          utxos.map(u => CoinSelector.calculateUtxoFee(u, feeRateDiff)).sum

        // how much change we have
        val excess = inAmt - utxosFee - target

        // Waste with change is the change cost and difference between fee and long term fee
        val waste1 =
          CoinSelector.calculateSelectionWaste(utxos = utxos,
                                               changeCostOpt = Some(changeCost),
                                               target = target,
                                               feeRate = feeRate,
                                               longTermFeeRate =
                                                 longTermFeeRate)

        assert(waste1 == utxoFeeDiffs + changeCost)

        // Waste without change is the excess and difference between fee and long term fee
        val waste2 =
          CoinSelector.calculateSelectionWaste(utxos = utxos,
                                               changeCostOpt = None,
                                               target = target,
                                               feeRate = feeRate,
                                               longTermFeeRate =
                                                 longTermFeeRate)

        assert(waste2 == utxoFeeDiffs + excess)

        // Waste with change and fee == long term fee is just cost of change
        val waste3 =
          CoinSelector.calculateSelectionWaste(utxos = utxos,
                                               changeCostOpt = Some(changeCost),
                                               target = target,
                                               feeRate = feeRate,
                                               longTermFeeRate = feeRate)

        assert(waste3 == changeCost)

        // Waste without change and fee == long term fee is just the excess
        val waste4 =
          CoinSelector.calculateSelectionWaste(utxos = utxos,
                                               changeCostOpt = None,
                                               target = target,
                                               feeRate = feeRate,
                                               longTermFeeRate = feeRate)

        assert(waste4 == excess)

        // Waste will be greater when fee is greater, but long term fee is the same
        val biggerFeeRate = SatoshisPerVirtualByte(feeRate.currencyUnit * 2)
        val waste5 =
          CoinSelector.calculateSelectionWaste(utxos = utxos,
                                               changeCostOpt = Some(changeCost),
                                               target = target,
                                               feeRate = biggerFeeRate,
                                               longTermFeeRate =
                                                 longTermFeeRate)

        // waste 1 is the same params but feeRate
        assert(waste5 > waste1)

        // Waste with change is the change cost and difference between fee and long term fee
        // With long term fee greater than fee, waste should be less than when long term fee is less than fee
        val waste6 =
          CoinSelector.calculateSelectionWaste(utxos = utxos,
                                               changeCostOpt = Some(changeCost),
                                               target = target,
                                               feeRate = longTermFeeRate,
                                               longTermFeeRate = feeRate)

        assert(waste6 == -utxoFeeDiffs + changeCost)
        // waste 1 is the same params but feeRate and longTermFeeRate swapped
        assert(waste6 < waste1)

        // 0 Waste only when fee == long term fee, no change, and no excess
        val waste7 =
          CoinSelector.calculateSelectionWaste(utxos = utxos,
                                               changeCostOpt = None,
                                               target = inAmt - utxosFee,
                                               feeRate = feeRate,
                                               longTermFeeRate = feeRate)

        assert(waste7 == CurrencyUnits.zero)
    }
  }

  def createSpendingInfoDbs(
      amounts: Vector[CurrencyUnit]): Vector[SpendingInfoDb] = {
    val result = amounts.map { amt =>
      val key = ECPublicKey.freshPublicKey
      val spk = P2WPKHWitnessSPKV0(key)
      val output = TransactionOutput(amt, spk)
      val scriptWitness = P2WPKHWitnessV0(key)
      val path = SegWitHDPath(HDCoinType.Testnet,
                              accountIndex = 0,
                              HDChainType.External,
                              addressIndex = 0)

      SegwitV0SpendingInfo(
        outPoint = EmptyTransactionOutPoint,
        output = output,
        privKeyPath = path,
        scriptWitness = scriptWitness,
        txid = EmptyTransactionOutPoint.txIdBE,
        state = TxoState.ConfirmedReceived,
        spendingTxIdOpt = None
      )
    }
    result
  }

  /** The test assumes feeRate is greater than longTermFeeRate
    * After generating fee rates this will order them correctly
    * so the tests will pass
    *
    * @param feeRateA first fee rate generated
    * @param feeRateB second fee rate generated
    * @return (feeRate, longTermFeeRate)
    */
  def correctFeeRates(
      feeRateA: SatoshisPerVirtualByte,
      feeRateB: SatoshisPerVirtualByte): (
      SatoshisPerVirtualByte,
      SatoshisPerVirtualByte) = {
    if (feeRateA.toLong > feeRateB.toLong) {
      (feeRateA, feeRateB)
    } else if (feeRateA.toLong < feeRateB.toLong) {
      (feeRateB, feeRateA)
    } else { // equal
      // one them needs to be higher
      val plusOne = SatoshisPerVirtualByte.fromLong(feeRateA.toLong + 1)
      (plusOne, feeRateB)
    }
  }
}
