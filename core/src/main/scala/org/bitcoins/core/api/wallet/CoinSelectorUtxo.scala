package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.FeeUnit

case class CoinSelectorUtxo(
    prevOut: TransactionOutput,
    outPoint: TransactionOutPoint,
    redeemScriptOpt: Option[ScriptPubKey],
    scriptWitnessOpt: Option[ScriptWitness]) {
  val value: CurrencyUnit = prevOut.value

  def fee(feeUnit: FeeUnit): CurrencyUnit = {
    CoinSelector.calculateUtxoFee(this, feeUnit)
  }

  def effectiveValue(feeUnit: FeeUnit): CurrencyUnit = {
    value - fee(feeUnit)
  }
}

object CoinSelectorUtxo {

  def fromSpendingInfoDb(db: SpendingInfoDb): CoinSelectorUtxo = {
    CoinSelectorUtxo(db.output,
                     db.outPoint,
                     db.redeemScriptOpt,
                     db.scriptWitnessOpt)
  }
}
