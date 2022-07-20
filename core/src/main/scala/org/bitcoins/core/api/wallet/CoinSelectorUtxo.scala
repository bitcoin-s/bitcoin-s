package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._

case class CoinSelectorUtxo(
    prevOut: TransactionOutput,
    outPoint: TransactionOutPoint,
    redeemScriptOpt: Option[ScriptPubKey],
    scriptWitnessOpt: Option[ScriptWitness])

object CoinSelectorUtxo {

  def fromSpendingInfoDb(db: SpendingInfoDb): CoinSelectorUtxo = {
    CoinSelectorUtxo(db.output,
                     db.outPoint,
                     db.redeemScriptOpt,
                     db.scriptWitnessOpt)
  }
}
