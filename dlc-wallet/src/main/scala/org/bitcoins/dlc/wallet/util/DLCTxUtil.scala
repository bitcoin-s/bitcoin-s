package org.bitcoins.dlc.wallet.util

import org.bitcoins.core.api.wallet.db.TransactionDb
import org.bitcoins.core.protocol.dlc.models.DLCFundingInput
import org.bitcoins.dlc.wallet.models.DLCFundingInputDb

object DLCTxUtil {

  /** Takes in a list of inputs to fund DLCs, and pairs them with the full funding transaction for this input
    * and then converts the input tx pair to a [[DLCFundingInput]]
    * @throws NoSuchElementException when we have an input we cannot find the funding transaction for
    */
  def matchPrevTxsWithInputs(
      inputs: Vector[DLCFundingInputDb],
      prevTxs: Vector[TransactionDb]): Vector[DLCFundingInput] = {
    inputs.sortBy(_.index).map { i =>
      prevTxs.find(_.txId == i.outPoint.txId) match {
        case Some(txDb) => i.toFundingInput(txDb.transaction)
        case None =>
          throw new NoSuchElementException(
            s"Could not find previous transaction with txIdBE=${i.outPoint.txId.flip.hex}")
      }
    }
  }

}
