package org.bitcoins.core.wallet.builder

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}

import scala.concurrent.{ExecutionContext, Future}

/** Contains a finalized tx (output from [[RawTxFinalizer.buildTx]]) and the
  * ScriptSignatureParams needed to sign that transaction. */
case class FinalizedTxWithSigningInfo(
    finalizedTx: Transaction,
    infos: Vector[ScriptSignatureParams[InputInfo]]) {

  def sign(expectedFeeRate: FeeUnit)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    RawTxSigner.sign(this, expectedFeeRate)
  }

  def sign(
      expectedFeeRate: FeeUnit,
      invariants: (
          Vector[ScriptSignatureParams[InputInfo]],
          Transaction) => Boolean)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    RawTxSigner.sign(this, expectedFeeRate, invariants)
  }
}
