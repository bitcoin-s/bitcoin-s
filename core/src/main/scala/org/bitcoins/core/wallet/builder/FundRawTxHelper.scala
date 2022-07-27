package org.bitcoins.core.wallet.builder

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}

case class FundRawTxHelper[T <: RawTxFinalizer](
    txBuilderWithFinalizer: RawTxBuilderWithFinalizer[T],
    scriptSigParams: Vector[ScriptSignatureParams[InputInfo]],
    feeRate: FeeUnit) {

  /** Produces the unsigned transaction built by fundrawtransaction */
  def unsignedTx: Transaction = txBuilderWithFinalizer.buildTx()

  /** Produces a signed bitcoin transaction with the given fee rate */
  def signedTx: Transaction = {
    RawTxSigner.sign(unsignedTx, scriptSigParams, feeRate)
  }
}
