package org.bitcoins.dlc

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}

case class SetupDLC(
    fundingTx: Transaction,
    cetWin: Transaction,
    cetWinWitness: P2WSHWitnessV0,
    cetLose: Transaction,
    cetLoseWitness: P2WSHWitnessV0,
    cetWinRemoteTxid: DoubleSha256DigestBE,
    cetWinRemoteWitness: P2WSHWitnessV0,
    cetLoseRemoteTxid: DoubleSha256DigestBE,
    cetLoseRemoteWitness: P2WSHWitnessV0,
    refundTx: Transaction) {
  require(
    cetWin.inputs.size == 1,
    s"CETs should only spend the funding input, cetWin has ${cetWin.inputs.size} inputs")
  require(
    cetWin.inputs.head.previousOutput == TransactionOutPoint(fundingTx.txId,
                                                             UInt32.zero),
    s"CETWin is not spending the funding input, ${cetWin.inputs.head}")
  require(
    cetLose.inputs.size == 1,
    s"CETs should only spend the funding input, cetLose has ${cetLose.inputs.size} inputs")
  require(
    cetLose.inputs.head.previousOutput == TransactionOutPoint(fundingTx.txId,
                                                              UInt32.zero),
    s"CETLose is not spending the funding input, ${cetLose.inputs.head}"
  )
  require(
    refundTx.inputs.size == 1,
    s"RefundTx should only spend the funding input, refundTx has ${refundTx.inputs.size} inputs")
  require(
    refundTx.inputs.head.previousOutput == TransactionOutPoint(fundingTx.txId,
                                                               UInt32.zero),
    s"RefundTx is not spending the funding input, ${refundTx.inputs.head}"
  )
}
