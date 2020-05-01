package org.bitcoins.dlc

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.crypto.{DoubleSha256DigestBE, Sha256DigestBE}

case class SetupDLC(
    fundingTx: Transaction,
    cets: Map[Sha256DigestBE, CETInfo],
    refundTx: Transaction) {
  cets.foreach {
    case (msg, cetInfo) =>
      require(
        cetInfo.tx.inputs.size == 1,
        s"CETs should only spend the funding input, local CET for $msg has ${cetInfo.tx.inputs.size} inputs")
      require(
        cetInfo.tx.inputs.head.previousOutput == TransactionOutPoint(
          fundingTx.txId,
          UInt32.zero),
        s"CET is not spending the funding input, ${cetInfo.tx.inputs.head}"
      )
  }
  require(
    refundTx.inputs.size == 1,
    s"RefundTx should only spend the funding input, refundTx has ${refundTx.inputs.size} inputs")
  require(
    refundTx.inputs.head.previousOutput == TransactionOutPoint(fundingTx.txId,
                                                               UInt32.zero),
    s"RefundTx is not spending the funding input, ${refundTx.inputs.head}"
  )
}

case class CETInfo(
    tx: Transaction,
    witness: P2WSHWitnessV0,
    remoteTxid: DoubleSha256DigestBE,
    remoteWitness: P2WSHWitnessV0)
