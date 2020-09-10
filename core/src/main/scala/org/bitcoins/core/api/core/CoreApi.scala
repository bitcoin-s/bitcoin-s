package org.bitcoins.core.api.core

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.psbt.PSBT

import scala.concurrent.Future

trait CoreApi {
  def combinePSBTs(psbts: Seq[PSBT]): Future[PSBT]

  def joinPSBTs(psbts: Seq[PSBT]): Future[PSBT] = combinePSBTs(psbts)

  def finalizePSBT(psbt: PSBT): Future[PSBT]

  def extractFromPSBT(psbt: PSBT): Future[Transaction]

  def convertToPSBT(transaction: Transaction): Future[PSBT]
}
