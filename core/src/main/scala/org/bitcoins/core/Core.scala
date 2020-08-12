package org.bitcoins.core

import org.bitcoins.core.api.core.CoreApi
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.psbt.PSBT

import scala.concurrent.Future
import scala.util.{Failure, Success}

object Core extends CoreApi {

  override def combinePSBTs(psbts: Seq[PSBT]): Future[PSBT] = {
    if (psbts.isEmpty) {
      Future.failed(new IllegalArgumentException("No PSBTs given"))
    } else {
      try {
        val empty = PSBT.fromUnsignedTx(psbts.head.transaction)
        val combined =
          psbts.foldLeft(empty)((accum, psbt) => accum.combinePSBT(psbt))

        Future.successful(combined)
      } catch {
        case err: IllegalArgumentException =>
          Future.failed(err)
      }
    }
  }

  override def finalizePSBT(psbt: PSBT): Future[PSBT] = {
    psbt.finalizePSBT match {
      case Success(finalized) =>
        Future.successful(finalized)
      case Failure(err) =>
        Future.failed(err)
    }
  }

  override def extractFromPSBT(psbt: PSBT): Future[Transaction] = {
    psbt.extractTransactionAndValidate match {
      case Success(extracted) =>
        Future.successful(extracted)
      case Failure(err) =>
        Future.failed(err)
    }
  }

  override def convertToPSBT(transaction: Transaction): Future[PSBT] = {
    try {
      val psbt = PSBT.fromUnsignedTx(transaction)

      Future.successful(psbt)
    } catch {
      case err: IllegalArgumentException =>
        Future.failed(err)
    }
  }
}
