package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.wallet.internal.RescanHandling

import scala.concurrent.{ExecutionContext, Future}

private[bitcoins] trait DLCRescanHandling extends RescanHandling {
  _: DLCWallet =>

  override def rescanNeutrinoWallet(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean)(implicit
      ec: ExecutionContext): Future[RescanState] = {
    val accountF = getDefaultAccount()
    val fundingSPKsF = dlcDataManagement.getAllFundingSPKs()
    val payoutSPKsF = dlcDataManagement.getAllPayoutSPKs()

    for {
      fundingSPKs <- fundingSPKsF
      payoutSPKs <- payoutSPKsF
      account <- accountF
      rescanState <- super.rescanNeutrinoWallet(
        account = account.hdAccount,
        startOpt = startOpt,
        endOpt = endOpt,
        addressBatchSize = addressBatchSize,
        externalScriptPubKeys = fundingSPKs ++ payoutSPKs,
        useCreationTime = useCreationTime
      )
    } yield rescanState
  }
}
