package org.bitcoins.wallet.api

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import org.bitcoins.wallet.models.DLCDb

import scala.concurrent.Future

trait DLCWalletApi { self: WalletApi =>

  def createDLCOffer(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[FeeUnit],
      locktime: UInt32,
      refundLT: UInt32): Future[DLCOffer]

  def registerDLCOffer(dlcOffer: DLCOffer): Future[DLCOffer] = {
    createDLCOffer(
      dlcOffer.oracleInfo,
      dlcOffer.contractInfo,
      dlcOffer.totalCollateral,
      Some(dlcOffer.feeRate),
      dlcOffer.timeouts.contractMaturity.toUInt32,
      dlcOffer.timeouts.contractTimeout.toUInt32
    )
  }

  def acceptDLCOffer(dlcOffer: DLCOffer): Future[DLCAccept]

  def signDLC(accept: DLCAccept): Future[DLCSign]

  def addDLCSigs(sigs: DLCSign): Future[DLCDb]

  def getDLCFundingTx(eventId: Sha256DigestBE): Future[Transaction]

  def broadcastDLCFundingTx(eventId: Sha256DigestBE): Future[Transaction]

  /** Creates the CET for the given eventId and oracle signature, does not broadcast it */
  def executeDLC(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[Transaction]

  /** Creates the refund transaction for the given eventId, does not broadcast it */
  def executeDLCRefund(eventId: Sha256DigestBE): Future[Transaction]

}
