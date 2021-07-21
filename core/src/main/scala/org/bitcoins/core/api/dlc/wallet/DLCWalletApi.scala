package org.bitcoins.core.api.dlc.wallet

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.wallet._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.dlc.accounting._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.Sha256Digest
import scodec.bits.ByteVector

import scala.concurrent._

trait DLCWalletApi { self: WalletApi =>

  def createDLCOffer(
      contractInfoTLV: ContractInfoV0TLV,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32): Future[DLCOffer] = {
    val contractInfo = ContractInfo.fromTLV(contractInfoTLV)
    createDLCOffer(contractInfo, collateral, feeRateOpt, locktime, refundLT)
  }

  def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32): Future[DLCOffer]

  def registerDLCOffer(dlcOffer: DLCOffer): Future[DLCOffer] = {
    createDLCOffer(
      dlcOffer.contractInfo,
      dlcOffer.totalCollateral,
      Some(dlcOffer.feeRate),
      dlcOffer.timeouts.contractMaturity.toUInt32,
      dlcOffer.timeouts.contractTimeout.toUInt32
    )
  }

  def acceptDLCOffer(dlcOfferTLV: DLCOfferTLV): Future[DLCAccept] = {
    acceptDLCOffer(DLCOffer.fromTLV(dlcOfferTLV))
  }

  def acceptDLCOffer(dlcOffer: DLCOffer): Future[DLCAccept]

  def signDLC(acceptTLV: DLCAcceptTLV): Future[DLCSign]

  def signDLC(accept: DLCAccept): Future[DLCSign]

  def addDLCSigs(signTLV: DLCSignTLV): Future[DLCDb]

  def addDLCSigs(sigs: DLCSign): Future[DLCDb]

  def getDLCFundingTx(contractId: ByteVector): Future[Transaction]

  def broadcastDLCFundingTx(contractId: ByteVector): Future[Transaction]

  /** Creates the CET for the given contractId and oracle signature, does not broadcast it */
  def executeDLC(
      contractId: ByteVector,
      oracleSig: OracleAttestmentTLV): Future[Transaction] =
    executeDLC(contractId, Vector(oracleSig))

  /** Creates the CET for the given contractId and oracle signature, does not broadcast it */
  def executeDLC(
      contractId: ByteVector,
      oracleSigs: Seq[OracleAttestmentTLV]): Future[Transaction]

  /** Creates the CET for the given contractId and oracle signature, does not broadcast it */
  def executeDLC(
      contractId: ByteVector,
      oracleSig: OracleSignatures): Future[Transaction] =
    executeDLC(contractId, Vector(oracleSig))

  /** Creates the CET for the given contractId and oracle signature, does not broadcast it */
  def executeDLC(
      contractId: ByteVector,
      oracleSigs: Vector[OracleSignatures]): Future[Transaction]

  /** Creates the refund transaction for the given contractId, does not broadcast it */
  def executeDLCRefund(contractId: ByteVector): Future[Transaction]

  def listDLCs(): Future[Vector[DLCStatus]]

  def findDLC(dlcId: Sha256Digest): Future[Option[DLCStatus]]

  def cancelDLC(dlcId: Sha256Digest): Future[Unit]

  /** Retrieves accounting and financial metrics for the entire dlc wallet */
  def getWalletAccounting(): Future[DLCWalletAccounting]
}

/** An HDWallet that supports DLCs and both Neutrino and SPV methods of syncing */
trait AnyDLCHDWalletApi
    extends HDWalletApi
    with DLCWalletApi
    with NeutrinoWalletApi
    with SpvWalletApi
