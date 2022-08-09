package org.bitcoins.core.api.dlc.wallet

import org.bitcoins.core.api.dlc.wallet.db.{
  DLCContactDb,
  DLCDb,
  IncomingDLCOfferDb
}
import org.bitcoins.core.api.wallet._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.dlc.accounting._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.Sha256Digest
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import scala.concurrent._

trait DLCWalletApi { self: WalletApi =>

  def createDLCOffer(
      contractInfoTLV: ContractInfoTLV,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      refundLT: UInt32,
      peerAddressOpt: Option[java.net.InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): Future[DLCOffer] = {
    val contractInfo = ContractInfo.fromTLV(contractInfoTLV)
    createDLCOffer(contractInfo,
                   collateral,
                   feeRateOpt,
                   refundLT,
                   peerAddressOpt,
                   externalPayoutAddressOpt,
                   externalChangeAddressOpt)
  }

  def createDLCOffer(
      contractInfoTLV: ContractInfoTLV,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      peerAddressOpt: Option[java.net.InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): Future[DLCOffer] = {
    val contractInfo = ContractInfo.fromTLV(contractInfoTLV)
    createDLCOffer(contractInfo,
                   collateral,
                   feeRateOpt,
                   locktime,
                   refundLT,
                   peerAddressOpt,
                   externalPayoutAddressOpt,
                   externalChangeAddressOpt)
  }

  def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      refundLT: UInt32,
      peerAddressOpt: Option[java.net.InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): Future[DLCOffer]

  def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      peerAddressOpt: Option[java.net.InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): Future[DLCOffer]

  def acceptDLCOffer(
      dlcOfferTLV: DLCOfferTLV,
      peerAddress: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): Future[DLCAccept] = {
    acceptDLCOffer(DLCOffer.fromTLV(dlcOfferTLV),
                   peerAddress,
                   externalPayoutAddressOpt,
                   externalChangeAddressOpt)
  }

  def acceptDLCOffer(
      dlcOffer: DLCOffer,
      peerAddress: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): Future[DLCAccept]

  def signDLC(acceptTLV: DLCAcceptTLV): Future[DLCSign]

  def signDLC(accept: DLCAccept): Future[DLCSign]

  def addDLCSigs(signTLV: DLCSignTLV): Future[DLCDb]

  def addDLCSigs(sigs: DLCSign): Future[DLCDb]

  def getDLCFundingTx(contractId: ByteVector): Future[Transaction]

  def broadcastDLCFundingTx(contractId: ByteVector): Future[Transaction]

  /** Creates the CET for the given contractId and oracle signature, does not broadcast it */
  def executeDLC(
      contractId: ByteVector,
      oracleSig: OracleAttestmentTLV): Future[Option[Transaction]] =
    executeDLC(contractId, Vector(oracleSig))

  /** Creates the CET for the given contractId and oracle signature, does not broadcast it */
  def executeDLC(
      contractId: ByteVector,
      oracleSigs: Seq[OracleAttestmentTLV]): Future[Option[Transaction]]

  /** Creates the CET for the given contractId and oracle signature, does not broadcast it */
  def executeDLC(
      contractId: ByteVector,
      oracleSig: OracleSignatures): Future[Option[Transaction]] =
    executeDLC(contractId, Vector(oracleSig))

  /** Creates the CET for the given contractId and oracle signature, does not broadcast it */
  def executeDLC(
      contractId: ByteVector,
      oracleSigs: Vector[OracleSignatures]): Future[Option[Transaction]]

  /** Creates the refund transaction for the given contractId, does not broadcast it */
  def executeDLCRefund(contractId: ByteVector): Future[Transaction]

  /** Fetches all DLCs with the given set of states */
  def listDLCs(states: Vector[DLCState]): Future[Vector[DLCStatus]]

  def listDLCs(): Future[Vector[DLCStatus]]

  def findDLC(dlcId: Sha256Digest): Future[Option[DLCStatus]]

  def findDLCByTemporaryContractId(
      tempContractId: Sha256Digest): Future[Option[DLCStatus]]

  def cancelDLC(dlcId: Sha256Digest): Future[Unit]

  def getDLCOffer(dlcId: Sha256Digest): Future[Option[DLCOffer]]

  /** Retrieves accounting and financial metrics for the entire dlc wallet */
  def getWalletAccounting(): Future[DLCWalletAccounting]

  def registerIncomingDLCOffer(
      offerTLV: DLCOfferTLV,
      peer: Option[String],
      message: Option[String]): Future[Sha256Digest]

  def listIncomingDLCOffers(): Future[Vector[IncomingDLCOfferDb]]

  def rejectIncomingDLCOffer(offerHash: Sha256Digest): Future[Unit]

  def findIncomingDLCOffer(
      offerHash: Sha256Digest): Future[Option[IncomingDLCOfferDb]]

  def listDLCContacts(): Future[Vector[DLCContactDb]]

  def addDLCContact(contact: DLCContactDb): Future[Unit]

  def removeDLCContact(address: InetSocketAddress): Future[Unit]

  def findDLCContacts(alias: String): Future[Vector[DLCContactDb]]

  def addDLCContactMapping(
      dlcId: Sha256Digest,
      contactId: InetSocketAddress): Future[Unit]

  def removeDLCContactMapping(dlcId: Sha256Digest): Future[Unit]

  def listDLCsByContact(address: InetSocketAddress): Future[Vector[DLCStatus]]
}

/** An HDWallet that supports DLCs and Neutrino method of syncing */
trait DLCNeutrinoHDWalletApi extends NeutrinoHDWalletApi with DLCWalletApi
