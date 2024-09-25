package org.bitcoins.wallet

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi
import org.bitcoins.core.api.dlc.wallet.db.{
  DLCContactDb,
  DLCDb,
  IncomingDLCOfferDb
}
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.*
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.dlc.accounting.DLCWalletAccounting
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.models.*
import org.bitcoins.core.protocol.tlv.*
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.crypto.{DoubleSha256DigestBE, Sha256Digest}
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

class WalletNotInitialized extends Exception("The wallet is not initialized")

class WalletHolder(initWalletOpt: Option[DLCNeutrinoHDWalletApi])(implicit
    ec: ExecutionContext
) extends DLCNeutrinoHDWalletApi
    with BitcoinSLogger {

  @volatile private var walletOpt: Option[DLCNeutrinoHDWalletApi] =
    initWalletOpt

  private def wallet: DLCNeutrinoHDWalletApi = synchronized {
    walletOpt match {
      case Some(wallet) => wallet
      case None =>
        throw new WalletNotInitialized
    }
  }

  override def accountHandling: AccountHandlingApi = wallet.accountHandling

  override def rescanHandling: RescanHandlingApi = wallet.rescanHandling

  override def fundTxHandling: FundTransactionHandlingApi =
    wallet.fundTxHandling

  override def utxoHandling: UtxoHandlingApi = wallet.utxoHandling

  override def addressHandling: AddressHandlingApi = wallet.addressHandling

  override def transactionProcessing: TransactionProcessingApi =
    wallet.transactionProcessing

  override def sendFundsHandling: SendFundsHandlingApi =
    wallet.sendFundsHandling
  def isInitialized: Boolean = synchronized {
    walletOpt.isDefined
  }

  def replaceWallet(
      newWallet: DLCNeutrinoHDWalletApi
  ): Future[DLCNeutrinoHDWalletApi] =
    synchronized {
      walletOpt = Some(newWallet)
      Future.successful(newWallet)
    }

  private def delegate[T]
      : (DLCNeutrinoHDWalletApi => Future[T]) => Future[T] = {
    Future(wallet).flatMap[T](_)
  }

  override def getNewAddress(): Future[BitcoinAddress] = delegate(
    _.getNewAddress())

  override def getNewChangeAddress(): Future[BitcoinAddress] = delegate(
    _.getNewChangeAddress())

  override def processCompactFilters(
      blockFilters: Vector[(DoubleSha256DigestBE, GolombFilter)]
  ): Future[NeutrinoHDWalletApi] = {
    delegate(_.processCompactFilters(blockFilters))
  }

  override def isRescanning(): Future[Boolean] = delegate(_.isRescanning())

  override lazy val nodeApi: NodeApi = wallet.nodeApi
  override lazy val chainQueryApi: ChainQueryApi = wallet.chainQueryApi
  override lazy val feeRateApi: FeeRateApi = wallet.feeRateApi
  override lazy val creationTime: Instant = wallet.creationTime

  override def getConfirmedBalance(): Future[CurrencyUnit] = delegate(
    _.getConfirmedBalance()
  )

  override def getUnconfirmedBalance(): Future[CurrencyUnit] = delegate(
    _.getUnconfirmedBalance()
  )

  override def isEmpty(): Future[Boolean] = delegate(_.isEmpty())

  override def getSyncState(): Future[BlockSyncState] = delegate(
    _.getSyncState()
  )

  override def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(
      contractInfo,
      collateral,
      feeRateOpt,
      refundLT,
      peerAddressOpt,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(
      contractInfo,
      collateral,
      feeRateOpt,
      locktime,
      refundLT,
      peerAddressOpt,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def acceptDLCOffer(
      dlcOffer: DLCMessage.DLCOffer,
      peerAddress: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCAccept] = delegate(
    _.acceptDLCOffer(
      dlcOffer,
      peerAddress,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def signDLC(acceptTLV: DLCAcceptTLV): Future[DLCMessage.DLCSign] =
    delegate(_.signDLC(acceptTLV))

  override def signDLC(
      accept: DLCMessage.DLCAccept
  ): Future[DLCMessage.DLCSign] = delegate(_.signDLC(accept))

  override def addDLCSigs(signTLV: DLCSignTLV): Future[DLCDb] = delegate(
    _.addDLCSigs(signTLV)
  )

  override def addDLCSigs(sigs: DLCMessage.DLCSign): Future[DLCDb] = delegate(
    _.addDLCSigs(sigs)
  )

  override def getDLCFundingTx(contractId: ByteVector): Future[Transaction] =
    delegate(_.getDLCFundingTx(contractId))

  override def broadcastDLCFundingTx(
      contractId: ByteVector
  ): Future[Transaction] = delegate(_.broadcastDLCFundingTx(contractId))

  override def executeDLC(
      contractId: ByteVector,
      oracleSigs: Seq[OracleAttestmentTLV]
  ): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSigs))

  override def executeDLC(
      contractId: ByteVector,
      oracleSigs: Vector[OracleSignatures]
  ): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSigs))

  override def executeDLCRefund(contractId: ByteVector): Future[Transaction] =
    delegate(_.executeDLCRefund(contractId))

  override def listDLCs(states: Vector[DLCState]): Future[Vector[DLCStatus]] = {
    delegate(_.listDLCs(states))
  }
  override def listDLCs(): Future[Vector[DLCStatus]] = delegate(_.listDLCs())

  override def findDLC(dlcId: Sha256Digest): Future[Option[DLCStatus]] =
    delegate(_.findDLC(dlcId))

  override def findDLCByTemporaryContractId(
      tempContractId: Sha256Digest
  ): Future[Option[DLCStatus]] = delegate(
    _.findDLCByTemporaryContractId(tempContractId)
  )

  override def cancelDLC(dlcId: Sha256Digest): Future[Unit] = delegate(
    _.cancelDLC(dlcId)
  )

  override def getDLCOffer(
      dlcId: Sha256Digest
  ): Future[Option[DLCMessage.DLCOffer]] = delegate(_.getDLCOffer(dlcId))

  override def getWalletAccounting(): Future[DLCWalletAccounting] = delegate(
    _.getWalletAccounting()
  )

  override def registerIncomingDLCOffer(
      offerTLV: DLCOfferTLV,
      peer: Option[String],
      message: Option[String]
  ): Future[Sha256Digest] = delegate(
    _.registerIncomingDLCOffer(offerTLV, peer, message)
  )

  override def listIncomingDLCOffers(): Future[Vector[IncomingDLCOfferDb]] =
    delegate(_.listIncomingDLCOffers())

  override def rejectIncomingDLCOffer(offerHash: Sha256Digest): Future[Unit] =
    delegate(_.rejectIncomingDLCOffer(offerHash))

  override def findIncomingDLCOffer(
      offerHash: Sha256Digest
  ): Future[Option[IncomingDLCOfferDb]] = delegate(
    _.findIncomingDLCOffer(offerHash)
  )

  override def listDLCContacts(): Future[Vector[DLCContactDb]] = delegate(
    _.listDLCContacts()
  )

  override def addDLCContact(contact: DLCContactDb): Future[Unit] = delegate(
    _.addDLCContact(contact)
  )

  override def removeDLCContact(address: InetSocketAddress): Future[Unit] =
    delegate(_.removeDLCContact(address))

  override def findDLCContacts(alias: String): Future[Vector[DLCContactDb]] =
    delegate(_.findDLCContacts(alias))

  override def addDLCContactMapping(
      dlcId: Sha256Digest,
      contactId: InetSocketAddress
  ): Future[Unit] = delegate(_.addDLCContactMapping(dlcId, contactId))

  override def removeDLCContactMapping(dlcId: Sha256Digest): Future[Unit] =
    delegate(_.removeDLCContactMapping(dlcId))

  override def listDLCsByContact(
      address: InetSocketAddress
  ): Future[Vector[DLCStatus]] = delegate(_.listDLCsByContact(address))

  def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit] =
    delegate(_.accountHandling.getConfirmedBalance(account))

  def getUnconfirmedBalance(account: HDAccount): Future[CurrencyUnit] =
    delegate(_.accountHandling.getUnconfirmedBalance(account))

  override def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]] =
    delegate(_.getSyncDescriptorOpt())

  override def getWalletName(): Future[String] = delegate(_.getWalletName())

  override def getInfo(): Future[WalletInfo] = delegate(_.getInfo())

  override def broadcastTransaction(transaction: Transaction): Future[Unit] =
    delegate(_.broadcastTransaction(transaction))

  override def getFeeRate(): Future[FeeUnit] = delegate(_.getFeeRate())

  override def getBalance()(implicit
      ec: ExecutionContext
  ): Future[CurrencyUnit] = delegate(_.getBalance())

  def getBalance(account: HDAccount)(implicit
      ec: ExecutionContext
  ): Future[CurrencyUnit] = delegate(_.accountHandling.getBalance(account))

  override def processCompactFilter(
      blockHash: DoubleSha256DigestBE,
      blockFilter: GolombFilter
  ): Future[NeutrinoHDWalletApi] =
    delegate(_.processCompactFilter(blockHash, blockFilter))

  override def createDLCOffer(
      contractInfoTLV: ContractInfoTLV,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(
      contractInfoTLV,
      collateral,
      feeRateOpt,
      refundLT,
      peerAddressOpt,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def createDLCOffer(
      contractInfoTLV: ContractInfoTLV,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(
      contractInfoTLV,
      collateral,
      feeRateOpt,
      locktime,
      refundLT,
      peerAddressOpt,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def acceptDLCOffer(
      dlcOfferTLV: DLCOfferTLV,
      peerAddress: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCAccept] = delegate(
    _.acceptDLCOffer(
      dlcOfferTLV,
      peerAddress,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def executeDLC(
      contractId: ByteVector,
      oracleSig: OracleAttestmentTLV
  ): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSig))

  override def executeDLC(
      contractId: ByteVector,
      oracleSig: OracleSignatures
  ): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSig))
}

object WalletHolder {

  def empty(implicit ec: ExecutionContext): WalletHolder = new WalletHolder(
    None
  )
}
