package org.bitcoins.server

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.crypto.{
  AesPassword,
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  MnemonicCode
}
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.hd.{AddressType, HDPurpose}
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{EmptyTransaction, Transaction}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.node.SpvNodeCallbacks
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{AccountDb, AddressDb, SpendingInfoDb}

import scala.concurrent.{ExecutionContext, Future}

object MockObjects {

  val testAddress = "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"

  val mockWalletApi = new UnlockedWalletApi {
    override def mnemonicCode: MnemonicCode = ???

    override def passphrase: AesPassword = ???

    override def lock(): LockedWalletApi = ???

    override def getBalance(): Future[CurrencyUnit] =
      Future.successful(Bitcoins(50))

    override def sendToAddress(
        address: BitcoinAddress,
        amount: CurrencyUnit,
        feeRate: FeeUnit,
        fromAccount: AccountDb): Future[Transaction] =
      Future.successful(EmptyTransaction)

    override def sendToAddress(
        address: BitcoinAddress,
        amount: CurrencyUnit,
        feeRate: FeeUnit
    ): Future[Transaction] = Future.successful(EmptyTransaction)

    override def createNewAccount(purpose: HDPurpose): Future[WalletApi] = ???

    override def createNewAccount(): Future[WalletApi] = ???

    override def getBloomFilter(): Future[BloomFilter] = ???

    override def processTransaction(
        transaction: Transaction,
        confirmations: Int): Future[LockedWalletApi] = ???

    override def getConfirmedBalance(): Future[CurrencyUnit] = ???

    override def getUnconfirmedBalance(): Future[CurrencyUnit] = ???

    override def listUtxos(): Future[Vector[SpendingInfoDb]] = ???

    override def listAddresses(): Future[Vector[AddressDb]] = ???

    override def getNewAddress(
        addressType: AddressType): Future[BitcoinAddress] = getNewAddress()

    override def getNewAddress(): Future[BitcoinAddress] = {
      val genesisAddress = BitcoinAddress(testAddress).get
      Future.successful(genesisAddress)
    }

    override def getAddressInfo(
        address: BitcoinAddress): Future[Option[AddressInfo]] = ???

    override protected def getNewChangeAddress(
        account: AccountDb): Future[BitcoinAddress] = ???

    override protected def getDefaultAccount(): Future[AccountDb] = ???

    override protected def getDefaultAccountForType(
        addressType: AddressType): Future[AccountDb] = ???

    override def unlock(passphrase: AesPassword): UnlockWalletResult = ???

    override def listAccounts(): Future[Vector[AccountDb]] = ???

    implicit override val walletConfig: WalletAppConfig = null

    implicit override val ec: ExecutionContext = null
  }

  val mockNode = new org.bitcoins.node.Node {
    implicit override def system: ActorSystem = ???

    implicit override def nodeAppConfig: NodeAppConfig = ???

    implicit override def chainAppConfig: ChainAppConfig = ???

    override val peer: Peer = null
    override val callbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty

    override def broadcastTransaction(
        transaction: Transaction): Future[Unit] = {
      FutureUtil.unit
    }

    override def rescan(
        scriptPubKeysToWatch: Vector[ScriptPubKey],
        startOpt: Option[BlockStamp],
        endOpt: Option[BlockStamp]): Future[Unit] = {
      FutureUtil.unit
    }
  }

  val mockChainApi = new ChainApi {

    implicit val chainConfig: ChainAppConfig = null

    override def processHeaders(headers: Vector[BlockHeader])(
        implicit ec: ExecutionContext): Future[ChainApi] = ???

    override def getHeader(hash: DoubleSha256DigestBE)(
        implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]] = ???

    override def getHeadersAtHeight(height: Int)(
        implicit ec: ExecutionContext): Future[Vector[BlockHeaderDb]] = ???

    override def getBlockCount(implicit ec: ExecutionContext): Future[Int] =
      Future.successful(1234567890)

    override def getBestBlockHash(
        implicit ec: ExecutionContext): Future[DoubleSha256DigestBE] =
      Future.successful(DoubleSha256DigestBE.empty)

    override def processFilterHeaders(
        filterHeaders: Vector[FilterHeader],
        stopHash: DoubleSha256DigestBE)(
        implicit ec: ExecutionContext): Future[ChainApi] = ???

    override def nextHeaderBatchRange(
        stopHash: DoubleSha256DigestBE,
        batchSize: Int)(implicit ec: ExecutionContext): Future[
      Option[(Int, DoubleSha256Digest)]] = ???

    override def nextFilterHeaderBatchRange(
        stopHash: DoubleSha256DigestBE,
        batchSize: Int)(implicit ec: ExecutionContext): Future[
      Option[(Int, DoubleSha256Digest)]] = ???

    override def processFilters(message: Vector[CompactFilterMessage])(
        implicit ec: ExecutionContext): Future[ChainApi] = ???

    override def processCheckpoints(
        checkpoints: Vector[DoubleSha256DigestBE],
        blockHash: DoubleSha256DigestBE)(
        implicit ec: ExecutionContext): Future[ChainApi] = ???

    override def getFilterHeaderCount(
        implicit ec: ExecutionContext): Future[Int] = ???

    override def getFilterHeadersAtHeight(height: Int)(
        implicit ec: ExecutionContext): Future[Vector[CompactFilterHeaderDb]] =
      ???

    override def getFilterHeader(blockHash: DoubleSha256DigestBE)(
        implicit ec: ExecutionContext): Future[Option[CompactFilterHeaderDb]] =
      ???

    override def getFilter(hash: DoubleSha256DigestBE)(
        implicit ec: ExecutionContext): Future[Option[CompactFilterDb]] = ???

    override def getFilterCount(implicit ec: ExecutionContext): Future[Int] =
      ???

    override def getFiltersAtHeight(height: Int)(
        implicit ec: ExecutionContext): Future[Vector[CompactFilterDb]] = ???

    override def getMatchingBlocks(
        scripts: Vector[ScriptPubKey],
        startOpt: Option[BlockStamp],
        endOpt: Option[BlockStamp],
        batchSize: Int,
        parallelismLevel: Int)(
        implicit ec: ExecutionContext): Future[Vector[DoubleSha256DigestBE]] =
      ???

    override def getHeightByBlockStamp(blockStamp: BlockStamp)(
        implicit ec: ExecutionContext): Future[Int] = ???
  }

}
