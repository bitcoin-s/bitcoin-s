package org.bitcoins.core.api.wallet

import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}

import scala.concurrent.{ExecutionContext, Future}

trait NeutrinoWalletApi { self: WalletApi =>

  def processCompactFilter(
      blockHash: DoubleSha256Digest,
      blockFilter: GolombFilter): Future[WalletApi with NeutrinoWalletApi] =
    processCompactFilters(Vector((blockHash, blockFilter)))

  def processCompactFilters(
      blockFilters: Vector[(DoubleSha256Digest, GolombFilter)]): Future[
    WalletApi with NeutrinoWalletApi]

  /** Recreates the account using BIP-157 approach
    *
    * DANGER! This method removes all records from the wallet database
    * and creates new ones while the account discovery process.
    *
    * The Wallet UI should check if the database is empty before calling
    * this method and let the end users to decide whether they want to proceed or not.
    *
    * This method generates [[addressBatchSize]] of addresses, then matches them against the BIP-158 compact filters,
    * and downloads and processes the matched blocks. This method keeps doing the steps until there are [[WalletConfig.addressGapLimit]]
    * or more unused addresses in a row. In this case it considers the discovery process completed.
    *
    * [[addressBatchSize]] - the number of addresses we should generate from a keychain to attempt to match in in a rescan
    * [[WalletConfig.addressGapLimit]] - the number of addresses required to go without a match before we determine that our wallet is "discovered".
    * For instance, if addressBatchSize=100, and AddressGapLimit=20 we do a rescan and the last address we find containing
    * funds is at index 75, we would not generate more addresses to try and rescan. However if the last index containing
    * funds was 81, we would generate another 100 addresses from the keychain and attempt to rescan those.
    *
    * @param startOpt start block (if None it starts from the genesis block)
    * @param endOpt end block (if None it ends at the current tip)
    * @param addressBatchSize how many addresses to match in a single pass
    */
  def rescanNeutrinoWallet(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean,
      force: Boolean)(implicit ec: ExecutionContext): Future[RescanState]

  /** Helper method to rescan the ENTIRE blockchain. */
  def fullRescanNeutrinoWallet(addressBatchSize: Int, force: Boolean = false)(
      implicit ec: ExecutionContext): Future[RescanState] =
    rescanNeutrinoWallet(startOpt = None,
                         endOpt = None,
                         addressBatchSize = addressBatchSize,
                         useCreationTime = false,
                         force = force)

  def discoveryBatchSize(): Int

}

object NeutrinoWalletApi {

  case class BlockMatchingResponse(
      blockHash: DoubleSha256DigestBE,
      blockHeight: Int)

}
