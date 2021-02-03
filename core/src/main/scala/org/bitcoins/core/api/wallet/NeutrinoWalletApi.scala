package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.NeutrinoWalletApi.BlockMatchingResponse
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}

import scala.concurrent.{ExecutionContext, Future}

trait NeutrinoWalletApi { self: WalletApi =>

  /** Processes the give block, updating our DB state if it's relevant to us.
    * @param block The block we're processing
    */
  def processBlock(block: Block): Future[WalletApi]

  def processCompactFilter(
      blockHash: DoubleSha256Digest,
      blockFilter: GolombFilter): Future[WalletApi] =
    processCompactFilters(Vector((blockHash, blockFilter)))

  def processCompactFilters(
      blockFilters: Vector[(DoubleSha256Digest, GolombFilter)]): Future[
    WalletApi]

  /** Iterates over the block filters in order to find filters that match to the given addresses
    *
    * I queries the filter database for [[batchSize]] filters a time
    * and tries to run [[GolombFilter.matchesAny]] for each filter.
    *
    * It tries to match the filters in parallel using [[parallelismLevel]] threads.
    * For best results use it with a separate execution context.
    *
    * @param scripts list of [[ScriptPubKey]]'s to watch
    * @param startOpt start point (if empty it starts with the genesis block)
    * @param endOpt end point (if empty it ends with the best tip)
    * @param batchSize number of filters that can be matched in one batch
    * @param parallelismLevel max number of threads required to perform matching
    *                         (default [[Runtime.getRuntime.availableProcessors()]])
    * @return a list of matching block hashes
    */
  def getMatchingBlocks(
      scripts: Vector[ScriptPubKey],
      startOpt: Option[BlockStamp] = None,
      endOpt: Option[BlockStamp] = None,
      batchSize: Int = 100,
      parallelismLevel: Int = Runtime.getRuntime.availableProcessors())(implicit
      ec: ExecutionContext): Future[Vector[BlockMatchingResponse]]

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
      useCreationTime: Boolean)(implicit ec: ExecutionContext): Future[Unit]

  /** Helper method to rescan the ENTIRE blockchain. */
  def fullRescanNeutrinoWallet(addressBatchSize: Int)(implicit
      ec: ExecutionContext): Future[Unit] =
    rescanNeutrinoWallet(startOpt = None,
                         endOpt = None,
                         addressBatchSize = addressBatchSize,
                         useCreationTime = false)

  def discoveryBatchSize(): Int = 25

}

object NeutrinoWalletApi {

  case class BlockMatchingResponse(
      blockHash: DoubleSha256DigestBE,
      blockHeight: Int)

}
