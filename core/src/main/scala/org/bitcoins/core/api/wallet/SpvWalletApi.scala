package org.bitcoins.core.api.wallet

import org.bitcoins.core.bloom.BloomFilter

import scala.concurrent.Future

/** API for the wallet project.
  *
  * This wallet API is BIP44 compliant.
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
trait SpvWalletApi { self: WalletApi =>

  /** Recreates the account using BIP-44 approach
    */
  def rescanSPVWallet(): Future[Unit]

  /** Retrieves a bloom filter that that can be sent to a P2P network node
    * to get information about our transactions, pubkeys and scripts.
    */
  def getBloomFilter(): Future[BloomFilter]
}
