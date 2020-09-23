package org.bitcoins.core.api.chain

import org.bitcoins.crypto.DoubleSha256Digest

/** This is a helper class for syncing block filters following the
  * BIP157 protocol. This indicates the starting block height we are
  * syncing filters at, and the last block hash we expect in the batch
  * of filters sent back to us by our peer
  * @see https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#cfheaders
  */
case class FilterSyncMarker(
    startHeight: Int,
    stopBlockHash: DoubleSha256Digest) {

  override def toString: String =
    s"FilterSyncMarker(startHeight = $stopBlockHash, stopBlockHash=${stopBlockHash.flip.hex})"
}
