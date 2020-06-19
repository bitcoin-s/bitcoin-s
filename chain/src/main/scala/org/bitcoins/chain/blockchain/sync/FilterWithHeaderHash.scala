package org.bitcoins.chain.blockchain.sync

import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.crypto.DoubleSha256DigestBE

/** Represents a [[GolombFilter]] with it's [[org.bitcoins.core.gcs.FilterHeader]] associated with it
  * This is needed because bitcoin core's 'getblockfilter' rpc returns things in this structure
  */
case class FilterWithHeaderHash(
    filter: GolombFilter,
    headerHash: DoubleSha256DigestBE)
