package org.bitcoins.chain.blockchain.sync

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.GolombFilter

/** Represents a [[GolombFilter]] with it's [[org.bitcoins.core.gcs.FilterHeader]] associated with it
  * This is needed because bitcoin core's 'getblockfilter' rpc returns things in this structure
  * */
case class FilterWithHeaderHash(
    filter: GolombFilter,
    headerHash: DoubleSha256DigestBE)
