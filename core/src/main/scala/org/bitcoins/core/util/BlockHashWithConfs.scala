package org.bitcoins.core.util

import org.bitcoins.crypto.DoubleSha256DigestBE

/** Block hash with the number of confirmations associated with it.
  * If confirmationsOpt is None, that means the block hash has zero confirmations
  */
case class BlockHashWithConfs(
    blockHash: DoubleSha256DigestBE,
    confirmationsOpt: Option[Int])
