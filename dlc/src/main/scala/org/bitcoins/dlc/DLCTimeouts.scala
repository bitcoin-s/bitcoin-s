package org.bitcoins.dlc

import org.bitcoins.core.protocol.BlockStampWithFuture

/** @param penaltyTimeout The CSV timeout in blocks used in all CETs
  * @param contractMaturity The CLTV in milliseconds when a signature is expected
  * @param contractTimeout The CLTV timeout in milliseconds after which the refund tx is valid
  */
case class DLCTimeouts(
    penaltyTimeout: Int,
    contractMaturity: BlockStampWithFuture,
    contractTimeout: BlockStampWithFuture
)
