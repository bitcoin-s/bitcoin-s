package org.bitcoins.core.api.chain
import org.bitcoins.core.protocol.blockchain.{BitcoinChainParams, BlockHeader}

trait TipValidationApi {

  /** Attempts to connect the given block header with the given blockchain
    * @param header
    *   the block header to connect to our chain
    * @param blockchain
    *   the blockchain we are attempting to connect to
    */
  def connectTip(
      header: BlockHeader,
      blockchain: Blockchain,
      chainParams: BitcoinChainParams): ConnectTipResult
}
