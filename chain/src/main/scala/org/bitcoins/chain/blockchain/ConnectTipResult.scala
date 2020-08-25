package org.bitcoins.chain.blockchain

import org.bitcoins.chain.validation.TipUpdateResult
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.protocol.blockchain.BlockHeader

/** The result indicating how the [[org.bitcoins.chain.validation.TipUpdateResult TipUpdateResult]]
  * modified the chain.
  *
  * We can
  *
  * 1. Extend the chain
  * 2. Reorg the chain
  * 3. Fail to connect to anything in the chain
  */
sealed trait ConnectTipResult {
  def tipUpdateResult: TipUpdateResult

  lazy val header: BlockHeader = tipUpdateResult.header

}

object ConnectTipResult {

  /** Indicates we sucuessfully extended our chain by one block */
  case class ExtendChain(
      tipUpdateResult: TipUpdateResult.Success,
      newChain: Blockchain)
      extends ConnectTipResult {
    require(
      headerDb == newChain.tip,
      s"Cannot extend chain without having tipUpdate be our best tip, tipUpdateResult=${tipUpdateResult.header} chain.tip=${newChain.tip}"
    )
    lazy val headerDb: BlockHeaderDb = tipUpdateResult.headerDb
  }

  /**
    * Means we had a reorg happen, aka the header was connected to
    * something that was _not_ our previous best tip
    * @param tipUpdateResult the successful connection
    * @param newChain the new chain where the best tip is the header we passed in
    */
  case class Reorg(
      tipUpdateResult: TipUpdateResult.Success,
      newChain: Blockchain)
      extends ConnectTipResult {
    require(
      headerDb == newChain.tip,
      s"Cannot reorg without having tipUpdate be our best tip, tipUpdateResult=${tipUpdateResult.header} chain.tip=${newChain.tip}")

    lazy val headerDb: BlockHeaderDb = tipUpdateResult.headerDb
  }

  /** Means we could not connect the header to anything in the given blockchain */
  case class BadTip(tipUpdateResult: TipUpdateResult.Failure)
      extends ConnectTipResult

}
