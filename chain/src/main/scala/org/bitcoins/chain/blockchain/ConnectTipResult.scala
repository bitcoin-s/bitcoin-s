package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.chain.validation.TipUpdateResult
import org.bitcoins.core.protocol.blockchain.BlockHeader

sealed trait ConnectTipResult {
  def tipUpdateResult: TipUpdateResult

  def header: BlockHeader = tipUpdateResult.header

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
    def headerDb: BlockHeaderDb = tipUpdateResult.headerDb
  }

  case class Reorg(
      tipUpdateResult: TipUpdateResult.Success,
      newChain: Blockchain)
      extends ConnectTipResult {
    require(
      headerDb == newChain.tip,
      s"Cannot reorg without having tipUpdate be our best tip, tipUpdateResult=${tipUpdateResult.header} chain.tip=${newChain.tip}")

    def headerDb: BlockHeaderDb = tipUpdateResult.headerDb
  }

  case class BadTip(tipUpdateResult: TipUpdateResult.Failure)
      extends ConnectTipResult

}
