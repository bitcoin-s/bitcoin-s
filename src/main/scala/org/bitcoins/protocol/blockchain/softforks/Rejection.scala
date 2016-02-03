package org.bitcoins.protocol.blockchain.softforks

/**
 * Created by Tom on 1/11/2016.
 */
trait RejectionProgress {
  def status : Boolean
  def newVersionBlocksFound : Int
  def requiredBlocks : Int
  def recentBlocksWindow : Int
}

case class RejectionProgressImpl(status : Boolean , newVersionBlocksFound : Int, requiredBlocks : Int,
                                   recentBlocksWindow : Int) extends RejectionProgress