package org.bitcoins.protocol.blockchain.softforks

/**
 * Created by Tom on 1/11/2016.
 */
trait EnforcementProgress {
  def status : Boolean
  def newVersionBlocksFound : Int
  def requiredBlocks : Int
  def recentBlocksWindow : Int
}

case class EnforcementProgressImpl(status : Boolean , newVersionBlocksFound : Int, requiredBlocks : Int,
                                    recentBlocksWindow : Int) extends EnforcementProgress