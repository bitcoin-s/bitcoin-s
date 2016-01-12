package org.scalacoin.protocol.blockchain.softforks



/**
 * Created by Tom on 1/11/2016.
 */
trait SoftForks {
  def id : String
  def version : Int
  def enforce : EnforcementProgress
  def reject : RejectionProgress
}

case class SoftForksImpl(id : String, version : Int, enforce : EnforcementProgress,
                         reject : RejectionProgress) extends SoftForks