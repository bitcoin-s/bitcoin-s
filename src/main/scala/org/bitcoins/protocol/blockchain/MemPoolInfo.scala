package org.bitcoins.protocol.blockchain

/**
 * Created by Tom on 1/11/2016.
 */
trait MemPoolInfo {
  def size : Int
  def bytes : Int
}

case class MemPoolInfoImpl(size : Int, bytes : Int) extends MemPoolInfo