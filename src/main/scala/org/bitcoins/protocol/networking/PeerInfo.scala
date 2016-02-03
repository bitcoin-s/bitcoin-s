package org.bitcoins.protocol.networking

/**
 * Created by Tom on 1/5/2016.
 */
trait PeerInfo {
  def id : Int
  def addr : String
  def addrLocal : String
  def services : String
  def lastSend : Long
  def lastRecv : Long
  def bytesSent : Long
  def bytesRecv : Long
  def connTime : Long
  def timeOffSet : Int
  def pingTime : Double
  def version : Long
  def subVer : String
  def inbound : Boolean
  def startingHeight : Int
  def banScore : Int
  def syncedHeaders : Int
  def syncedBlocks : Int
  def inFlight : Seq[Int]
  def whiteListed : Boolean
}

case class PeerInfoImpl(id : Int, addr : String, addrLocal : String, services : String, lastSend : Long,
  lastRecv : Long, bytesSent : Long, bytesRecv : Long, connTime : Long, timeOffSet : Int, pingTime : Double,
   version : Long, subVer : String, inbound : Boolean, startingHeight : Int, banScore : Int,
  syncedHeaders : Int, syncedBlocks : Int, inFlight : Seq[Int], whiteListed : Boolean) extends PeerInfo