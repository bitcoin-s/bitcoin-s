package org.scalacoin.protocol.networking

/**
 * Created by Tom on 1/5/2016.
 */
trait PeerInfo {
  def id : Int
  def addr : String
  def addrlocal : String
  def services : String
  def lastsend : Long
  def lastrecv : Long
  def bytessent : Long
  def bytesrecv : Long
  def conntime : Long
  def timeoffset : Int
  def pingtime : Double
  def pingwait : Double
  def version : Long
  def subver : String
  def inbound : Boolean
  def startingheight : Int
  def banscore : Int
  def synced_headers : Int
  def synced_blocks : Int
  def inflight : Array
  def whitelisted : Boolean
}

case class PeerInfoImpl(id : Int, addr : String, addrlocal : String, services : String, lastsend : Long,
  lastrecv : Long, bytessent : Long, bytesrecv : Long, conntime : Long, timeoffset : Int, pingtime : Double,
  pingwait : Double, version : Long, subver : String, inbound : Boolean, startingheight : Int, banscore : Int,
  synced_headers : Int, synced_blocks : Int, inflight : Array, whitelisted : Boolean) extends PeerInfo