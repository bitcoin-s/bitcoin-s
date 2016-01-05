package org.scalacoin.protocol.networking

/**
 * Created by Tom on 1/5/2016.
 */
trait NetworkInfo {
  def version : Int
  def subversion : String
  def protocolversion: Int
  def localservices : String
  def timeoffset : Int
  def connections : Int
  def networks : Array
  def relayfee : Double
  def localaddresses : Array
}

case class NetworkInfoImpl(version : Int, subversion : String, protocolversion : Int, localservices : String,
  timeoffset : Int, connections : Int, networks : Array, relayfee : Double, localaddresses: Array) extends NetworkInfo
