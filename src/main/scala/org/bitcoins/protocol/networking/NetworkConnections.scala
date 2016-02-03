package org.bitcoins.protocol.networking

/**
 * Created by Tom on 1/6/2016.
 */
trait NetworkConnections {
  def name : String
  def limited : Boolean
  def reachable : Boolean
  def proxy : String
  def proxyRandomizeCredentials : Boolean
}

case class NetworkConnectionsImpl(name : String, limited : Boolean, reachable : Boolean, proxy : String,
      proxyRandomizeCredentials : Boolean) extends NetworkConnections
