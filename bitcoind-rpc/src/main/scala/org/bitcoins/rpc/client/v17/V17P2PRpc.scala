package org.bitcoins.rpc.client.v17

import scala.concurrent.Future

import org.bitcoins.rpc.client.common._
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._

trait V17P2PRpc { self: Client => 
  def getPeerInfo: Future[Vector[Peer]] = {
    bitcoindCall[Vector[Peer]]("getpeerinfo")
  }
}