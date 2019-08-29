package org.bitcoins.rpc.client.v16

import org.bitcoins.rpc.client.common._
import scala.concurrent.Future
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._

trait V16P2PRpc { self : Client => 
  def getPeerInfo: Future[Vector[Peer]] = {
    bitcoindCall[Vector[Peer]]("getpeerinfo")
  }
}