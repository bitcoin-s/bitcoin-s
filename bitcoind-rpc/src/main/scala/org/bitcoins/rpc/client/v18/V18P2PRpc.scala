package org.bitcoins.rpc.client.v18

import org.bitcoins.rpc.client.common._
import scala.concurrent.Future
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._
import org.bitcoins.rpc.serializers.JsonReaders._

trait V18P2PRpc { self: Client =>

  def getPeerInfo: Future[Vector[PeerV18]] = {
    bitcoindCall[Vector[PeerV18]]("getpeerinfo")
  }

}
