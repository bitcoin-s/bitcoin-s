package org.bitcoins.commons.jsonmodels

import org.bitcoins.core.config.{BitcoinNetwork, BitcoinNetworks}
import org.bitcoins.crypto.DoubleSha256DigestBE
import ujson._

/** Basic information about the chain state of the Bitcoin-S server */
case class BitcoinSServerInfo(
    network: BitcoinNetwork,
    blockHeight: Int,
    blockHash: DoubleSha256DigestBE) {

  lazy val toJson: Value = {
    Obj(
      "network" -> Str(network.name),
      "blockHeight" -> Num(blockHeight),
      "blockHash" -> Str(blockHash.hex)
    )
  }
}

object BitcoinSServerInfo {

  def fromJson(json: Value): BitcoinSServerInfo = {
    val obj = json.obj

    val network = BitcoinNetworks.fromString(obj("network").str)
    val height = obj("blockHeight").num.toInt
    val blockHash = DoubleSha256DigestBE(obj("blockHash").str)

    BitcoinSServerInfo(network, height, blockHash)
  }
}
