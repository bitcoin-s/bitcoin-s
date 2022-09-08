package org.bitcoins.commons.jsonmodels

import org.bitcoins.core.config.{BitcoinNetwork, BitcoinNetworks}
import org.bitcoins.core.serializers.PicklerKeys
import org.bitcoins.crypto.DoubleSha256DigestBE
import ujson._

/** Basic information about the chain state of the Bitcoin-S server */
case class BitcoinSServerInfo(
    network: BitcoinNetwork,
    blockHeight: Int,
    blockHash: DoubleSha256DigestBE,
    torStarted: Boolean,
    syncing: Boolean,
    isInitialBlockDownload: Boolean) {

  lazy val toJson: Value = {
    Obj(
      PicklerKeys.networkKey -> Str(network.name),
      PicklerKeys.blockHeightKey -> Num(blockHeight),
      PicklerKeys.blockHashKey -> Str(blockHash.hex),
      PicklerKeys.torStartedKey -> Bool(torStarted),
      PicklerKeys.syncKey -> Bool(syncing),
      PicklerKeys.isInitialBlockDownload -> Bool(isInitialBlockDownload)
    )
  }
}

object BitcoinSServerInfo {

  def fromJson(json: Value): BitcoinSServerInfo = {
    val obj = json.obj

    val network = BitcoinNetworks.fromString(obj(PicklerKeys.networkKey).str)
    val height = obj(PicklerKeys.blockHeightKey).num.toInt
    val blockHash = DoubleSha256DigestBE(obj(PicklerKeys.blockHashKey).str)
    val torStarted = obj(PicklerKeys.torStartedKey).bool
    val sync = obj(PicklerKeys.syncKey).bool
    val isIBD = obj(PicklerKeys.isInitialBlockDownload).bool

    BitcoinSServerInfo(network = network,
                       blockHeight = height,
                       blockHash = blockHash,
                       torStarted = torStarted,
                       syncing = sync,
                       isInitialBlockDownload = isIBD)
  }
}
