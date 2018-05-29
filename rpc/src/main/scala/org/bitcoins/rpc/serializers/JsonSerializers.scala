package org.bitcoins.rpc.serializers

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{ Int32, UInt32 }
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonReaders._
import play.api.libs.json.{ Json, Reads }

object JsonSerializers {
  // Internal Types
  implicit val doubleSha256DigestReads: Reads[DoubleSha256Digest] = DoubleSha256DigestReads
  implicit val bitcoinsReads: Reads[Bitcoins] = BitcoinsReads
  implicit val blockHeaderReads: Reads[BlockHeader] = BlockHeaderReads
  implicit val int32Reads: Reads[Int32] = Int32Reads
  implicit val uInt32Reads: Reads[UInt32] = UInt32Reads

  // Network Models
  implicit val networkReads: Reads[Network] = Json.reads[Network]
  implicit val networkAddressReads: Reads[NetworkAddress] = Json.reads[NetworkAddress]
  implicit val networkInfoReads: Reads[GetNetworkInfoResult] = Json.reads[GetNetworkInfoResult]

  implicit val chainTipReads: Reads[ChainTip] = Json.reads[ChainTip]

  implicit val blockHeaderFormattedReads: Reads[GetBlockHeaderResult] = Json.reads[GetBlockHeaderResult]

  // Mining Models
  implicit val minginInfoReads: Reads[GetMiningInfoResult] = Json.reads[GetMiningInfoResult]

  // Wallet Models
}