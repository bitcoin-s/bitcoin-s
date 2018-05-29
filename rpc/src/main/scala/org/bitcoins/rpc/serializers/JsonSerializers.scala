package org.bitcoins.rpc.serializers

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonReaders._
import play.api.libs.json.{Json, Reads}

object JsonSerializers {
  // Internal Types
  implicit val doubleSha256DigestReads: Reads[DoubleSha256Digest] = DoubleSha256DigestReads
  implicit val bitcoinsReads: Reads[Bitcoins] = BitcoinsReads

  // Network Models
  implicit val networkReads: Reads[Network] = Json.reads[Network]
  implicit val networkAddressReads: Reads[NetworkAddress] = Json.reads[NetworkAddress]
  implicit val networkInfoReads: Reads[GetNetworkInfoResult] = Json.reads[GetNetworkInfoResult]

  implicit val chainTipReads: Reads[ChainTip] = Json.reads[ChainTip]

  // Mining Models
  implicit val minginInfoReads: Reads[GetMiningInfoResult] = Json.reads[GetMiningInfoResult]

  // Wallet Models
}