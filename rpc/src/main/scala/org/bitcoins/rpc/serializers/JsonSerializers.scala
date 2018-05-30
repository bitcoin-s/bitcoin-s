package org.bitcoins.rpc.serializers

import java.net.InetAddress

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonReaders._
import play.api.libs.json.{Json, Reads}

object JsonSerializers {
  // Internal Types
  implicit val doubleSha256DigestReads: Reads[DoubleSha256Digest] = DoubleSha256DigestReads
  implicit val bitcoinsReads: Reads[Bitcoins] = BitcoinsReads
  implicit val blockHeaderReads: Reads[BlockHeader] = BlockHeaderReads
  implicit val int32Reads: Reads[Int32] = Int32Reads
  implicit val uInt32Reads: Reads[UInt32] = UInt32Reads
  implicit val addressReads: Reads[Address] = AddressReads
  implicit val unitReads: Reads[Unit] = UnitReads
  implicit val inetAddressReads: Reads[InetAddress] = InetAddressReads
  implicit val scriptPubKeyReads: Reads[ScriptPubKey] = ScriptPubKeyReads

  // Network Models
  implicit val networkReads: Reads[Network] = Json.reads[Network]
  implicit val networkAddressReads: Reads[NetworkAddress] = Json.reads[NetworkAddress]
  implicit val networkInfoReads: Reads[GetNetworkInfoResult] = Json.reads[GetNetworkInfoResult]

  implicit val chainTipReads: Reads[ChainTip] = Json.reads[ChainTip]

  implicit val blockHeaderFormattedReads: Reads[GetBlockHeaderResult] = Json.reads[GetBlockHeaderResult]

  implicit val validateAddressResultReads: Reads[ValidateAddressResult] = Json.reads[ValidateAddressResult]

  implicit val nodeBanReads: Reads[NodeBan] = Json.reads[NodeBan]

  implicit val nodeAddressReads: Reads[NodeAddress] = Json.reads[NodeAddress]
  implicit val nodeReads: Reads[Node] = Json.reads[Node]

  implicit val getMemPoolEntryResultReads: Reads[GetMemPoolEntryResult] = Json.reads[GetMemPoolEntryResult]

  implicit val getMemPoolInfoResultReads: Reads[GetMemPoolInfoResult] = Json.reads[GetMemPoolInfoResult]

  implicit val getTxOutSetInfoResultReads: Reads[GetTxOutSetInfoResult] = Json.reads[GetTxOutSetInfoResult]

  // Mining Models
  implicit val miningInfoReads: Reads[GetMiningInfoResult] = Json.reads[GetMiningInfoResult]

  // Wallet Models
  implicit val getWalletInfoResultReads: Reads[GetWalletInfoResult] = Json.reads[GetWalletInfoResult]

  implicit val bumpFeeReads: Reads[BumpFeeResult] = Json.reads[BumpFeeResult]

  implicit val createMultiSigReads: Reads[CreateMultiSigResult] = Json.reads[CreateMultiSigResult]
}