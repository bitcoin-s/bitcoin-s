package org.bitcoins.rpc.serializers

import java.net.InetAddress

import org.bitcoins.core.crypto.{ DoubleSha256Digest, ECPublicKey, Sha256Hash160Digest }
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{ Int32, UInt32 }
import org.bitcoins.core.protocol.{ Address, BitcoinAddress, P2PKHAddress, P2SHAddress }
import org.bitcoins.core.protocol.blockchain.{ Block, BlockHeader, MerkleBlock }
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{ Transaction, TransactionInput, TransactionOutPoint, TransactionOutput }
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.serializers.JsonWriters._
import play.api.libs.json.{ Json, Reads, Writes, __ }
import play.api.libs.functional.syntax._

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
  implicit val blockReads: Reads[Block] = BlockReads
  implicit val sha256Hash160DigestReads: Reads[Sha256Hash160Digest] = Sha256Hash160DigestReads
  implicit val eCPublicKeyReads: Reads[ECPublicKey] = ECPublicKeyReads
  implicit val p2PKHAddressReads: Reads[P2PKHAddress] = P2PKHAddressReads
  implicit val p2SHAddressReads: Reads[P2SHAddress] = P2SHAddressReads
  implicit val transactionInputReads: Reads[TransactionInput] = TransactionInputReads
  implicit val bitcoinAddressReads: Reads[BitcoinAddress] = BitcoinAddressReads
  implicit val merkleBlockReads: Reads[MerkleBlock] = MerkleBlockReads
  implicit val transactionReads: Reads[Transaction] = TransactionReads
  implicit val transactionOutPointReads: Reads[TransactionOutPoint] = TransactionOutPointReads

  implicit val bitcoinsWrites: Writes[Bitcoins] = BitcoinsWrites
  implicit val bitcoinAddressWrites: Writes[BitcoinAddress] = BitcoinAddressWrites
  implicit val doubleSha256DigestWrites: Writes[DoubleSha256Digest] = DoubleSha256DigestWrites
  implicit val scriptPubKeyWrites: Writes[ScriptPubKey] = ScriptPubKeyWrites

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

  implicit val getBlockResultReads: Reads[GetBlockResult] = Json.reads[GetBlockResult]

  implicit val rpcScriptPubKeyReads: Reads[RpcScriptPubKey] = Json.reads[RpcScriptPubKey]
  implicit val rpcTransactionOutputReads: Reads[RpcTransactionOutput] = Json.reads[RpcTransactionOutput]
  implicit val rpcTransactionReads: Reads[RpcTransaction] = Json.reads[RpcTransaction]
  implicit val getBlockWithTransactionsResultReads: Reads[GetBlockWithTransactionsResult] = Json.reads[GetBlockWithTransactionsResult]

  implicit val paymentReads: Reads[Payment] = Json.reads[Payment]
  implicit val listSinceBlockResultReads: Reads[ListSinceBlockResult] = Json.reads[ListSinceBlockResult]

  implicit val listTransactionsResultReads: Reads[ListTransactionsResult] = Json.reads[ListTransactionsResult]

  implicit val receivedAddressReads: Reads[ReceivedAddress] = Json.reads[ReceivedAddress]

  implicit val TransactionDetailsReads: Reads[TransactionDetails] = Json.reads[TransactionDetails]
  implicit val getTransactionResultReads: Reads[GetTransactionResult] = Json.reads[GetTransactionResult]

  implicit val unspentOutputReads: Reads[UnspentOutput] = Json.reads[UnspentOutput]

  implicit val lockUnspentParameterWrites: Writes[LockUnspentOutputParameter] = Json.writes[LockUnspentOutputParameter]

  implicit val signRawTransactionResultReads: Reads[SignRawTransactionResult] = Json.reads[SignRawTransactionResult]

  // Mining Models
  implicit val miningInfoReads: Reads[GetMiningInfoResult] = Json.reads[GetMiningInfoResult]

  // Wallet Models
  implicit val getWalletInfoResultReads: Reads[GetWalletInfoResult] = Json.reads[GetWalletInfoResult]

  implicit val bumpFeeReads: Reads[BumpFeeResult] = Json.reads[BumpFeeResult]

  implicit val createMultiSigReads: Reads[CreateMultiSigResult] = Json.reads[CreateMultiSigResult]

  implicit val decodeScriptResultReads: Reads[DecodeScriptResult] = (
    (__ \ "asm").read[String] and
    (__ \ "type").readNullable[String] and
    (__ \ "reqSigs").readNullable[Int] and
    (__ \ "addresses").readNullable[Vector[P2PKHAddress]] and
    (__ \ "p2sh").read[P2SHAddress])(DecodeScriptResult)

  implicit val fundRawTransactionResultReads: Reads[FundRawTransactionResult] = Json.reads[FundRawTransactionResult]

  implicit val rpcAccoutReads: Reads[RpcAccount] = Json.reads[RpcAccount]
}