package org.bitcoins.rpc.serializers

import java.io.File
import java.net.{ InetAddress, URI }

import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  ECPublicKey,
  Sha256Hash160Digest
}
import org.bitcoins.core.currency.{ Bitcoins, Satoshis }
import org.bitcoins.core.number.{ Int32, UInt32, UInt64 }
import org.bitcoins.core.protocol.{
  Address,
  BitcoinAddress,
  P2PKHAddress,
  P2SHAddress
}
import org.bitcoins.core.protocol.blockchain.{ Block, BlockHeader, MerkleBlock }
import org.bitcoins.core.protocol.script.{ ScriptPubKey, ScriptSignature }
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.serializers.JsonWriters._
import play.api.libs.json._
import play.api.libs.functional.syntax._

object JsonSerializers {
  implicit val bigIntReads: Reads[BigInt] = BigIntReads

  // Internal Types
  implicit val doubleSha256DigestReads: Reads[DoubleSha256Digest] =
    DoubleSha256DigestReads
  implicit val bitcoinsReads: Reads[Bitcoins] = BitcoinsReads
  implicit val satoshisReads: Reads[Satoshis] = SatoshisReads
  implicit val blockHeaderReads: Reads[BlockHeader] = BlockHeaderReads
  implicit val int32Reads: Reads[Int32] = Int32Reads
  implicit val uInt32Reads: Reads[UInt32] = UInt32Reads
  implicit val uInt64Reads: Reads[UInt64] = UInt64Reads
  implicit val addressReads: Reads[Address] = AddressReads
  implicit val unitReads: Reads[Unit] = UnitReads
  implicit val inetAddressReads: Reads[InetAddress] = InetAddressReads
  implicit val scriptPubKeyReads: Reads[ScriptPubKey] = ScriptPubKeyReads
  implicit val blockReads: Reads[Block] = BlockReads
  implicit val sha256Hash160DigestReads: Reads[Sha256Hash160Digest] =
    Sha256Hash160DigestReads
  implicit val eCPublicKeyReads: Reads[ECPublicKey] = ECPublicKeyReads
  implicit val p2PKHAddressReads: Reads[P2PKHAddress] = P2PKHAddressReads
  implicit val p2SHAddressReads: Reads[P2SHAddress] = P2SHAddressReads
  implicit val transactionInputReads: Reads[TransactionInput] =
    TransactionInputReads
  implicit val bitcoinAddressReads: Reads[BitcoinAddress] = BitcoinAddressReads
  implicit val merkleBlockReads: Reads[MerkleBlock] = MerkleBlockReads
  implicit val transactionReads: Reads[Transaction] = TransactionReads
  implicit val transactionOutPointReads: Reads[TransactionOutPoint] =
    TransactionOutPointReads
  implicit val bitcoinFeeUnitReads: Reads[BitcoinFeeUnit] = BitcoinFeeUnitReads
  implicit val fileReads: Reads[File] = FileReads
  implicit val uRIReads: Reads[URI] = URIReads
  implicit val scriptSignatureReads: Reads[ScriptSignature] =
    ScriptSignatureReads

  implicit val bitcoinsWrites: Writes[Bitcoins] = BitcoinsWrites
  implicit val bitcoinAddressWrites: Writes[BitcoinAddress] =
    BitcoinAddressWrites
  implicit val doubleSha256DigestWrites: Writes[DoubleSha256Digest] =
    DoubleSha256DigestWrites
  implicit val scriptPubKeyWrites: Writes[ScriptPubKey] = ScriptPubKeyWrites
  implicit val transactionInputWrites: Writes[TransactionInput] =
    TransactionInputWrites
  implicit val uInt32Writes: Writes[UInt32] = UInt32Writes
  implicit val transactionWrites: Writes[Transaction] = TransactionWrites

  // Transaction Models
  implicit val rpcScriptPubKeyReads: Reads[RpcScriptPubKey] =
    ((__ \ "asm").read[String] and
      (__ \ "hex").read[String] and
      (__ \ "reqSigs").readNullable[Int] and
      (__ \ "type").read[String] and
      (__ \ "addresses").readNullable[Vector[BitcoinAddress]])(RpcScriptPubKey)
  implicit val rpcTransactionOutputReads: Reads[RpcTransactionOutput] =
    Json.reads[RpcTransactionOutput]
  implicit val rpcTransactionReads: Reads[RpcTransaction] =
    Json.reads[RpcTransaction]

  implicit val decodeScriptResultReads: Reads[DecodeScriptResult] =
    ((__ \ "asm").read[String] and
      (__ \ "type").readNullable[String] and
      (__ \ "reqSigs").readNullable[Int] and
      (__ \ "addresses").readNullable[Vector[P2PKHAddress]] and
      (__ \ "p2sh").read[P2SHAddress])(DecodeScriptResult)

  implicit val fundRawTransactionResultReads: Reads[FundRawTransactionResult] =
    Json.reads[FundRawTransactionResult]

  implicit val getRawTransactionScriptSigReads: Reads[GetRawTransactionScriptSig] = Json.reads[GetRawTransactionScriptSig]
  implicit val getRawTransactionVinReads: Reads[GetRawTransactionVin] =
    Json.reads[GetRawTransactionVin]
  implicit val getRawTransactionResultReads: Reads[GetRawTransactionResult] =
    Json.reads[GetRawTransactionResult]

  implicit val signRawTransactionErrorReads: Reads[SignRawTransactionError] =
    Json.reads[SignRawTransactionError]
  implicit val signRawTransactionResultReads: Reads[SignRawTransactionResult] =
    Json.reads[SignRawTransactionResult]

  // Network Models
  implicit val nodeAddressReads: Reads[NodeAddress] = Json.reads[NodeAddress]
  implicit val nodeReads: Reads[Node] = Json.reads[Node]

  implicit val netTargetReads: Reads[NetTarget] = Json.reads[NetTarget]
  implicit val getNetTotalsResultReads: Reads[GetNetTotalsResult] =
    Json.reads[GetNetTotalsResult]

  implicit val networkReads: Reads[Network] = Json.reads[Network]
  implicit val networkAddressReads: Reads[NetworkAddress] =
    Json.reads[NetworkAddress]
  implicit val networkInfoReads: Reads[GetNetworkInfoResult] =
    Json.reads[GetNetworkInfoResult]

  implicit val peerNetworkInfoReads: Reads[PeerNetworkInfo] =
    Json.reads[PeerNetworkInfo]
  implicit val peerReads: Reads[Peer] = ((__ \ "id").read[Int] and
    __.read[PeerNetworkInfo] and
    (__ \ "version").read[Int] and
    (__ \ "subver").read[String] and
    (__ \ "inbound").read[Boolean] and
    (__ \ "addnode").read[Boolean] and
    (__ \ "startingheight").read[Int] and
    (__ \ "banscore").read[Int] and
    (__ \ "synced_headers").read[Int] and
    (__ \ "synced_blocks").read[Int] and
    (__ \ "inflight").read[Vector[Int]] and
    (__ \ "whitelisted").read[Boolean] and
    (__ \ "bytessent_per_msg").read[Map[String, Int]] and
    (__ \ "bytesrecv_per_msg").read[Map[String, Int]])(Peer)

  implicit val nodeBanReads: Reads[NodeBan] = Json.reads[NodeBan]

  // Blockchain Models
  implicit val getBlockResultReads: Reads[GetBlockResult] =
    Json.reads[GetBlockResult]

  implicit val getBlockWithTransactionsResultReads: Reads[GetBlockWithTransactionsResult] = Json.reads[GetBlockWithTransactionsResult]

  implicit val softforkProgressReads: Reads[SoftforkProgress] =
    Json.reads[SoftforkProgress]
  implicit val softforkReads: Reads[Softfork] = Json.reads[Softfork]
  implicit val bip9SoftforkReads: Reads[Bip9Softfork] = Json.reads[Bip9Softfork]
  implicit val getBlockChainInfoResultReads: Reads[GetBlockChainInfoResult] =
    Json.reads[GetBlockChainInfoResult]

  implicit val blockHeaderFormattedReads: Reads[GetBlockHeaderResult] =
    Json.reads[GetBlockHeaderResult]

  implicit val chainTipReads: Reads[ChainTip] = Json.reads[ChainTip]

  implicit val getChainTxStatsResultReads: Reads[GetChainTxStatsResult] =
    Json.reads[GetChainTxStatsResult]

  implicit val getMemPoolResultReads: Reads[GetMemPoolResult] =
    Json.reads[GetMemPoolResult]

  implicit val getMemPoolEntryResultReads: Reads[GetMemPoolEntryResult] =
    Json.reads[GetMemPoolEntryResult]

  implicit val getMemPoolInfoResultReads: Reads[GetMemPoolInfoResult] =
    Json.reads[GetMemPoolInfoResult]

  implicit val getTxOutResultReads: Reads[GetTxOutResult] =
    Json.reads[GetTxOutResult]

  implicit val getTxOutSetInfoResultReads: Reads[GetTxOutSetInfoResult] =
    Json.reads[GetTxOutSetInfoResult]

  // Wallet Models
  implicit val multiSigReads: Reads[MultiSigResult] =
    Json.reads[MultiSigResult]

  implicit val bumpFeeReads: Reads[BumpFeeResult] = Json.reads[BumpFeeResult]

  implicit val TransactionDetailsReads: Reads[TransactionDetails] =
    Json.reads[TransactionDetails]
  implicit val getTransactionResultReads: Reads[GetTransactionResult] =
    ((__ \ "amount").read[Bitcoins] and
      (__ \ "fee").readNullable[Bitcoins] and
      (__ \ "confirmations").read[Int] and
      (__ \ "generated").readNullable[Boolean] and
      (__ \ "blockhash").readNullable[DoubleSha256Digest] and
      (__ \ "blockindex").readNullable[Int] and
      (__ \ "blocktime").readNullable[UInt32] and
      (__ \ "txid").read[DoubleSha256Digest] and
      (__ \ "walletconflicts").read[Vector[DoubleSha256Digest]] and
      (__ \ "time").read[UInt32] and
      (__ \ "timereceived").read[UInt32] and
      (__ \ "bip125-replaceable").read[String] and
      (__ \ "comment").readNullable[String] and
      (__ \ "to").readNullable[String] and
      (__ \ "details").read[Vector[TransactionDetails]] and
      (__ \ "hex").read[Transaction])(GetTransactionResult)

  implicit val getWalletInfoResultReads: Reads[GetWalletInfoResult] =
    Json.reads[GetWalletInfoResult]

  implicit val importMultiErrorReads: Reads[ImportMultiError] =
    Json.reads[ImportMultiError]
  implicit val importMultiResultReads: Reads[ImportMultiResult] =
    Json.reads[ImportMultiResult]

  implicit val rpcAddressReads: Reads[RpcAddress] = RpcAddressReads

  implicit val rpcAccoutReads: Reads[RpcAccount] = Json.reads[RpcAccount]

  implicit val dumpWalletResultReads: Reads[DumpWalletResult] =
    Json.reads[DumpWalletResult]

  implicit val rescanBlockChainResultReads: Reads[RescanBlockChainResult] =
    Json.reads[RescanBlockChainResult]

  implicit val receivedAddressReads: Reads[ReceivedAddress] =
    Json.reads[ReceivedAddress]

  implicit val receivedAccountReads: Reads[ReceivedAccount] =
    Json.reads[ReceivedAccount]

  implicit val paymentReads: Reads[Payment] =
    ((__ \ "involvesWatchonly").readNullable[Boolean] and
      (__ \ "account").readNullable[String] and
      (__ \ "address").readNullable[BitcoinAddress] and
      (__ \ "category").read[String] and
      (__ \ "amount").read[Bitcoins] and
      (__ \ "vout").read[Int] and
      (__ \ "fee").readNullable[Bitcoins] and
      (__ \ "confirmations").read[Int] and
      (__ \ "generated").readNullable[Boolean] and
      (__ \ "blockhash").readNullable[DoubleSha256Digest] and
      (__ \ "blockindex").readNullable[Int] and
      (__ \ "blocktime").readNullable[UInt32] and
      (__ \ "txid").read[DoubleSha256Digest] and
      (__ \ "walletconflicts").read[Vector[DoubleSha256Digest]] and
      (__ \ "time").read[UInt32] and
      (__ \ "timereceived").read[UInt32] and
      (__ \ "bip125-replaceable").read[String] and
      (__ \ "comment").readNullable[String] and
      (__ \ "to").readNullable[String])(Payment)
  implicit val listSinceBlockResultReads: Reads[ListSinceBlockResult] =
    Json.reads[ListSinceBlockResult]

  implicit val listTransactionsResultReads: Reads[ListTransactionsResult] =
    ((__ \ "account").readNullable[String] and
      (__ \ "address").readNullable[BitcoinAddress] and
      (__ \ "category").read[String] and
      (__ \ "amount").read[Bitcoins] and
      (__ \ "label").readNullable[String] and
      (__ \ "vout").readNullable[Int] and
      (__ \ "fee").readNullable[Bitcoins] and
      (__ \ "confirmations").readNullable[Int] and
      (__ \ "trusted").readNullable[Boolean] and
      (__ \ "generated").readNullable[Boolean] and
      (__ \ "blockhash").readNullable[DoubleSha256Digest] and
      (__ \ "blockindex").readNullable[Int] and
      (__ \ "blocktime").readNullable[UInt32] and
      (__ \ "txid").readNullable[DoubleSha256Digest] and
      (__ \ "walletconflicts").readNullable[Vector[DoubleSha256Digest]] and
      (__ \ "time").read[UInt32] and
      (__ \ "timereceived").readNullable[UInt32] and
      (__ \ "comment").readNullable[String] and
      (__ \ "to").readNullable[String] and
      (__ \ "otheraccount").readNullable[String] and
      (__ \ "bip125-replaceable").read[String] and
      (__ \ "abandoned").readNullable[Boolean])(ListTransactionsResult)

  implicit val unspentOutputReads: Reads[UnspentOutput] =
    Json.reads[UnspentOutput]

  // Other Models
  implicit val blockTransactionReads: Reads[BlockTransaction] =
    Json.reads[BlockTransaction]
  implicit val getBlockTemplateResultReads: Reads[GetBlockTemplateResult] =
    Json.reads[GetBlockTemplateResult]

  implicit val miningInfoReads: Reads[GetMiningInfoResult] =
    Json.reads[GetMiningInfoResult]

  implicit val memoryManagerReads: Reads[MemoryManager] =
    Json.reads[MemoryManager]
  implicit val getMemoryInfoResultReads: Reads[GetMemoryInfoResult] =
    Json.reads[GetMemoryInfoResult]

  implicit val validateAddressResultReads: Reads[ValidateAddressResult] =
    Json.reads[ValidateAddressResult]

  implicit val estimateSmartFeeResultReads: Reads[EstimateSmartFeeResult] =
    Json.reads[EstimateSmartFeeResult]

  // Map stuff
  implicit def mapDoubleSha256DigestReads: Reads[Map[DoubleSha256Digest, GetMemPoolResult]] =
    Reads.mapReads[DoubleSha256Digest, GetMemPoolResult](s =>
      JsSuccess(DoubleSha256Digest.fromHex(s)))

  implicit val outputMapWrites: Writes[Map[BitcoinAddress, Bitcoins]] =
    mapWrites[BitcoinAddress, Bitcoins](_.value)
}
