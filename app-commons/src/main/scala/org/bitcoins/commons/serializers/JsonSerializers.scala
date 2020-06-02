package org.bitcoins.commons.serializers

import java.io.File
import java.net.{InetAddress, URI}

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.{Int32, UInt32, UInt64}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature, WitnessScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint}
import org.bitcoins.core.protocol.{Address, BitcoinAddress, P2PKHAddress, P2SHAddress}
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.wallet.fee.{
  BitcoinFeeUnit,
  SatoshisPerKiloByte,
  SatoshisPerVirtualByte
}
import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.commons.serializers.JsonWriters._
import java.time.LocalDateTime

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.commons.jsonmodels.bitcoind.{
  AddressInfoResult,
  AnalyzePsbtInput,
  AnalyzePsbtResult,
  ArrayOfWalletsInput,
  BalanceInfo,
  Bip9Softfork,
  BlockTransaction,
  BumpFeeResult,
  ChainTip,
  CreateWalletResult,
  DecodePsbtResult,
  DecodeScriptResult,
  DeriveAddressesResult,
  DumpWalletResult,
  EmbeddedResult,
  EstimateSmartFeeResult,
  FeeInfo,
  FinalizePsbtResult,
  FinalizedPsbt,
  FundRawTransactionResult,
  GetBalancesResult,
  GetBlockChainInfoResult,
  GetBlockHeaderResult,
  GetBlockResult,
  GetBlockTemplateResult,
  GetBlockWithTransactionsResult,
  GetChainTxStatsResult,
  GetDescriptorInfoResult,
  GetMemPoolEntryResultPostV19,
  GetMemPoolEntryResultPreV19,
  GetMemPoolInfoResult,
  GetMemPoolResultPostV19,
  GetMemPoolResultPreV19,
  GetMemoryInfoResult,
  GetMiningInfoResult,
  GetNetTotalsResult,
  GetNetworkInfoResult,
  GetNodeAddressesResult,
  GetRawTransactionResult,
  GetRawTransactionScriptSig,
  GetRawTransactionVin,
  GetRpcInfoResult,
  GetTransactionResult,
  GetTxOutResult,
  GetTxOutSetInfoResult,
  GetWalletInfoResult,
  ImportMultiError,
  ImportMultiResult,
  LabelResult,
  ListSinceBlockResult,
  ListTransactionsResult,
  ListWalletDirResult,
  MemoryManager,
  MultiSigResult,
  NetTarget,
  Network,
  NetworkAddress,
  Node,
  NodeAddress,
  NodeBan,
  NonFinalizedPsbt,
  Payment,
  Peer,
  PeerNetworkInfo,
  PsbtBIP32Deriv,
  PsbtMissingData,
  PsbtWitnessUtxoInput,
  ReceivedAccount,
  ReceivedAddress,
  ReceivedLabel,
  RescanBlockChainResult,
  RpcAccount,
  RpcAddress,
  RpcCommands,
  RpcPsbtInput,
  RpcPsbtOutput,
  RpcPsbtScript,
  RpcScriptPubKey,
  RpcTransaction,
  RpcTransactionOutput,
  SetWalletFlagResult,
  SignRawTransactionError,
  SignRawTransactionResult,
  Softfork,
  SoftforkProgress,
  SubmitHeaderResult,
  TestMempoolAcceptResult,
  TransactionDetails,
  UnspentOutput,
  ValidateAddressResultImpl,
  WalletCreateFundedPsbtResult,
  WalletProcessPsbtResult
}
import org.bitcoins.commons.jsonmodels.wallet.{
  BitcoinerLiveEstimate,
  BitcoinerLiveResult
}
import org.bitcoins.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECDigitalSignature,
  ECPublicKey,
  RipeMd160Digest,
  RipeMd160DigestBE,
  Sha256Hash160Digest
}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.duration.DurationLong

object JsonSerializers {

  implicit val bigIntReads: Reads[BigInt] = BigIntReads
  implicit val localDateTimeReads: Reads[LocalDateTime] = LocalDateTimeReads

  // Internal Types
  implicit val doubleSha256DigestReads: Reads[DoubleSha256Digest] =
    DoubleSha256DigestReads
  implicit val doubleSha256DigestBEReads: Reads[DoubleSha256DigestBE] =
    DoubleSha256DigestBEReads
  implicit val ripeMd160DigestReads: Reads[RipeMd160Digest] =
    RipeMd160DigestReads
  implicit val ripeMd160DigestBEReads: Reads[RipeMd160DigestBE] =
    RipeMd160DigestBEReads
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

  implicit val addressTypeWrites: Writes[AddressType] = AddressTypeWrites
  implicit val bitcoinsWrites: Writes[Bitcoins] = BitcoinsWrites
  implicit val bitcoinAddressWrites: Writes[BitcoinAddress] =
    BitcoinAddressWrites
  implicit val doubleSha256DigestWrites: Writes[DoubleSha256Digest] =
    DoubleSha256DigestWrites
  implicit val scriptPubKeyWrites: Writes[ScriptPubKey] = ScriptPubKeyWrites
  implicit val witnessScriptPubKeyWrites: Writes[WitnessScriptPubKey] = WitnessScriptPubKeyWrites
  implicit val transactionInputWrites: Writes[TransactionInput] =
    TransactionInputWrites
  implicit val uInt32Writes: Writes[UInt32] = UInt32Writes
  implicit val transactionWrites: Writes[Transaction] = TransactionWrites
  implicit val xpubFormat: Format[ExtPublicKey] = new Format[ExtPublicKey] {
    override def reads(json: JsValue): JsResult[ExtPublicKey] =
      SerializerUtil.processJsStringOpt(ExtPublicKey.fromString(_).toOption)(
        json)

    override def writes(key: ExtPublicKey): JsValue = JsString(key.toString)
  }

  implicit val xprivForamt: Format[ExtPrivateKey] = new Format[ExtPrivateKey] {
    override def reads(json: JsValue): JsResult[ExtPrivateKey] =
      SerializerUtil.processJsStringOpt(ExtPrivateKey.fromString(_).toOption)(
        json)
    override def writes(key: ExtPrivateKey): JsValue = JsString(key.toString)
  }

  // Transaction Models
  implicit val rpcScriptPubKeyReads: Reads[RpcScriptPubKey] =
    ((__ \ "asm").read[String] and
      (__ \ "hex").read[String] and
      (__ \ "reqSigs").readNullable[Int] and
      (__ \ "type").read[ScriptType] and
      (__ \ "addresses").readNullable[Vector[BitcoinAddress]])(RpcScriptPubKey)
  implicit val rpcTransactionOutputReads: Reads[RpcTransactionOutput] =
    Json.reads[RpcTransactionOutput]
  implicit val rpcTransactionReads: Reads[RpcTransaction] =
    Json.reads[RpcTransaction]

  implicit val decodeScriptResultReads: Reads[DecodeScriptResult] =
    ((__ \ "asm").read[String] and
      (__ \ "type").readNullable[ScriptType] and
      (__ \ "reqSigs").readNullable[Int] and
      (__ \ "addresses").readNullable[Vector[P2PKHAddress]] and
      (__ \ "p2sh").read[P2SHAddress])(DecodeScriptResult)

  implicit val fundRawTransactionResultReads: Reads[FundRawTransactionResult] =
    Json.reads[FundRawTransactionResult]

  implicit val getRawTransactionScriptSigReads: Reads[
    GetRawTransactionScriptSig] = Json.reads[GetRawTransactionScriptSig]
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

  implicit val satsPerKbReads: Reads[SatoshisPerKiloByte] =
    new Reads[SatoshisPerKiloByte] {

      def reads(json: JsValue): JsResult[SatoshisPerKiloByte] =
        SerializerUtil.processJsNumber(num =>
          SatoshisPerKiloByte(Satoshis(num.toBigInt)))(json)
    }

  implicit val satsPerVBReads: Reads[SatoshisPerVirtualByte] =
    new Reads[SatoshisPerVirtualByte] {

      def reads(json: JsValue): JsResult[SatoshisPerVirtualByte] =
        SerializerUtil.processJsNumber(num =>
          SatoshisPerVirtualByte(Satoshis(num.toBigInt)))(json)
    }

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
    (__ \ "bytesrecv_per_msg").read[Map[String, Int]] and
    (__ \ "minfeefilter").readNullable[SatoshisPerKiloByte])(Peer)

  implicit val nodeBanReads: Reads[NodeBan] = Json.reads[NodeBan]

  // Blockchain Models
  implicit val getBlockResultReads: Reads[GetBlockResult] =
    Json.reads[GetBlockResult]

  implicit val getBlockWithTransactionsResultReads: Reads[
    GetBlockWithTransactionsResult] =
    Json.reads[GetBlockWithTransactionsResult]

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

  implicit val feeInfoReads: Reads[FeeInfo] = Json.reads[FeeInfo]

  implicit val getMemPoolResultPreV19Reads: Reads[GetMemPoolResultPreV19] =
    Json.reads[GetMemPoolResultPreV19]

  implicit val getMemPoolResultPostV19Reads: Reads[GetMemPoolResultPostV19] =
    Json.reads[GetMemPoolResultPostV19]

  implicit val getMemPoolEntryResultPreV19Reads: Reads[
    GetMemPoolEntryResultPreV19] =
    Json.reads[GetMemPoolEntryResultPreV19]

  implicit val getMemPoolEntryResultPostV19Reads: Reads[
    GetMemPoolEntryResultPostV19] =
    Json.reads[GetMemPoolEntryResultPostV19]

  implicit val getMemPoolInfoResultReads: Reads[GetMemPoolInfoResult] =
    Json.reads[GetMemPoolInfoResult]

  implicit val getTxOutResultReads: Reads[GetTxOutResult] =
    Json.reads[GetTxOutResult]

  implicit val getTxOutSetInfoResultReads: Reads[GetTxOutSetInfoResult] =
    Json.reads[GetTxOutSetInfoResult]

  implicit object Bip32PathFormats extends Format[BIP32Path] {
    override def reads(json: JsValue): JsResult[BIP32Path] =
      json.validate[String].map(BIP32Path.fromString)
    override def writes(o: BIP32Path): JsValue =
      JsString(o.toString)
  }

  // Wallet Models
  implicit val multiSigReads: Reads[MultiSigResult] =
    Json.reads[MultiSigResult]

  implicit val bumpFeeReads: Reads[BumpFeeResult] = Json.reads[BumpFeeResult]

  implicit val setWalletFlagResultReads: Reads[SetWalletFlagResult] =
    Json.reads[SetWalletFlagResult]

  implicit val balanceInfoReads: Reads[BalanceInfo] = Json.reads[BalanceInfo]
  implicit val getBalancesResultReads: Reads[GetBalancesResult] =
    Json.reads[GetBalancesResult]

  implicit val TransactionDetailsReads: Reads[TransactionDetails] =
    Json.reads[TransactionDetails]
  implicit val getTransactionResultReads: Reads[GetTransactionResult] =
    ((__ \ "amount").read[Bitcoins] and
      (__ \ "fee").readNullable[Bitcoins] and
      (__ \ "confirmations").read[Int] and
      (__ \ "generated").readNullable[Boolean] and
      (__ \ "blockhash").readNullable[DoubleSha256DigestBE] and
      (__ \ "blockindex").readNullable[Int] and
      (__ \ "blocktime").readNullable[UInt32] and
      (__ \ "txid").read[DoubleSha256DigestBE] and
      (__ \ "walletconflicts").read[Vector[DoubleSha256DigestBE]] and
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

  implicit val labelResult: Reads[LabelResult] =
    Json.reads[LabelResult]

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
      (__ \ "blockhash").readNullable[DoubleSha256DigestBE] and
      (__ \ "blockindex").readNullable[Int] and
      (__ \ "blocktime").readNullable[UInt32] and
      (__ \ "txid").read[DoubleSha256DigestBE] and
      (__ \ "walletconflicts").read[Vector[DoubleSha256DigestBE]] and
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
      (__ \ "blockhash").readNullable[DoubleSha256DigestBE] and
      (__ \ "blockindex").readNullable[Int] and
      (__ \ "blocktime").readNullable[UInt32] and
      (__ \ "txid").readNullable[DoubleSha256DigestBE] and
      (__ \ "walletconflicts").readNullable[Vector[DoubleSha256DigestBE]] and
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

  implicit val validateAddressResultReads: Reads[ValidateAddressResultImpl] =
    Json.reads[ValidateAddressResultImpl]

  implicit val embeddedResultReads: Reads[EmbeddedResult] =
    Json.reads[EmbeddedResult]

  implicit val addressInfoResultReads: Reads[AddressInfoResult] =
    Json.reads[AddressInfoResult]

  implicit val receivedLabelReads: Reads[ReceivedLabel] =
    Json.reads[ReceivedLabel]

  implicit val estimateSmartFeeResultReads: Reads[EstimateSmartFeeResult] =
    Json.reads[EstimateSmartFeeResult]

  implicit val walletProcessPsbtResultReads: Reads[WalletProcessPsbtResult] =
    Json.reads[WalletProcessPsbtResult]

  implicit val finalizedPsbtReads: Reads[FinalizedPsbt] = FinalizedPsbtReads

  implicit val nonFinalizedPsbtReads: Reads[NonFinalizedPsbt] =
    NonFinalizedPsbtReads

  implicit val finalizePsbtResultReads: Reads[FinalizePsbtResult] =
    FinalizePsbtResultReads

  implicit val rpcPsbtOutputReads: Reads[RpcPsbtOutput] = RpcPsbtOutputReads

  implicit val psbtBIP32DerivsReads: Reads[PsbtBIP32Deriv] =
    PsbtBIP32DerivsReads

  implicit val rpcPsbtScriptReads: Reads[RpcPsbtScript] = RpcPsbtScriptReads

  implicit val psbtWitnessUtxoInputReads: Reads[PsbtWitnessUtxoInput] =
    Json.reads[PsbtWitnessUtxoInput]

  implicit val mapPubKeySignatureReads: Reads[
    Map[ECPublicKey, ECDigitalSignature]] = MapPubKeySignatureReads

  implicit val rpcPsbtInputReads: Reads[RpcPsbtInput] = RpcPsbtInputReads

  implicit val decodePsbtResultReads: Reads[DecodePsbtResult] =
    Json.reads[DecodePsbtResult]

  implicit val psbtMissingDataReads: Reads[PsbtMissingData] =
    Json.reads[PsbtMissingData]

  implicit val analyzePsbtInputReads: Reads[AnalyzePsbtInput] =
    Json.reads[AnalyzePsbtInput]

  implicit val analyzePsbtResultReads: Reads[AnalyzePsbtResult] =
    Json.reads[AnalyzePsbtResult]

  implicit val getNodeAddressesReads: Reads[GetNodeAddressesResult] =
    Reads[GetNodeAddressesResult] { js =>
      for {
        time <- (js \ "time").validate[Long].map(_.seconds)
        services <- (js \ "services").validate[Int]
        address <- (js \ "address").validate[URI]
        port <- (js \ "port").validate[Int]
      } yield GetNodeAddressesResult(time, services, address, port)
    }

  implicit val rgetpcCommandsReads: Reads[RpcCommands] = Reads[RpcCommands] {
    js =>
      for {
        method <- (js \ "method").validate[String]
        duration <- (js \ "duration").validate[Long].map(_.microseconds)
      } yield RpcCommands(method, duration)
  }

  implicit val getRpcInfoResultReads: Reads[GetRpcInfoResult] =
    Json.reads[GetRpcInfoResult]

  implicit val arrayOfWalletsInputReads: Reads[ArrayOfWalletsInput] =
    Json.reads[ArrayOfWalletsInput]

  implicit val listWalletsDirResultReads: Reads[ListWalletDirResult] =
    Json.reads[ListWalletDirResult]

  implicit val deriveAddressesResultReads: Reads[DeriveAddressesResult] =
    Json.reads[DeriveAddressesResult]

  implicit val submitHeaderResultReads: Reads[SubmitHeaderResult] =
    Json.reads[SubmitHeaderResult]

  implicit val getDescriptorInfoResultReads: Reads[GetDescriptorInfoResult] =
    Json.reads[GetDescriptorInfoResult]

  implicit val walletCreateFundedPsbtResultReads: Reads[
    WalletCreateFundedPsbtResult] =
    Json.reads[WalletCreateFundedPsbtResult]

  implicit val scriptTypeReads: Reads[ScriptType] = ScriptTypeReads

  implicit val testMempoolAcceptResultReads: Reads[TestMempoolAcceptResult] =
    TestMempoolAcceptResultReads

  implicit val createWalletResultReads: Reads[CreateWalletResult] =
    Json.reads[CreateWalletResult]

  implicit val bitcoinerLiveEstimateReads: Reads[BitcoinerLiveEstimate] =
    Json.reads[BitcoinerLiveEstimate]

  implicit val bitcoinerLiveResultReads: Reads[BitcoinerLiveResult] =
    Json.reads[BitcoinerLiveResult]

  // Map stuff
  implicit def mapDoubleSha256DigestReadsPreV19: Reads[
    Map[DoubleSha256Digest, GetMemPoolResultPreV19]] =
    Reads.mapReads[DoubleSha256Digest, GetMemPoolResultPreV19](s =>
      JsSuccess(DoubleSha256Digest.fromHex(s)))

  implicit def mapDoubleSha256DigestReadsPostV19: Reads[
    Map[DoubleSha256Digest, GetMemPoolResultPostV19]] =
    Reads.mapReads[DoubleSha256Digest, GetMemPoolResultPostV19](s =>
      JsSuccess(DoubleSha256Digest.fromHex(s)))

  implicit def mapDoubleSha256DigestBEReadsPreV19: Reads[
    Map[DoubleSha256DigestBE, GetMemPoolResultPreV19]] =
    Reads.mapReads[DoubleSha256DigestBE, GetMemPoolResultPreV19](s =>
      JsSuccess(DoubleSha256DigestBE.fromHex(s)))

  implicit def mapDoubleSha256DigestBEReadsPostV19: Reads[
    Map[DoubleSha256DigestBE, GetMemPoolResultPostV19]] =
    Reads.mapReads[DoubleSha256DigestBE, GetMemPoolResultPostV19](s =>
      JsSuccess(DoubleSha256DigestBE.fromHex(s)))

  implicit def mapAddressesByLabelReads: Reads[
    Map[BitcoinAddress, LabelResult]] =
    Reads.mapReads[BitcoinAddress, LabelResult](s =>
      JsSuccess(BitcoinAddress.fromString(s).get))

  implicit def mapBitcoinerLiveEstimateReads: Reads[
    Map[Int, BitcoinerLiveEstimate]] =
    Reads.mapReads[Int, BitcoinerLiveEstimate](s => JsSuccess(s.toInt))

  implicit val outputMapWrites: Writes[Map[BitcoinAddress, Bitcoins]] =
    mapWrites[BitcoinAddress, Bitcoins](_.value)

}
