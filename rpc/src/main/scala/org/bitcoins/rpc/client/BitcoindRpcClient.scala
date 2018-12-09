package org.bitcoins.rpc.client

import java.net.URI
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import org.bitcoins.rpc.RpcUtil
import org.bitcoins.rpc.client.RpcOpts.AddressType
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.sys.process._
import scala.util.Try

class BitcoindRpcClient(val instance: BitcoindInstance)(
    implicit
    system: ActorSystem) {
  private val resultKey = "result"
  private val errorKey = "error"
  private val logger = BitcoinSLogger.logger
  private implicit val network = instance.network
  private implicit val m = ActorMaterializer.create(system)
  private implicit val ec: ExecutionContext = m.executionContext

  def getDaemon: BitcoindInstance = instance

  def isStarted(): Boolean = {

    def isConnected(): Future[Boolean] = {
      val request = buildRequest(instance, "ping", JsArray.empty)
      val responseF = sendRequest(request)
      responseF.map(r => logger.info(s"response isConnected $r"))
      val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

      // Ping successful if no error can be parsed from the payload
      val result: Future[Boolean] = payloadF.map { payload =>
        (payload \ errorKey).validate[RpcError] match {
          case _: JsSuccess[RpcError] => false
          case _: JsError             => true
        }
      }

      result
    }

    val await = Try(
      RpcUtil.awaitConditionF(conditionF = () => isConnected(),
                              duration = 1.seconds))

    await.isSuccess
  }

  def isConnected(): Future[Boolean] = {
    val request = buildRequest(instance, "ping", JsArray.empty)
    val responseF = sendRequest(request)
    responseF.map(r => logger.debug(s"response isConnected $r"))
    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    // Ping successful if no error can be parsed from the payload
    val result: Future[Boolean] = payloadF.map { payload =>
      (payload \ errorKey).validate[RpcError] match {
        case _: JsSuccess[RpcError] => false
        case _: JsError             => true
      }
    }

    result
  }

  def abandonTransaction(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("abandontransaction", List(JsString(txid.hex)))
  }

  def abortRescan(): Future[Unit] = {
    bitcoindCall[Unit]("abortrescan")
  }

  private def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      account: String = "",
      addressType: Option[AddressType]): Future[MultiSigResult] = {
    def keyToString(key: Either[ECPublicKey, P2PKHAddress]): JsString =
      key match {
        case Right(k) => JsString(k.value)
        case Left(k)  => JsString(k.hex)
      }

    val params =
      if (addressType.isEmpty) {
        List(JsNumber(minSignatures),
             JsArray(keys.map(keyToString)),
             JsString(account))
      } else {
        List(JsNumber(minSignatures),
             JsArray(keys.map(keyToString)),
             JsString(account),
             JsString(RpcOpts.addressTypeString(addressType.get)))
      }

    bitcoindCall[MultiSigResult]("addmultisigaddress", params)
  }

  def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]]): Future[MultiSigResult] =
    addMultiSigAddress(minSignatures, keys, addressType = None)

  def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      account: String): Future[MultiSigResult] =
    addMultiSigAddress(minSignatures, keys, account, None)

  def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      addressType: AddressType): Future[MultiSigResult] =
    addMultiSigAddress(minSignatures, keys, addressType = Some(addressType))

  def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      account: String,
      addressType: AddressType): Future[MultiSigResult] =
    addMultiSigAddress(minSignatures, keys, account, Some(addressType))

  def addNode(address: URI, command: String): Future[Unit] = {
    bitcoindCall[Unit]("addnode",
                       List(JsString(address.getAuthority), JsString(command)))
  }

  def backupWallet(destination: String): Future[Unit] = {
    bitcoindCall[Unit]("backupwallet", List(JsString(destination)))
  }

  def bumpFee(
      txid: DoubleSha256Digest,
      confTarget: Int = 6,
      totalFee: Option[Satoshis] = None,
      replaceable: Boolean = true,
      estimateMode: String = "UNSET"): Future[BumpFeeResult] = {
    val options =
      if (totalFee.isEmpty) {
        List(("confTarget", JsNumber(confTarget)),
             ("replaceable", JsBoolean(replaceable)),
             ("estimate_mode", JsString(estimateMode)))
      } else {
        List(
          ("confTarget", JsNumber(confTarget)),
          ("totalFee", JsNumber(totalFee.get.toBigDecimal)),
          ("replaceable", JsBoolean(replaceable)),
          ("estimate_mode", JsString(estimateMode))
        )
      }

    bitcoindCall[BumpFeeResult]("bumpfee",
                                List(JsString(txid.hex), JsObject(options)))
  }

  def clearBanned(): Future[Unit] = {
    bitcoindCall[Unit]("clearbanned")
  }

  def combineRawTransaction(txs: Vector[Transaction]): Future[Transaction] = {
    bitcoindCall[Transaction]("combinerawtransaction", List(Json.toJson(txs)))
  }

  def createMultiSig(
      minSignatures: Int,
      keys: Vector[ECPublicKey]): Future[MultiSigResult] = {
    bitcoindCall[MultiSigResult](
      "createmultisig",
      List(JsNumber(minSignatures), Json.toJson(keys.map(_.hex))))
  }

  def createRawTransaction(
      inputs: Vector[TransactionInput],
      outputs: Map[BitcoinAddress, Bitcoins],
      locktime: Int = 0): Future[Transaction] = {
    bitcoindCall[Transaction](
      "createrawtransaction",
      List(Json.toJson(inputs), Json.toJson(outputs), JsNumber(locktime)))
  }

  def decodeRawTransaction(transaction: Transaction): Future[RpcTransaction] = {
    bitcoindCall[RpcTransaction]("decoderawtransaction",
                                 List(JsString(transaction.hex)))
  }

  // TODO: add ScriptPubKey.asmHex
  def decodeScript(script: ScriptPubKey): Future[DecodeScriptResult] = {
    bitcoindCall[DecodeScriptResult](
      "decodescript",
      List(JsString(BitcoinSUtil.encodeHex(script.asmBytes))))
  }

  def disconnectNode(address: URI): Future[Unit] = {
    bitcoindCall[Unit]("disconnectnode", List(JsString(address.getAuthority)))
  }

  def dumpPrivKey(address: BitcoinAddress): Future[ECPrivateKey] = {
    bitcoindCall[String]("dumpprivkey", List(JsString(address.value)))
      .map(ECPrivateKey.fromWIFToPrivateKey)
  }

  def dumpWallet(filePath: String): Future[DumpWalletResult] = {
    bitcoindCall[DumpWalletResult]("dumpwallet", List(JsString(filePath)))
  }

  def encryptWallet(passphrase: String): Future[String] = {
    bitcoindCall[String]("encryptwallet", List(JsString(passphrase)))
  }

  // Needs manual testing!
  def estimateSmartFee(
      blocks: Int,
      mode: String = "CONSERVATIVE"): Future[EstimateSmartFeeResult] = {
    bitcoindCall[EstimateSmartFeeResult]("estimatesmartfee",
                                         List(JsNumber(blocks), JsString(mode)))
  }

  private def fundRawTransaction(
      transaction: Transaction,
      options: Option[RpcOpts.FundRawTransactionOptions]): Future[
    FundRawTransactionResult] = {
    val params =
      if (options.isEmpty) {
        List(JsString(transaction.hex))
      } else {
        List(JsString(transaction.hex), Json.toJson(options.get))
      }

    bitcoindCall[FundRawTransactionResult]("fundrawtransaction", params)
  }

  def fundRawTransaction(
      transaction: Transaction): Future[FundRawTransactionResult] =
    fundRawTransaction(transaction, None)

  def fundRawTransaction(
      transaction: Transaction,
      options: RpcOpts.FundRawTransactionOptions): Future[
    FundRawTransactionResult] = fundRawTransaction(transaction, Some(options))

  def generate(
      blocks: Int,
      maxTries: Int = 1000000): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "generate",
      List(JsNumber(blocks), JsNumber(maxTries)))
  }

  def generateToAddress(
      blocks: Int,
      address: BitcoinAddress,
      maxTries: Int = 1000000): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "generatetoaddress",
      List(JsNumber(blocks), JsString(address.toString), JsNumber(maxTries)))
  }

  def getAccount(address: BitcoinAddress): Future[String] = {
    bitcoindCall[String]("getaccount", List(JsString(address.value)))
  }

  def getAccountAddress(account: String): Future[BitcoinAddress] = {
    bitcoindCall[BitcoinAddress]("getaccountaddress", List(JsString(account)))
  }

  private def getAddedNodeInfo(node: Option[URI]): Future[Vector[Node]] = {
    val params =
      if (node.isEmpty) {
        List.empty
      } else {
        List(JsString(node.get.getAuthority))
      }
    bitcoindCall[Vector[Node]]("getaddednodeinfo", params)
  }
  def getAddedNodeInfo: Future[Vector[Node]] = getAddedNodeInfo(None)

  def getAddedNodeInfo(node: URI): Future[Vector[Node]] =
    getAddedNodeInfo(Some(node))

  def getAddressesByAccount(account: String): Future[Vector[BitcoinAddress]] = {
    bitcoindCall[Vector[BitcoinAddress]]("getaddressesbyaccount",
                                         List(JsString(account)))
  }

  def getBalance: Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getbalance")
  }

  def getBestBlockHash: Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getbestblockhash")
  }

  def getBlock(headerHash: DoubleSha256Digest): Future[GetBlockResult] = {
    val isJsonObject = JsNumber(1)
    bitcoindCall[GetBlockResult]("getblock",
                                 List(JsString(headerHash.hex), isJsonObject))
  }

  def getBlockChainInfo: Future[GetBlockChainInfoResult] = {
    bitcoindCall[GetBlockChainInfoResult]("getblockchaininfo")
  }

  def getBlockCount: Future[Int] = {
    bitcoindCall[Int]("getblockcount")
  }

  def getBlockHash(height: Int): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getblockhash", List(JsNumber(height)))
  }

  def getBlockHeader(
      headerHash: DoubleSha256Digest): Future[GetBlockHeaderResult] = {
    bitcoindCall[GetBlockHeaderResult](
      "getblockheader",
      List(JsString(headerHash.hex), JsBoolean(true)))
  }

  def getBlockHeaderRaw(headerHash: DoubleSha256Digest): Future[BlockHeader] = {
    bitcoindCall[BlockHeader]("getblockheader",
                              List(JsString(headerHash.hex), JsBoolean(false)))
  }

  def getBlockRaw(headerHash: DoubleSha256Digest): Future[Block] = {
    bitcoindCall[Block]("getblock", List(JsString(headerHash.hex), JsNumber(0)))
  }

  def getBlockTemplate(
      request: Option[RpcOpts.BlockTemplateRequest] = None): Future[
    GetBlockTemplateResult] = {
    val params =
      if (request.isEmpty) {
        List.empty
      } else {
        List(Json.toJson(request.get))
      }
    bitcoindCall[GetBlockTemplateResult]("getblocktemplate", params)
  }

  def getBlockWithTransactions(headerHash: DoubleSha256Digest): Future[
    GetBlockWithTransactionsResult] = {
    val isVerboseJsonObject = JsNumber(2)
    bitcoindCall[GetBlockWithTransactionsResult](
      "getblock",
      List(JsString(headerHash.hex), isVerboseJsonObject))
  }

  def getChainTips: Future[Vector[ChainTip]] = {
    bitcoindCall[Vector[ChainTip]]("getchaintips")
  }

  private def getChainTxStats(
      blocks: Option[Int],
      blockHash: Option[DoubleSha256Digest]): Future[GetChainTxStatsResult] = {
    val params =
      if (blocks.isEmpty) {
        List.empty
      } else if (blockHash.isEmpty) {
        List(JsNumber(blocks.get))
      } else {
        List(JsNumber(blocks.get), JsString(blockHash.get.hex))
      }
    bitcoindCall[GetChainTxStatsResult]("getchaintxstats", params)
  }

  def getChainTxStats: Future[GetChainTxStatsResult] =
    getChainTxStats(None, None)

  def getChainTxStats(blocks: Int): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), None)

  def getChainTxStats(
      blocks: Int,
      blockHash: DoubleSha256Digest): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), Some(blockHash))

  def getConnectionCount: Future[Int] = {
    bitcoindCall[Int]("getconnectioncount")
  }

  def getDifficulty: Future[BigDecimal] = {
    bitcoindCall[BigDecimal]("getdifficulty")
  }

  def getMemoryInfo: Future[GetMemoryInfoResult] = {
    bitcoindCall[GetMemoryInfoResult]("getmemoryinfo")
  }

  def getMemPoolAncestors(
      txid: DoubleSha256Digest): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def getMemPoolAncestorsVerbose(txid: DoubleSha256Digest): Future[
    Map[DoubleSha256Digest, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256Digest, GetMemPoolResult]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getMemPoolDescendants(
      txid: DoubleSha256Digest): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def getMemPoolDescendantsVerbose(txid: DoubleSha256Digest): Future[
    Map[DoubleSha256Digest, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256Digest, GetMemPoolResult]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getMemPoolEntry(
      txid: DoubleSha256Digest): Future[GetMemPoolEntryResult] = {
    bitcoindCall[GetMemPoolEntryResult]("getmempoolentry",
                                        List(JsString(txid.hex)))
  }

  def getMemPoolInfo: Future[GetMemPoolInfoResult] = {
    bitcoindCall[GetMemPoolInfoResult]("getmempoolinfo")
  }

  def getMiningInfo: Future[GetMiningInfoResult] = {
    bitcoindCall[GetMiningInfoResult]("getmininginfo")
  }

  def getNetTotals: Future[GetNetTotalsResult] = {
    bitcoindCall[GetNetTotalsResult]("getnettotals")
  }

  def getNetworkHashPS(
      blocks: Int = 120,
      height: Int = -1): Future[BigDecimal] = {
    bitcoindCall[BigDecimal]("getnetworkhashps",
                             List(JsNumber(blocks), JsNumber(height)))
  }

  def getNetworkInfo: Future[GetNetworkInfoResult] = {
    bitcoindCall[GetNetworkInfoResult]("getnetworkinfo")
  }

  private def getNewAddress(
      account: String = "",
      addressType: Option[AddressType]): Future[BitcoinAddress] = {
    val params =
      if (addressType.isEmpty) {
        List(JsString(account))
      } else {
        List(JsString(account),
             JsString(RpcOpts.addressTypeString(addressType.get)))
      }

    bitcoindCall[BitcoinAddress]("getnewaddress", params)
  }

  def getNewAddress(): Future[BitcoinAddress] =
    getNewAddress(addressType = None)

  def getNewAddress(account: String): Future[BitcoinAddress] =
    getNewAddress(account, None)

  def getNewAddress(addressType: AddressType): Future[BitcoinAddress] =
    getNewAddress(addressType = Some(addressType))

  def getNewAddress(
      account: String,
      addressType: AddressType): Future[BitcoinAddress] =
    getNewAddress(account, Some(addressType))

  def getPeerInfo: Future[Vector[Peer]] = {
    bitcoindCall[Vector[Peer]]("getpeerinfo")
  }

  private def getRawChangeAddress(
      addressType: Option[AddressType]): Future[BitcoinAddress] = {
    if (addressType.isEmpty) {
      bitcoindCall[BitcoinAddress]("getrawchangeaddress")
    } else {
      bitcoindCall[BitcoinAddress](
        "getrawchangeaddress",
        List(JsString(RpcOpts.addressTypeString(addressType.get))))
    }
  }
  def getRawChangeAddress(): Future[BitcoinAddress] = getRawChangeAddress(None)

  def getRawChangeAddress(addressType: AddressType): Future[BitcoinAddress] =
    getRawChangeAddress(Some(addressType))

  def getRawMemPool: Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]]("getrawmempool",
                                             List(JsBoolean(false)))
  }

  def getRawMemPoolWithTransactions: Future[
    Map[DoubleSha256Digest, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256Digest, GetMemPoolResult]](
      "getrawmempool",
      List(JsBoolean(true)))
  }

  def getRawTransaction(
      txid: DoubleSha256Digest): Future[GetRawTransactionResult] = {
    bitcoindCall[GetRawTransactionResult](
      "getrawtransaction",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getRawTransactionRaw(txid: DoubleSha256Digest): Future[Transaction] = {
    bitcoindCall[Transaction]("getrawtransaction",
                              List(JsString(txid.hex), JsBoolean(false)))
  }

  def getReceivedByAccount(
      account: String,
      confirmations: Int = 1): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbyaccount",
                           List(JsString(account), JsNumber(confirmations)))
  }

  def getReceivedByAddress(
      address: BitcoinAddress,
      minConfirmations: Int = 1): Future[Bitcoins] = {
    bitcoindCall[Bitcoins](
      "getreceivedbyaddress",
      List(JsString(address.toString), JsNumber(minConfirmations)))
  }

  def getTransaction(
      txid: DoubleSha256Digest,
      watchOnly: Boolean = false): Future[GetTransactionResult] = {
    bitcoindCall[GetTransactionResult](
      "gettransaction",
      List(JsString(txid.hex), JsBoolean(watchOnly)))
  }

  def getTxOut(
      txid: DoubleSha256Digest,
      vout: Int,
      includeMemPool: Boolean = true): Future[GetTxOutResult] = {
    bitcoindCall[GetTxOutResult](
      "gettxout",
      List(JsString(txid.hex), JsNumber(vout), JsBoolean(includeMemPool)))
  }

  private def getTxOutProof(
      txids: Vector[DoubleSha256Digest],
      headerHash: Option[DoubleSha256Digest]): Future[MerkleBlock] = {
    val params = {
      val hashes = JsArray(txids.map(hash => JsString(hash.hex)))
      if (headerHash.isEmpty) {
        List(hashes)
      } else {
        List(hashes, JsString(headerHash.get.hex))
      }
    }
    bitcoindCall[MerkleBlock]("gettxoutproof", params)
  }

  def getTxOutProof(txids: Vector[DoubleSha256Digest]): Future[MerkleBlock] =
    getTxOutProof(txids, None)

  def getTxOutProof(
      txids: Vector[DoubleSha256Digest],
      headerHash: DoubleSha256Digest): Future[MerkleBlock] =
    getTxOutProof(txids, Some(headerHash))

  def getTxOutSetInfo: Future[GetTxOutSetInfoResult] = {
    bitcoindCall[GetTxOutSetInfoResult]("gettxoutsetinfo")
  }

  def getUnconfirmedBalance: Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getunconfirmedbalance")
  }

  def getWalletInfo: Future[GetWalletInfoResult] = {
    bitcoindCall[GetWalletInfoResult]("getwalletinfo")
  }

  def help(rpcName: String = ""): Future[String] = {
    bitcoindCall[String]("help", List(JsString(rpcName)))
  }

  def importAddress(
      address: BitcoinAddress,
      account: String = "",
      rescan: Boolean = true,
      p2sh: Boolean = false): Future[Unit] = {
    bitcoindCall[Unit]("importaddress",
                       List(JsString(address.value),
                            JsString(account),
                            JsBoolean(rescan),
                            JsBoolean(p2sh)))
  }

  // This is here so that network is accessible
  implicit object ECPrivateKeyWrites extends Writes[ECPrivateKey] {
    override def writes(o: ECPrivateKey): JsValue = JsString(o.toWIF(network))
  }
  implicit val eCPrivateKeyWrites: Writes[ECPrivateKey] = ECPrivateKeyWrites
  implicit val importMultiAddressWrites: Writes[RpcOpts.ImportMultiAddress] =
    Json.writes[RpcOpts.ImportMultiAddress]
  implicit val importMultiRequestWrites: Writes[RpcOpts.ImportMultiRequest] =
    Json.writes[RpcOpts.ImportMultiRequest]

  def importMulti(
      requests: Vector[RpcOpts.ImportMultiRequest],
      rescan: Boolean = true): Future[Vector[ImportMultiResult]] = {
    bitcoindCall[Vector[ImportMultiResult]](
      "importmulti",
      List(Json.toJson(requests), JsObject(Map("rescan" -> JsBoolean(rescan)))))
  }

  def importPrivKey(
      key: ECPrivateKey,
      account: String = "",
      rescan: Boolean = true): Future[Unit] = {
    bitcoindCall[Unit](
      "importprivkey",
      List(JsString(key.toWIF(network)), JsString(account), JsBoolean(rescan)))
  }

  def importPrunedFunds(
      transaction: Transaction,
      txOutProof: MerkleBlock): Future[Unit] = {
    bitcoindCall[Unit](
      "importprunedfunds",
      List(JsString(transaction.hex), JsString(txOutProof.hex)))
  }

  def importPubKey(
      pubKey: ECPublicKey,
      label: String = "",
      rescan: Boolean = true): Future[Unit] = {
    bitcoindCall[Unit](
      "importpubkey",
      List(JsString(pubKey.hex), JsString(label), JsBoolean(rescan)))
  }

  def importWallet(filePath: String): Future[Unit] = {
    bitcoindCall[Unit]("importwallet", List(JsString(filePath)))
  }

  def invalidateBlock(blockHash: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("invalidateblock", List(JsString(blockHash.hex)))
  }

  def keyPoolRefill(keyPoolSize: Int = 100): Future[Unit] = {
    bitcoindCall[Unit]("keypoolrefill", List(JsNumber(keyPoolSize)))
  }

  def listAccounts(
      confirmations: Int = 1,
      includeWatchOnly: Boolean = false): Future[Map[String, Bitcoins]] = {
    bitcoindCall[Map[String, Bitcoins]](
      "listaccounts",
      List(JsNumber(confirmations), JsBoolean(includeWatchOnly)))
  }

  def listAddressGroupings: Future[Vector[Vector[RpcAddress]]] = {
    bitcoindCall[Vector[Vector[RpcAddress]]]("listaddressgroupings")
  }

  def listBanned: Future[Vector[NodeBan]] = {
    bitcoindCall[Vector[NodeBan]]("listbanned")
  }

  def listLockUnspent: Future[Vector[TransactionOutPoint]] = {
    bitcoindCall[Vector[TransactionOutPoint]]("listlockunspent")
  }

  def listReceivedByAccount(
      confirmations: Int = 1,
      includeEmpty: Boolean = false,
      includeWatchOnly: Boolean = false): Future[Vector[ReceivedAccount]] = {
    bitcoindCall[Vector[ReceivedAccount]]("listreceivedbyaccount",
                                          List(JsNumber(confirmations),
                                               JsBoolean(includeEmpty),
                                               JsBoolean(includeWatchOnly)))
  }

  def listReceivedByAddress(
      confirmations: Int = 1,
      includeEmpty: Boolean = false,
      includeWatchOnly: Boolean = false): Future[Vector[ReceivedAddress]] = {
    bitcoindCall[Vector[ReceivedAddress]]("listreceivedbyaddress",
                                          List(JsNumber(confirmations),
                                               JsBoolean(includeEmpty),
                                               JsBoolean(includeWatchOnly)))
  }

  def listSinceBlock(
      headerHash: Option[DoubleSha256Digest] = None,
      confirmations: Int = 1,
      includeWatchOnly: Boolean = false): Future[ListSinceBlockResult] = {
    val params =
      if (headerHash.isEmpty) {
        List.empty
      } else {
        List(JsString(headerHash.get.hex),
             JsNumber(confirmations),
             JsBoolean(includeWatchOnly))
      }
    bitcoindCall[ListSinceBlockResult]("listsinceblock", params)
  }

  def listSinceBlock: Future[ListSinceBlockResult] = listSinceBlock(None)

  def listSinceBlock(
      headerHash: DoubleSha256Digest): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash))

  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      confirmations: Int): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), confirmations)

  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      includeWatchOnly: Boolean): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), includeWatchOnly = includeWatchOnly)

  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      confirmations: Int,
      includeWatchOnly: Boolean): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), confirmations, includeWatchOnly)

  def listTransactions(
      account: String = "*",
      count: Int = 10,
      skip: Int = 0,
      includeWatchOnly: Boolean = false): Future[
    Vector[ListTransactionsResult]] = {
    bitcoindCall[Vector[ListTransactionsResult]](
      "listtransactions",
      List(JsString(account),
           JsNumber(count),
           JsNumber(skip),
           JsBoolean(includeWatchOnly)))
  }

  private def listUnspent(
      minConfirmations: Int = 1,
      maxConfirmations: Int = 9999999,
      addresses: Option[Vector[BitcoinAddress]]): Future[
    Vector[UnspentOutput]] = {
    val params =
      if (addresses.isEmpty) {
        List(JsNumber(minConfirmations), JsNumber(maxConfirmations))
      } else {
        List(JsNumber(minConfirmations),
             JsNumber(maxConfirmations),
             Json.toJson(addresses.get))
      }
    bitcoindCall[Vector[UnspentOutput]]("listunspent", params)
  }
  def listUnspent: Future[Vector[UnspentOutput]] = listUnspent(addresses = None)

  def listUnspent(
      minConfirmations: Int,
      maxConfirmations: Int): Future[Vector[UnspentOutput]] =
    listUnspent(minConfirmations, maxConfirmations, None)

  def listUnspent(
      addresses: Vector[BitcoinAddress]): Future[Vector[UnspentOutput]] =
    listUnspent(addresses = addresses)

  def listUnspent(
      minConfirmations: Int,
      maxConfirmations: Int,
      addresses: Vector[BitcoinAddress]): Future[Vector[UnspentOutput]] =
    listUnspent(minConfirmations, maxConfirmations, Some(addresses))

  def listWallets: Future[Vector[String]] = {
    bitcoindCall[Vector[String]]("listwallets")
  }

  def lockUnspent(
      unlock: Boolean,
      outputs: Vector[RpcOpts.LockUnspentOutputParameter]): Future[Boolean] = {
    bitcoindCall[Boolean]("lockunspent",
                          List(JsBoolean(unlock), Json.toJson(outputs)))
  }

  private def logging(
      include: Option[Vector[String]],
      exclude: Option[Vector[String]]): Future[Map[String, Int]] = {
    val params =
      if (include.isEmpty && exclude.isEmpty) {
        List.empty
      } else if (include.isEmpty) {
        List(JsArray.empty, Json.toJson(exclude.get))
      } else if (exclude.isEmpty) {
        List(Json.toJson(include.get), JsArray.empty)
      } else {
        List(Json.toJson(include.get), Json.toJson(exclude.get))
      }
    bitcoindCall[Map[String, Int]]("logging", params)
  }
  def logging: Future[Map[String, Int]] = logging(None, None)

  def logging(
      include: Vector[String] = Vector.empty,
      exclude: Vector[String] = Vector.empty): Future[Map[String, Int]] = {
    val inc = if (include.nonEmpty) Some(include) else None
    val exc = if (exclude.nonEmpty) Some(exclude) else None
    logging(inc, exc)
  }

  def move(
      fromAccount: String,
      toAccount: String,
      amount: Bitcoins,
      comment: String = ""): Future[Boolean] = {
    bitcoindCall[Boolean]("move",
                          List(JsString(fromAccount),
                               JsString(toAccount),
                               JsNumber(amount.toBigDecimal),
                               JsNumber(6),
                               JsString(comment)))
  }

  def ping(): Future[Unit] = {
    bitcoindCall[Unit]("ping")
  }

  def preciousBlock(headerHash: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("preciousblock", List(JsString(headerHash.hex)))
  }

  def prioritiseTransaction(
      txid: DoubleSha256Digest,
      feeDelta: Satoshis): Future[Boolean] = {
    bitcoindCall[Boolean](
      "prioritisetransaction",
      List(JsString(txid.hex), JsNumber(0), JsNumber(feeDelta.toLong)))
  }

  def pruneBlockChain(height: Int): Future[Int] = {
    bitcoindCall[Int]("pruneblockchain", List(JsNumber(height)))
  }

  def removePrunedFunds(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("removeprunedfunds", List(JsString(txid.hex)))
  }

  private def rescanBlockChain(
      start: Option[Int],
      stop: Option[Int]): Future[RescanBlockChainResult] = {
    val params =
      if (start.isEmpty) {
        List.empty
      } else if (stop.isEmpty) {
        List(JsNumber(start.get))
      } else {
        List(JsNumber(start.get), JsNumber(stop.get))
      }
    bitcoindCall[RescanBlockChainResult]("rescanblockchain", params)
  }

  def rescanBlockChain(): Future[RescanBlockChainResult] =
    rescanBlockChain(None, None)

  def rescanBlockChain(start: Int): Future[RescanBlockChainResult] =
    rescanBlockChain(Some(start), None)

  def rescanBlockChain(start: Int, stop: Int): Future[RescanBlockChainResult] =
    rescanBlockChain(Some(start), Some(stop))

  def saveMemPool(): Future[Unit] = {
    bitcoindCall[Unit]("savemempool")
  }

  def sendFrom(
      fromAccount: String,
      toAddress: BitcoinAddress,
      amount: Bitcoins,
      confirmations: Int = 1,
      comment: String = "",
      toComment: String = ""): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendfrom",
      List(JsString(fromAccount),
           JsString(toAddress.value),
           JsNumber(amount.toBigDecimal),
           JsNumber(confirmations),
           JsString(comment),
           JsString(toComment))
    )
  }

  def sendMany(
      amounts: Map[BitcoinAddress, Bitcoins],
      minconf: Int = 1,
      comment: String = "",
      subtractFeeFrom: Vector[BitcoinAddress] = Vector.empty): Future[
    DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("sendmany",
                                     List(JsString(""),
                                          Json.toJson(amounts),
                                          JsNumber(minconf),
                                          JsString(comment),
                                          Json.toJson(subtractFeeFrom)))
  }

  def sendToAddress(
      address: BitcoinAddress,
      amount: Bitcoins,
      localComment: String = "",
      toComment: String = "",
      subractFeeFromAmount: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendtoaddress",
      List(JsString(address.toString),
           JsNumber(amount.toBigDecimal),
           JsString(localComment),
           JsString(toComment),
           JsBoolean(subractFeeFromAmount))
    )
  }

  def sendRawTransaction(
      transaction: Transaction,
      allowHighFees: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendrawtransaction",
      List(JsString(transaction.hex), JsBoolean(allowHighFees)))
  }

  def setAccount(address: BitcoinAddress, account: String): Future[Unit] = {
    bitcoindCall[Unit]("setaccount",
                       List(JsString(address.value), JsString(account)))
  }

  def setBan(
      address: URI,
      command: String,
      banTime: Int = 86400,
      absolute: Boolean = false): Future[Unit] = {
    bitcoindCall[Unit]("setban",
                       List(JsString(address.getAuthority),
                            JsString(command),
                            JsNumber(banTime),
                            JsBoolean(absolute)))
  }

  def setNetworkActive(activate: Boolean): Future[Unit] = {
    bitcoindCall[Unit]("setnetworkactive", List(JsBoolean(activate)))
  }

  // TODO: Should be BitcoinFeeUnit
  def setTxFee(feePerKB: Bitcoins): Future[Boolean] = {
    bitcoindCall[Boolean]("settxfee", List(JsNumber(feePerKB.toBigDecimal)))
  }

  def signMessage(address: P2PKHAddress, message: String): Future[String] = {
    bitcoindCall[String]("signmessage",
                         List(JsString(address.value), JsString(message)))
  }

  def signMessageWithPrivKey(
      key: ECPrivateKey,
      message: String): Future[String] = {
    bitcoindCall[String]("signmessagewithprivkey",
                         List(JsString(key.toWIF(network)), JsString(message)))
  }

  private def signRawTransaction(
      transaction: Transaction,
      utxoDeps: Option[Vector[RpcOpts.SignRawTransactionOutputParameter]],
      keys: Option[Vector[ECPrivateKey]],
      sigHash: Option[String]): Future[SignRawTransactionResult] = {

    val utxos = utxoDeps.map(Json.toJson(_)).getOrElse(JsArray.empty)
    val jsonKeys = keys.map(Json.toJson(_)).getOrElse(JsArray.empty)

    val params =
      if (utxoDeps.isEmpty) {
        List(JsString(transaction.hex))
      } else if (keys.isEmpty) {
        List(JsString(transaction.hex), utxos)
      } else if (sigHash.isEmpty) {
        List(JsString(transaction.hex), utxos, jsonKeys)
      } else {
        List(JsString(transaction.hex), utxos, jsonKeys, JsString(sigHash.get))
      }

    bitcoindCall[SignRawTransactionResult]("signrawtransaction", params)
  }

  def signRawTransaction(
      transaction: Transaction): Future[SignRawTransactionResult] =
    signRawTransaction(transaction, None, None, None)

  def signRawTransaction(
      transaction: Transaction,
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter]): Future[
    SignRawTransactionResult] =
    signRawTransaction(transaction, Some(utxoDeps), None, None)

  def signRawTransaction(
      transaction: Transaction,
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter],
      keys: Vector[ECPrivateKey]): Future[SignRawTransactionResult] =
    signRawTransaction(transaction, Some(utxoDeps), Some(keys), None)

  def signRawTransaction(
      transaction: Transaction,
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter],
      keys: Vector[ECPrivateKey],
      sigHash: String): Future[SignRawTransactionResult] =
    signRawTransaction(transaction, Some(utxoDeps), Some(keys), Some(sigHash))

  def stop(): Future[String] = {
    bitcoindCall[String]("stop")
  }

  def submitBlock(block: Block): Future[Unit] = {
    bitcoindCall[Unit]("submitblock", List(JsString(block.hex)))
  }

  def uptime: Future[UInt32] = {
    bitcoindCall[UInt32]("uptime")
  }

  def validateAddress(
      address: BitcoinAddress): Future[ValidateAddressResult] = {
    bitcoindCall[ValidateAddressResult]("validateaddress",
                                        List(JsString(address.toString)))
  }

  def verifyChain(level: Int = 3, blocks: Int = 6): Future[Boolean] = {
    bitcoindCall[Boolean]("verifychain",
                          List(JsNumber(level), JsNumber(blocks)))
  }

  def verifyMessage(
      address: P2PKHAddress,
      signature: String,
      message: String): Future[Boolean] = {
    bitcoindCall[Boolean](
      "verifymessage",
      List(JsString(address.value), JsString(signature), JsString(message)))
  }

  def verifyTxOutProof(
      proof: MerkleBlock): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]]("verifytxoutproof",
                                             List(JsString(proof.hex)))
  }

  def walletLock(): Future[Unit] = {
    bitcoindCall[Unit]("walletlock")
  }

  def walletPassphrase(passphrase: String, seconds: Int): Future[Unit] = {
    bitcoindCall[Unit]("walletpassphrase",
                       List(JsString(passphrase), JsNumber(seconds)))
  }

  def walletPassphraseChange(
      currentPassphrase: String,
      newPassphrase: String): Future[Unit] = {
    bitcoindCall[Unit](
      "walletpassphrasechange",
      List(JsString(currentPassphrase), JsString(newPassphrase)))
  }

  private def bitcoindCall[T](
      command: String,
      parameters: List[JsValue] = List.empty)(
      implicit
      reader: Reads[T]): Future[T] = {
    val request = buildRequest(instance, command, JsArray(parameters))
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      parseResult((payload \ resultKey).validate[T], payload)
    }
  }

  case class RpcError(code: Int, message: String)
  implicit val rpcErrorReads: Reads[RpcError] = Json.reads[RpcError]

  // Should both logging and throwing be happening?
  private def parseResult[T](result: JsResult[T], json: JsValue): T = {
    checkUnitError[T](result, json)

    result match {
      case res: JsSuccess[T] => res.value
      case res: JsError =>
        (json \ errorKey).validate[RpcError] match {
          case err: JsSuccess[RpcError] =>
            logger.error(s"Error ${err.value.code}: ${err.value.message}")
            throw new RuntimeException(
              s"Error ${err.value.code}: ${err.value.message}")
          case _: JsError =>
            logger.error(JsError.toJson(res).toString())
            throw new IllegalArgumentException(
              s"Could not parse JsResult: ${(json \ resultKey).get}")
        }
    }
  }

  // Catches errors thrown by calls with Unit as the expected return type (which isn't handled by UnitReads)
  private def checkUnitError[T](result: JsResult[T], json: JsValue): Unit = {
    if (result == JsSuccess(())) {
      (json \ errorKey).validate[RpcError] match {
        case err: JsSuccess[RpcError] =>
          logger.error(s"Error ${err.value.code}: ${err.value.message}")
          throw new RuntimeException(
            s"Error ${err.value.code}: ${err.value.message}")
        case _: JsError =>
      }
    }
  }

  private def getPayload(response: HttpResponse): Future[JsValue] = {
    val payloadF = response.entity.dataBytes.runFold(ByteString.empty)(_ ++ _)

    payloadF.map { payload =>
      Json.parse(payload.decodeString(ByteString.UTF_8))
    }
  }

  def sendRequest(req: HttpRequest): Future[HttpResponse] = {
    Http(m.system).singleRequest(req)
  }

  def buildRequest(
      instance: BitcoindInstance,
      methodName: String,
      params: JsArray): HttpRequest = {
    val uuid = UUID.randomUUID().toString

    val m: Map[String, JsValue] = Map("method" -> JsString(methodName),
                                      "params" -> params,
                                      "id" -> JsString(uuid))

    val jsObject = JsObject(m)

    logger.debug(s"json rpc request: $m")

    // Would toString work?
    val uri = "http://" + instance.rpcUri.getHost + ":" + instance.rpcUri.getPort
    val username = instance.authCredentials.username
    val password = instance.authCredentials.password
    HttpRequest(
      method = HttpMethods.POST,
      uri,
      entity = HttpEntity(ContentTypes.`application/json`, jsObject.toString()))
      .addCredentials(
        HttpCredentials.createBasicHttpCredentials(username, password))
  }

  def start(): String = {
    val cmd = List("bitcoind",
                   "-datadir=" + instance.authCredentials.datadir,
                   "-rpcport=" + instance.rpcUri.getPort,
                   "-port=" + instance.uri.getPort)
    logger.debug(s"starting bitcoind")
    val _ = Process(cmd).run()
    "Started bitcoind!"
  }
}
