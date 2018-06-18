package org.bitcoins.rpc.client

import java.net.URI

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit
import org.bitcoins.rpc.config.DaemonInstance
import play.api.libs.json._
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._
import org.bitcoins.rpc.serializers.JsonWriters.mapWrites

import scala.concurrent.{ExecutionContext, Future}
import scala.sys.process._

class RpcClient(instance: DaemonInstance)(
    implicit
    m: ActorMaterializer,
    ec: ExecutionContext) {
  private val resultKey = "result"
  private val errorKey = "error"
  private val logger = BitcoinSLogger.logger
  private implicit val network = instance.network

  def getDaemon: DaemonInstance = instance

  // TODO: WRITE TESTS

  def bumpFee(
      txid: DoubleSha256Digest,
      confTarget: Int = 6,
      totalFee: Option[Satoshis],
      replaceable: Boolean = true,
      estimateMode: String = "UNSET"): Future[BumpFeeResult] = {
    val options =
      if (totalFee.nonEmpty) {
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

  // Needs manual testing!
  def estimateSmartFee(
      blocks: Int,
      mode: String = "CONSERVATIVE"): Future[EstimateSmartFeeResult] = {
    bitcoindCall[EstimateSmartFeeResult]("estimatesmartfee",
                                         List(JsNumber(blocks), JsString(mode)))
  }

  def importPrunedFunds(
      transaction: Transaction,
      txOutProof: MerkleBlock): Future[Unit] = {
    bitcoindCall[Unit](
      "importprunedfunds",
      List(JsString(transaction.hex), JsString(txOutProof.hex)))
  }

  def preciousBlock(headerHash: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("preciousblock", List(JsString(headerHash.hex)))
  }

  def prioritiseTransaction(
      txid: DoubleSha256Digest,
      feeDelta: Satoshis): Future[Boolean] = {
    bitcoindCall[Boolean](
      "prioritiseTransaction",
      List(JsString(txid.hex), JsNumber(0), JsNumber(feeDelta.toLong)))
  }

  def pruneBlockChain(height: Int): Future[Int] = {
    bitcoindCall[Int]("pruneblockchain", List(JsNumber(height)))
  }

  def removePrunedFunds(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("removeprunedfunds", List(JsString(txid.hex)))
  }

  // Should be BitcoinFeeUnit
  def setTxFee(feePerKB: Bitcoins): Future[Boolean] = {
    bitcoindCall[Boolean]("settxfee", List(JsNumber(feePerKB.toBigDecimal)))
  }

  def submitBlock(block: Block): Future[Unit] = {
    bitcoindCall[Unit]("submitblock", List(JsString(block.hex)))
  }

  def combineRawTransaction(txs: Vector[Transaction]): Future[Transaction] = {
    bitcoindCall[Transaction]("combinerawtransaction", List(Json.toJson(txs)))
  }

  def importPubKey(pubKey: ScriptPubKey, label: String = "", rescan: Boolean = true): Future[Unit] = {
    bitcoindCall[Unit]("importpubkey", List(JsString(pubKey.hex), JsString(label), JsBoolean(rescan)))
  }

  // TODO: GetBlockTemplate
  // TODO: Overload calls with Option inputs?
  // --------------------------------------------------------------------------------
  // EVERYTHING BELOW THIS COMMENT HAS TESTS

  def abandonTransaction(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("abandontransaction", List(JsString(txid.hex)))
  }

  def abortRescan: Future[Unit] = {
    bitcoindCall[Unit]("abortrescan")
  }

  def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      account: String = "",
      addressType: Option[String] = None): Future[MultiSigResult] = {
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
             JsString(addressType.get))
      }

    bitcoindCall[MultiSigResult]("addmultisigaddress", params)
  }

  def addNode(address: URI, command: String): Future[Unit] = {
    bitcoindCall[Unit]("addnode",
                       List(JsString(address.getAuthority), JsString(command)))
  }

  def backupWallet(destination: String): Future[Unit] = {
    bitcoindCall[Unit]("backupwallet", List(JsString(destination)))
  }

  def clearBanned: Future[Unit] = {
    bitcoindCall[Unit]("clearbanned")
  }

  def createMultiSig(
      minSignatures: Int,
      keys: Vector[ECPublicKey]): Future[MultiSigResult] = {
    bitcoindCall[MultiSigResult](
      "createmultisig",
      List(JsNumber(minSignatures), Json.toJson(keys.map(_.hex))))
  }

  implicit val outputMapWrites: Writes[Map[BitcoinAddress, Bitcoins]] = mapWrites[BitcoinAddress, Bitcoins](_.value)
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

  def dumpPrivKey(address: P2PKHAddress): Future[ECPrivateKey] = {
    bitcoindCall[String]("dumpprivkey", List(JsString(address.value)))
      .map(ECPrivateKey.fromWIFToPrivateKey)
  }

  def dumpWallet(filePath: String): Future[DumpWalletResult] = {
    bitcoindCall[DumpWalletResult]("dumpwallet", List(JsString(filePath)))
  }

  def encryptWallet(passphrase: String): Future[String] = {
    bitcoindCall[String]("encryptwallet", List(JsString(passphrase)))
  }

  def fundRawTransaction(
                          transaction: Transaction,
                          options: Option[RpcOpts.FundRawTransactionOptions] = None): Future[
    FundRawTransactionResult] = {
    val params =
      if (options.isEmpty) {
        List(JsString(transaction.hex))
      } else {
        List(JsString(transaction.hex), Json.toJson(options.get))
      }

    bitcoindCall[FundRawTransactionResult](
      "fundrawtransaction", params)
  }

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

  def getAddedNodeInfo(node: Option[URI] = None): Future[Vector[Node]] = {
    val params =
      if (node.isEmpty) {
        List()
      } else {
        List(JsString(node.get.getAuthority))
      }
    bitcoindCall[Vector[Node]]("getaddednodeinfo", params)
  }

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

  def getChainTxStats(blocks: Option[Int] = None, blockHash: Option[DoubleSha256Digest] = None): Future[GetChainTxStatsResult] = {
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

  def getNewAddress(
      account: String = "",
      addressType: Option[String] = None): Future[BitcoinAddress] = {
    val params =
      if (addressType.isEmpty) {
        List(JsString(account))
      } else {
        List(JsString(account), JsString(addressType.get))
      }

    bitcoindCall[BitcoinAddress]("getnewaddress", params)
  }

  def getPeerInfo: Future[Vector[Peer]] = {
    bitcoindCall[Vector[Peer]]("getpeerinfo")
  }

  // Should have a default addressType set by -changetype
  def getRawChangeAddress(
      addressType: Option[String] = None): Future[BitcoinAddress] = {
    if (addressType.isEmpty) {
      bitcoindCall[BitcoinAddress]("getrawchangeaddress")
    } else {
      bitcoindCall[BitcoinAddress]("getrawchangeaddress",
                                   List(JsString(addressType.get)))
    }
  }

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

  def getRawTransaction(txid: DoubleSha256Digest): Future[GetRawTransactionResult] = {
    bitcoindCall[GetRawTransactionResult]("getrawtransaction",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getRawTransactionRaw(txid: DoubleSha256Digest): Future[Transaction] = {
    bitcoindCall[Transaction]("getrawtransaction",
                              List(JsString(txid.hex), JsBoolean(false)))
  }

  def getReceivedByAccount(account: String, confirmations: Int = 1): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbyaccount", List(JsString(account), JsNumber(confirmations)))
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

  def getTxOutProof(
                     txids: Vector[DoubleSha256Digest],
                     headerHash: Option[DoubleSha256Digest] = None): Future[MerkleBlock] = {
    def params = {
      val hashes = JsArray(txids.map(hash => JsString(hash.hex)))
      if (headerHash.isEmpty) {
        List(hashes)
      } else {
        List(hashes, JsString(headerHash.get.hex))
      }
    }
    bitcoindCall[MerkleBlock]("gettxoutproof", params)
  }

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

  def keyPoolRefill(keyPoolSize: Int = 100): Future[Unit] = {
    bitcoindCall[Unit]("keypoolrefill", List(JsNumber(keyPoolSize)))
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
      List(Json.toJson(requests), JsObject(Seq(("rescan", JsBoolean(rescan))))))
  }

  def importPrivKey(
      key: ECPrivateKey,
      account: String = "",
      rescan: Boolean = true): Future[Unit] = {
    bitcoindCall[Unit](
      "importprivkey",
      List(JsString(key.toWIF(network)), JsString(account), JsBoolean(rescan)))
  }

  def importWallet(filePath: String): Future[Unit] = {
    bitcoindCall[Unit]("importwallet", List(JsString(filePath)))
  }

  def listAccounts(confirmations: Int = 1, includeWatchOnly: Boolean = false): Future[Map[String, Bitcoins]] = {
    bitcoindCall[Map[String, Bitcoins]]("listaccounts", List(JsNumber(confirmations), JsBoolean(includeWatchOnly)))
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

  def listReceivedByAccount(confirmations: Int = 1, includeEmpty: Boolean = false, includeWatchOnly: Boolean = false): Future[Vector[ReceivedAccount]] = {
    bitcoindCall[Vector[ReceivedAccount]]("listreceivedbyaccount", List(JsNumber(confirmations), JsBoolean(includeEmpty), JsBoolean(includeWatchOnly)))
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

  // Need to configure default headerHash
  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      confirmations: Int = 1,
      includeWatchOnly: Boolean = false): Future[ListSinceBlockResult] = {
    bitcoindCall[ListSinceBlockResult]("listsinceblock",
                                       List(JsString(headerHash.hex),
                                            JsNumber(confirmations),
                                            JsBoolean(includeWatchOnly)))
  }

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

  def listUnspent(
                   minConfirmations: Int = 1,
                   maxConfirmations: Int = 9999999,
                   addresses: Option[Vector[BitcoinAddress]] = None): Future[Vector[UnspentOutput]] = {
    val params =
      if (addresses.isEmpty) {
        List(JsNumber(minConfirmations),
          JsNumber(maxConfirmations))
      } else {
        List(JsNumber(minConfirmations),
          JsNumber(maxConfirmations),
          Json.toJson(addresses.get))
      }
    bitcoindCall[Vector[UnspentOutput]]("listunspent", params)
  }

  def listWallets: Future[Vector[String]] = {
    bitcoindCall[Vector[String]]("listwallets")
  }

  def lockUnspent(
                   unlock: Boolean,
                   outputs: Vector[RpcOpts.LockUnspentOutputParameter]): Future[Boolean] = {
    bitcoindCall[Boolean]("lockunspent",
      List(JsBoolean(unlock), Json.toJson(outputs)))
  }

  def logging(include: Option[Vector[String]] = None, exclude: Option[Vector[String]] = None): Future[Map[String, Int]] = {
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

  def move(fromAccount: String, toAccount: String, amount: Bitcoins, comment: String = ""): Future[Boolean] = {
    bitcoindCall[Boolean]("move", List(JsString(fromAccount), JsString(toAccount), JsNumber(amount.toBigDecimal), JsNumber(6), JsString(comment)))
  }

  def ping: Future[Unit] = {
    bitcoindCall[Unit]("ping")
  }

  def rescanBlockChain(start: Option[Int] = None, stop: Option[Int] = None): Future[RescanBlockChainResult] = {
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

  def saveMemPool: Future[Unit] = {
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
    bitcoindCall[Unit]("setaccount", List(JsString(address.value), JsString(account)))
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

  def signRawTransaction(
      transaction: Transaction,
      utxoDeps: Option[Vector[RpcOpts.SignRawTransactionOutputParameter]] = None,
      keys: Option[Vector[ECPrivateKey]] = None,
      sigHash: Option[String] = None): Future[SignRawTransactionResult] = {
    val params =
      if (utxoDeps.isEmpty) {
        List(JsString(transaction.hex))
      } else {
        val utxos = Json.toJson(utxoDeps.get)
        if (keys.isEmpty) {
          List(JsString(transaction.hex), utxos)
        } else {
          val jsonKeys = Json.toJson(keys.get)
          if (sigHash.isEmpty) {
            List(JsString(transaction.hex), utxos, jsonKeys)
          } else {
            List(JsString(transaction.hex),
                 utxos,
                 jsonKeys,
                 JsString(sigHash.get))
          }
        }
      }

    bitcoindCall[SignRawTransactionResult]("signrawtransaction", params)
  }

  def stop: Future[String] = {
    bitcoindCall[String]("stop")
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

  def verifyTxOutProof(proof: MerkleBlock): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]]("verifytxoutproof",
      List(JsString(proof.hex)))
  }

  def walletLock: Future[Unit] = {
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
    // First catch errors thrown by calls with Unit as the expected return type (which isn't handled by UnitReads)
    if (result == JsSuccess(())) {
      (json \ errorKey).validate[RpcError] match {
        case err: JsSuccess[RpcError] =>
          logger.error(s"Error ${err.value.code}: ${err.value.message}")
          throw new RuntimeException(s"Error ${err.value.code}: ${err.value.message}") // More specific type?
        case _ : JsError =>
      }
    }

    result match {
      case res: JsSuccess[T] => res.value
      case res: JsError =>
        (json \ errorKey).validate[RpcError] match {
          case err: JsSuccess[RpcError] =>
            logger.error(s"Error ${err.value.code}: ${err.value.message}")
            throw new RuntimeException(s"Error ${err.value.code}: ${err.value.message}") // More specific type?
          case _ : JsError =>
            logger.error(JsError.toJson(res).toString())
            throw new IllegalArgumentException(s"Could not parse JsResult: ${json \ resultKey}")
        }
    }
  }

  private def getPayload(response: HttpResponse): Future[JsValue] = {
    val payloadF = response.entity.dataBytes.runFold(ByteString(""))(_ ++ _)

    payloadF.map { payload =>
      Json.parse(payload.decodeString(ByteString.UTF_8))
    }
  }

  def sendRequest(req: HttpRequest): Future[HttpResponse] = {
    Http(m.system).singleRequest(req)
  }

  def buildRequest(
      instance: DaemonInstance,
      methodName: String,
      params: JsArray): HttpRequest = {
    val m: Map[String, JsValue] = Map("method" -> JsString(methodName),
                                      "params" -> params,
                                      "id" -> JsString("")) // java.util.UUID
    val jsObject = JsObject(m)

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

  def start(): String =
    ("bitcoind -datadir=" + instance.authCredentials.datadir).!!
}
