package org.bitcoins.rpc.client

import java.io.File
import java.net.InetAddress

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.{Address, P2PKHAddress, P2SHAddress}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{TransactionInput, TransactionOutput}
import org.bitcoins.core.util.BitcoinSLogger
import play.api.libs.json._
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.functional.syntax._

class RpcClient()(implicit m: ActorMaterializer, ec: ExecutionContext) {
  private val resultKey = "result"
  private val logger = BitcoinSLogger.logger

  // TODO: WRITE TESTS

  def abandonTransaction(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("abandontransaction", List(JsString(txid.hex)))
  }

  def addMultiSigAddress(minSignatures: Int, keys: Vector[Either[ECPublicKey, P2PKHAddress]], account: String = ""): Future[Address] = {
    def keyToString(key: Either[ECPublicKey, P2PKHAddress]): JsString = key match {
      case Right(k) => JsString(k.value)
      case Left(k) => JsString(k.hex)
    }
    bitcoindCall[Address]("addmultisigaddress", List(JsNumber(minSignatures), JsArray(keys.map(keyToString)), JsString(account)))
  }

  def addNode(address: InetAddress, command: String): Future[Unit] = {
    bitcoindCall[Unit]("addnode", List(JsString(address.toString), JsString(command)))
  }

  def addWitnessAddress(address: Address): Future[Address] = {
    bitcoindCall[Address]("addwitnessaddress", List(JsString(address.toString)))
  }

  def backupWallet(destination: File): Future[Unit] = {
    bitcoindCall[Unit]("backupwallet", List(JsString(destination.getAbsolutePath)))
  }

  def bumpFee(txid: DoubleSha256Digest, confTarget: Int = 6, totalFee: Option[Satoshis], replaceable: Boolean = true): Future[BumpFeeResult] = {
    val options =
    if (totalFee.nonEmpty)
      List(("confTarget", JsNumber(confTarget)), ("replaceable", JsBoolean(replaceable)))
    else
      List(("confTarget", JsNumber(confTarget)), ("totalFee", JsNumber(totalFee.get.toBigDecimal)), ("replaceable", JsBoolean(replaceable)))

    bitcoindCall[BumpFeeResult]("bumpfee", List(JsString(txid.hex), JsObject(options)))
  }

  def clearBanned: Future[Unit] = {
    bitcoindCall[Unit]("clearbanned")
  }

  def createMultiSig(minSignatures: Int, keys: Vector[Either[ECPublicKey, P2PKHAddress]]): Future[CreateMultiSigResult] = {
    def keyToString(key: Either[ECPublicKey, P2PKHAddress]): JsString = key match {
      case Right(k) => JsString(k.value)
      case Left(k) => JsString(k.hex)
    }
    bitcoindCall[CreateMultiSigResult]("createmultisig", List(JsNumber(minSignatures), JsArray(keys.map(keyToString))))
  }

  //This needs a home
  case class DecodeScriptResult(
                               asm: String,
                               typeOfScript: Option[String],
                               reqSigs: Option[Int],
                               addresses: Option[Vector[P2PKHAddress]],
                               p2sh: P2SHAddress
                               )
  implicit val decodeScriptResultReads: Reads[DecodeScriptResult] = (
    (__ \ "asm").read[String] and
    (__ \ "type").readNullable[String] and
    (__ \ "reqSigs").readNullable[Int] and
    (__ \ "addresses").readNullable[Vector[P2PKHAddress]] and
    (__ \ "p2sh").read[P2SHAddress]
  )(DecodeScriptResult)

  // Is ScriptPubKey the right type for script?
  def decodeScript(script: ScriptPubKey): Future[DecodeScriptResult] = {
    bitcoindCall[DecodeScriptResult]("decodescript", List(JsString(script.hex)))
  }

  def disconnectNode(address: InetAddress): Future[Unit] = {
    bitcoindCall[Unit]("disconnectnode", List(JsString(address.toString)))
  }

  def dumpPrivKey(address: Address): Future[ECPrivateKey] = {
    bitcoindCall[String]("dumpprivkey", List(JsString(address.toString)))
      .map(ECPrivateKey.fromWIFToPrivateKey)
  }

  def dumpWallet(file: File): Future[Unit] = {
    bitcoindCall[Unit]("dumpwallet", List(JsString(file.getAbsolutePath)))
  }

  def encryptWallet(passphrase: String): Future[String] = {
    bitcoindCall[String]("encryptwallet", List(JsString(passphrase)))
  }

  def estimateFee(blocks: Int): Future[Satoshis] = {
    bitcoindCall[Bitcoins]("estimatefee", List(JsNumber(blocks)))
      .map(_.satoshis)
  }

  // Is Double the correct return type? PRIORITY
  def estimatePriority(blocks: Int): Future[Double] = {
    bitcoindCall[Double]("estimatepriority", List(JsNumber(blocks)))
  }

  def generateToAddress(blocks: Int, address: Address, maxTries: Int = 1000000): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]]("generatetoaddress", List(JsNumber(blocks), JsString(address.toString), JsNumber(maxTries)))
  }

  def getAccountAddress(account: String = ""): Future[Address] = {
    bitcoindCall[Address]("getaccountaddress", List(JsString(account)))
  }

  def getAccount(address: Address): Future[String] = {
    bitcoindCall[String]("getaccount", List(JsString(address.toString)))
  }

  def getAddedNodeInfo(details: Boolean, node: Option[InetAddress]): Future[Vector[Node]] = {
    val params = if (node.isEmpty) List(JsBoolean(details)) else List(JsBoolean(details), JsString(node.get.toString))
    bitcoindCall[Vector[Node]]("getaddednodeinfo", params)
  }

  def getAddressByAccount(account: String): Future[Vector[Address]] = {
    bitcoindCall[Vector[Address]]("getaddressesbyaccount", List(JsString(account)))
  }

  def getBalance(account: String = "*", minConfirmations: Int = 0, includeWatchOnly: Boolean = true): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getbalance", List(JsString(account), JsNumber(minConfirmations), JsBoolean(includeWatchOnly)))
  }

  def getBlockRaw(headerHash: DoubleSha256Digest): Future[Block] = {
    bitcoindCall[Block]("getblock", List(JsString(headerHash.hex), JsNumber(0)))
  }

  // This needs a home
  case class GetBlockResult(
                           hash: DoubleSha256Digest,
                           confirmations: Int,
                           strippedsize: Int,
                           size: Int,
                           weight: Int,
                           height: Int,
                           version: Int,
                           versionHex: Int32,
                           merkleroot: DoubleSha256Digest,
                           tx: Vector[DoubleSha256Digest],
                           time: UInt32,
                           mediantime: UInt32,
                           nonce: UInt32,
                           bits: UInt32,
                           difficulty: Double,
                           chainwork: String,
                           previousblockhash: Option[DoubleSha256Digest],
                           nextblockhash: Option[DoubleSha256Digest]
                           )
  implicit val getBlockResultReads: Reads[GetBlockResult] = Json.reads[GetBlockResult]

  def getBlock(headerHash: DoubleSha256Digest): Future[GetBlockResult] = {
    bitcoindCall[GetBlockResult]("getblock", List(JsString(headerHash.hex), JsNumber(1)))
  }

  // This needs a home
  case class GetBlockWithTransactionsResult(
                                           hash: DoubleSha256Digest,
                                           confirmations: Int,
                                           strippedsize: Int,
                                           size: Int,
                                           weight: Int,
                                           height: Int,
                                           version: Int,
                                           versionHex: Int32,
                                           merkleroot: DoubleSha256Digest,
                                           tx: Vector[RpcTransaction],
                                           time: UInt32,
                                           mediantime: UInt32,
                                           nonce: UInt32,
                                           bits: UInt32,
                                           difficulty: Double,
                                           chainwork: String,
                                           previousblockhash: Option[DoubleSha256Digest],
                                           nextblockhash: Option[DoubleSha256Digest]
                                           )

  case class RpcTransaction(
                        txid: DoubleSha256Digest,
                        hash: DoubleSha256Digest,
                        version: Int,
                        size: Int,
                        vsize: Int,
                        locktime: UInt32,
                        vin: Vector[TransactionInput],
                        vout: Vector[TransactionOutput],
                        hex: Block
                        )

  implicit val rpcTransactionReads: Reads[RpcTransaction] = Json.reads[RpcTransaction]
  implicit val getBlockWithTransactionsResultReads: Reads[GetBlockWithTransactionsResult] = Json.reads[GetBlockWithTransactionsResult]

  def getBlockWithTransactions(headerHash: DoubleSha256Digest): Future[GetBlockWithTransactionsResult] = {
    bitcoindCall[GetBlockWithTransactionsResult]("getblock", List(JsString(headerHash.hex), JsNumber(2)))
  }

  def getBlockHash(height: Int): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getblockhash", List(JsNumber(height)))
  }

  // Is Double the correct return type?
  def getDifficulty: Future[Double] = {
    bitcoindCall[Double]("getdifficulty")
  }

  def getMemPoolEntry(txid: DoubleSha256Digest): Future[GetMemPoolEntryResult] = {
    bitcoindCall[GetMemPoolEntryResult]("getmempoolentry", List(JsString(txid.hex)))
  }

  def getMemPoolInfo: Future[GetMemPoolInfoResult] = {
    bitcoindCall[GetMemPoolInfoResult]("getmempoolinfo")
  }

  def getNetworkHashPS(blocks: Int = 120, height: Int = -1): Future[Int] = {
    bitcoindCall[Int]("getnetworkhashps", List(JsNumber(blocks), JsNumber(height)))
  }

  def getRawChangeAddress: Future[P2PKHAddress] = {
    bitcoindCall[P2PKHAddress]("getrawchangeaddress")
  }

  def getReceivedByAccount(account: String, minConfirmations: Int): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbyaccount", List(JsString(account), JsNumber(minConfirmations)))
  }

  def getReceivedByAddress(address: Address, minConfirmations: Int): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbyaddress", List(JsString(address.toString), JsNumber(minConfirmations)))
  }

  // Is String the correct return type?
  def getTxOutProof(txids: Vector[DoubleSha256Digest], headerHash: Option[DoubleSha256Digest]): Future[String] = {
    def params =
      if (headerHash.isEmpty)
        List(JsArray(txids.map(hash => JsString(hash.hex))))
      else
        List(JsArray(txids.map(hash => JsString(hash.hex))), JsString(headerHash.get.hex))
    bitcoindCall[String]("gettxoutproof", params)
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

  def importPrivKey(key: String, account: String = "", rescan: Boolean = true): Future[Unit] = {
    bitcoindCall[Unit]("importprivkey", List(JsString(key), JsString(account), JsBoolean(rescan)))
  }

  def importWallet(file: File): Future[Unit] = {
    bitcoindCall[Unit]("importwallet", List(JsString(file.getName)))
  }

  def keyPoolRefill(keyPoolSize: Int = 0): Future[Unit] = {
    bitcoindCall[Unit]("keypoolrefill", List(JsNumber(keyPoolSize)))
  }

  def listBanned: Future[Vector[NodeBan]] = {
    bitcoindCall[Vector[NodeBan]]("listbanned")
  }

  def ping: Future[Unit] = {
    bitcoindCall[Unit]("ping")
  }

  def preciousBlock(headerHash: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("preciousblock", List(JsString(headerHash.hex)))
  }

  def prioritiseTransaction(txid: DoubleSha256Digest, priority: Double, fee: Satoshis): Future[Boolean] = {
    bitcoindCall[Boolean]("prioritiseTransaction", List(JsString(txid.hex), JsNumber(priority), JsNumber(fee.toLong)))
  }

  def pruneBlockChain(height: Int): Future[Int] = {
    bitcoindCall[Int]("pruneblockchain", List(JsNumber(height)))
  }

  def removePrunedFunds(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("removeprunedfunds", List(JsString(txid.hex)))
  }

  // Is String the right type for transaction? TRANSACTION
  def sendRawTransaction(transaction: String, allowHighFees: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("sendrawtransaction", List(JsString(transaction), JsBoolean(allowHighFees)))
  }

  def sendToAddress(address: Address, amount: Bitcoins, localComment: String = "", toComment: String = "", subractFeeFromAmount: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("sendtoaddress", List(JsString(address.toString), JsNumber(amount.toBigDecimal), JsString(localComment), JsString(toComment), JsBoolean(subractFeeFromAmount)))
  }

  def setBan(address: InetAddress, command: String, banTime: Int = 86400, absolute: Boolean = false): Future[Unit] = {
    bitcoindCall[Unit]("setban", List(JsString(address.getHostAddress), JsString(command), JsNumber(banTime), JsBoolean(absolute)))
  }

  def setNetworkActive(activate: Boolean): Future[Unit] = {
    bitcoindCall[Unit]("setnetworkactive", List(JsBoolean(activate)))
  }

  def setTxFee(feePerKB: Bitcoins): Future[Boolean] = {
    bitcoindCall[Boolean]("settxfee", List(JsNumber(feePerKB.toBigDecimal)))
  }

  def stop: Future[String] = {
    bitcoindCall[String]("stop")
  }

  def validateAddress(address: Address): Future[ValidateAddressResult] = {
    bitcoindCall[ValidateAddressResult]("validateaddress", List(JsString(address.toString)))
  }

  def verifyChain(level: Int, blocks: Int): Future[Boolean] = {
    bitcoindCall[Boolean]("verifychain", List(JsNumber(level), JsNumber(blocks)))
  }

  def verifyTxOutProof(proof: String): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]]("verifytxoutproof", List(JsString(proof)))
  }

  def walletLock: Future[Unit] = {
    bitcoindCall[Unit]("walletlock")
  }

  def walletPassphrase(passphrase: String, seconds: Int): Future[Unit] = {
    bitcoindCall[Unit]("walletpassphrase", List(JsString(passphrase), JsNumber(seconds)))
  }

  def walletPassphraseChange(currentPassphrase: String, newPassphrase: String): Future[Unit] = {
    bitcoindCall[Unit]("walletpassphrasechange", List(JsString(currentPassphrase), JsString(newPassphrase)))
  }

  // Uses string:object pattern - CreateRawTransaction, GetBlockChainInfo, GetMemoryInfo, GetMemPoolAncestors, GetMemPoolDescendants, GetNetTotals, GetPeerInfo, GetTxOut, ImportMulti
  // Uses nameless object - DecodeRawTransaction, FundRawTransaction
  // Also Skipped: GetBlockTemplate, GetRawMemPool, GetRawTransaction, GetTransaction, ImportAddress, ImportPrunedFunds, ListAccounts-Ping, SendFrom, SendMany, SetAccount, SignMessage-SignRawTransaction, SubmitBlock, VerifyMessage
  // TODO: Overload calls with Option inputs?
  // --------------------------------------------------------------------------------
  // EVERYTHING BELOW THIS COMMENT HAS TESTS

  def getBestBlockHash: Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getbestblockhash")
  }

  def getBlockCount: Future[Int] = {
    bitcoindCall[Int]("getblockcount")
  }

  def getConnectionCount: Future[Int] = {
    bitcoindCall[Int]("getconnectioncount")
  }

  def getMiningInfo: Future[GetMiningInfoResult] = {
    bitcoindCall[GetMiningInfoResult]("getmininginfo")
  }

  def getChainTips: Future[Vector[ChainTip]] = {
    bitcoindCall[Vector[ChainTip]]("getchaintips")
  }

  def getNetworkInfo: Future[GetNetworkInfoResult] = {
    bitcoindCall[GetNetworkInfoResult]("getnetworkinfo")
  }

  def getNewAddress(account: String = ""): Future[Address] = {
    bitcoindCall[Address]("getnewaddress", List(JsString(account)))
  }

  def getBlockHeaderRaw(headerHash: DoubleSha256Digest): Future[BlockHeader] = {
    bitcoindCall[BlockHeader]("getblockheader", List(JsString(headerHash.hex), JsBoolean(false)))
  }

  def getBlockHeader(headerHash: DoubleSha256Digest): Future[GetBlockHeaderResult] = {
    bitcoindCall[GetBlockHeaderResult]("getblockheader", List(JsString(headerHash.hex), JsBoolean(true)))
  }

  def generate(blocks: Int, maxTries: Int = 1000000): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]]("generate", List(JsNumber(blocks), JsNumber(maxTries)))
  }

  private def bitcoindCall[T](command: String, parameters: List[JsValue] = List.empty)(implicit reader: Reads[T]): Future[T] = {
    val request = buildRequest(command, JsArray(parameters))
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      parseResult((payload \ resultKey).validate[T])
    }
  }

  private def parseResult[T](result: JsResult[T]): T = {
    result match {
      case res: JsSuccess[T] => res.value
      case res: JsError =>
        logger.error(JsError.toJson(res).toString())
        throw new IllegalArgumentException(s"Could not parse JsResult: $res")
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

  def buildRequest(methodName: String, params: JsArray): HttpRequest = {
    val m: Map[String, JsValue] = Map(
      "method" -> JsString(methodName),
      "params" -> params,
      "id" -> JsString(""))
    val jsObject = JsObject(m)

    val uri = "http://localhost:18332"
    val username = "nadav"
    val password = "abc123"
    HttpRequest(method = HttpMethods.POST, uri,
      entity = HttpEntity(ContentTypes.`application/json`, jsObject.toString()))
      .addCredentials(HttpCredentials.createBasicHttpCredentials(username, password))
  }
}
