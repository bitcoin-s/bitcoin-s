package org.bitcoins.rpc.client

import java.net.InetAddress

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.BitcoinSLogger
import play.api.libs.json._
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._

import scala.concurrent.{ExecutionContext, Future}

class RpcClient()(implicit m: ActorMaterializer, ec: ExecutionContext) {
  private val resultKey = "result"
  private val logger = BitcoinSLogger.logger

  // TODO: WRITE TESTS

  def abandonTransaction(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("abandontransaction", JsArray(List(JsString(txid.hex))))
  }

  // Is String the right type for keys? KEYADDRESS
  def addMultiSigAddress(minSignatures: Int, keys: Array[String], account: String = ""): Future[Address] = {
    bitcoindCall[Address]("addmultisigaddress", JsArray(List(JsNumber(minSignatures), JsArray(keys.map(JsString)), JsString(account))))
  }

  def addNode(address: InetAddress, command: String): Future[Unit] = {
    bitcoindCall[Unit]("addnode", JsArray(List(JsString(address.toString), JsString(command))))
  }

  def addWitnessAddress(address: Address): Future[Address] = {
    bitcoindCall[Address]("addwitnessaddress", JsArray(List(JsString(address.toString))))
  }

  // Is String the correct type for destination? FILE
  def backupWallet(destination: String): Future[Unit] = {
    bitcoindCall[Unit]("backupwallet", JsArray(List(JsString(destination))))
  }

  def bumpFee(txid: DoubleSha256Digest, confTarget: Int = 6, totalFee: Option[Satoshis], replaceable: Boolean = true): Future[BumpFeeResult] = {
    val options =
    if (totalFee.nonEmpty)
      List(("confTarget", JsNumber(confTarget)), ("replaceable", JsBoolean(replaceable)))
    else
      List(("confTarget", JsNumber(confTarget)), ("totalFee", JsNumber(totalFee.get.toBigDecimal)), ("replaceable", JsBoolean(replaceable)))

    bitcoindCall[BumpFeeResult]("bumpfee", JsArray(List(JsString(txid.hex), JsObject(options))))
  }

  def clearBanned: Future[Unit] = {
    bitcoindCall[Unit]("clearbanned")
  }

  // Is String the correct type for keys? KEYADDRESS
  def createMultiSig(minSignatures: Int, keys: Array[String]): Future[CreateMultiSigResult] = {
    bitcoindCall[CreateMultiSigResult]("createmultisig", JsArray(List(JsNumber(minSignatures), JsArray(keys.map(JsString)))))
  }

  def disconnectNode(address: InetAddress): Future[Unit] = {
    bitcoindCall[Unit]("disconnectnode", JsArray(List(JsString(address.toString))))
  }

  // Is String the correct return type? KEYADDRESS
  def dumpPrivKey(address: Address): Future[String] = {
    bitcoindCall[String]("dumpprivkey", JsArray(List(JsString(address.toString))))
  }

  // Is String the right type for file? FILE
  def dumpWallet(file: String): Future[Unit] = {
    bitcoindCall[Unit]("dumpwallet", JsArray(List(JsString(file))))
  }

  def encryptWallet(passphrase: String): Future[String] = {
    bitcoindCall[String]("encryptwallet", JsArray(List(JsString(passphrase))))
  }

  def estimateFee(blocks: Int): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("estimatefee", JsArray(List(JsNumber(blocks))))
  }

  // Is Double the correct return type? PRIORITY
  def estimatePriority(blocks: Int): Future[Double] = {
    bitcoindCall[Double]("estimatepriority", JsArray(List(JsNumber(blocks))))
  }

  def generateToAddress(blocks: Int, address: Address, maxTries: Int = 1000000): Future[Array[DoubleSha256Digest]] = {
    bitcoindCall[Array[DoubleSha256Digest]]("generatetoaddress", JsArray(List(JsNumber(blocks), JsString(address.toString), JsNumber(maxTries))))
  }

  def getAccountAddress(account: String = ""): Future[Address] = {
    bitcoindCall[Address]("getaccountaddress", JsArray(List(JsString(account))))
  }

  def getAccount(address: Address): Future[String] = {
    bitcoindCall[String]("getaccount", JsArray(List(JsString(address.toString))))
  }

  def getAddedNodeInfo(details: Boolean, node: Option[InetAddress]): Future[Array[Node]] = {
    val params = if (node.isEmpty) List(JsBoolean(details)) else List(JsBoolean(details), JsString(node.get.toString))
    bitcoindCall[Array[Node]]("getaddednodeinfo", JsArray(params))
  }

  def getAddressByAccount(account: String): Future[Array[Address]] = {
    bitcoindCall[Array[Address]]("getaddressesbyaccount", JsArray(List(JsString(account))))
  }

  def getBalance(account: String = "*", minConfirmations: Int = 0, includeWatchOnly: Boolean = true): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getbalance", JsArray(List(JsString(account), JsNumber(minConfirmations), JsBoolean(includeWatchOnly))))
  }

  // Is BlockHeader the correct return type? SERIALIZEDBLOCK
  def getBlockRaw(headerHash: DoubleSha256Digest): Future[BlockHeader] = {
    bitcoindCall[BlockHeader]("getblock", JsArray(List(JsString(headerHash.hex), JsNumber(0))))
  }

  def getBlockHash(height: Int): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getblockhash", JsArray(List(JsNumber(height))))
  }

  // Is Double the correct return type?
  def getDifficulty: Future[Double] = {
    bitcoindCall[Double]("getdifficulty")
  }

  def getMemPoolEntry(txid: DoubleSha256Digest): Future[GetMemPoolEntryResult] = {
    bitcoindCall[GetMemPoolEntryResult]("getmempoolentry", JsArray(List(JsString(txid.hex))))
  }

  def getMemPoolInfo: Future[GetMemPoolInfoResult] = {
    bitcoindCall[GetMemPoolInfoResult]("getmempoolinfo")
  }

  def getNetworkHashPS(blocks: Int = 120, height: Int = -1): Future[Int] = {
    bitcoindCall[Int]("getnetworkhashps", JsArray(List(JsNumber(blocks), JsNumber(height))))
  }

  // Is Address the correct return type? ADDRESS
  def getRawChangeAddress: Future[Address] = {
    bitcoindCall[Address]("getrawchangeaddress")
  }

  def getReceivedByAccount(account: String, minConfirmations: Int): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbyaccount", JsArray(List(JsString(account), JsNumber(minConfirmations))))
  }

  def getReceivedByAddress(address: Address, minConfirmations: Int): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbyaddress", JsArray(List(JsString(address.toString), JsNumber(minConfirmations))))
  }

  // Is String the correct return type?
  def getTxOutProof(txids: Array[DoubleSha256Digest], headerHash: Option[DoubleSha256Digest]): Future[String] = {
    def params =
      if (headerHash.isEmpty)
        List(JsArray(txids.map(hash => JsString(hash.hex))))
      else
        List(JsArray(txids.map(hash => JsString(hash.hex))), JsString(headerHash.get.hex))
    bitcoindCall[String]("gettxoutproof", JsArray(params))
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
    bitcoindCall[String]("help", JsArray(List(JsString(rpcName))))
  }

  // Is String the right type for key? KEYADDRESS
  def importPrivKey(key: String, account: String = "", rescan: Boolean = true): Future[Unit] = {
    bitcoindCall[Unit]("importprivkey", JsArray(List(JsString(key), JsString(account), JsBoolean(rescan))))
  }

  // Is String the right type for fileName? FILE
  def importWallet(fileName: String): Future[Unit] = {
    bitcoindCall[Unit]("importwallet", JsArray(List(JsString(fileName))))
  }

  def keyPoolRefill(keyPoolSize: Int = 0): Future[Unit] = {
    bitcoindCall[Unit]("keypoolrefill", JsArray(List(JsNumber(keyPoolSize))))
  }

  def listBanned: Future[Array[NodeBan]] = {
    bitcoindCall[Array[NodeBan]]("listbanned")
  }

  def ping: Future[Unit] = {
    bitcoindCall[Unit]("ping")
  }

  def preciousBlock(headerHash: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("preciousblock", JsArray(List(JsString(headerHash.hex))))
  }

  def prioritiseTransaction(txid: DoubleSha256Digest, priority: Double, fee: Satoshis): Future[Boolean] = {
    bitcoindCall[Boolean]("prioritiseTransaction", JsArray(List(JsString(txid.hex), JsNumber(priority), JsNumber(fee.toLong))))
  }

  def pruneBlockChain(height: Int): Future[Int] = {
    bitcoindCall[Int]("pruneblockchain", JsArray(List(JsNumber(height))))
  }

  def removePrunedFunds(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("removeprunedfunds", JsArray(List(JsString(txid.hex))))
  }

  // Is String the right type for transaction? TRANSACTION
  def sendRawTransaction(transaction: String, allowHighFees: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("sendrawtransaction", JsArray(List(JsString(transaction), JsBoolean(allowHighFees))))
  }

  def sendToAddress(address: Address, amount: Bitcoins, localComment: String = "", toComment: String = "", subractFeeFromAmount: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("sendtoaddress", JsArray(List(JsString(address.toString), JsNumber(amount.toBigDecimal), JsString(localComment), JsString(toComment), JsBoolean(subractFeeFromAmount))))
  }

  def setNetworkActive(activate: Boolean): Future[Unit] = {
    bitcoindCall[Unit]("setnetworkactive", JsArray(List(JsBoolean(activate))))
  }

  def setTxFee(feePerKB: Bitcoins): Future[Boolean] = {
    bitcoindCall[Boolean]("settxfee", JsArray(List(JsNumber(feePerKB.toBigDecimal))))
  }

  def stop: Future[String] = {
    bitcoindCall[String]("stop")
  }

  def validateAddress(address: Address): Future[ValidateAddressResult] = {
    bitcoindCall[ValidateAddressResult]("validateaddress", JsArray(List(JsString(address.toString))))
  }

  def verifyChain(level: Int, blocks: Int): Future[Boolean] = {
    bitcoindCall[Boolean]("verifychain", JsArray(List(JsNumber(level), JsNumber(blocks))))
  }

  def verifyTxOutProof(proof: String): Future[Array[DoubleSha256Digest]] = {
    bitcoindCall[Array[DoubleSha256Digest]]("verifytxoutproof", JsArray(List(JsString(proof))))
  }

  def walletLock: Future[Unit] = {
    bitcoindCall[Unit]("walletlock")
  }

  def walletPassphrase(passphrase: String, seconds: Int): Future[Unit] = {
    bitcoindCall[Unit]("walletpassphrase", JsArray(List(JsString(passphrase), JsNumber(seconds))))
  }

  def walletPassphraseChange(currentPassphrase: String, newPassphrase: String): Future[Unit] = {
    bitcoindCall[Unit]("walletpassphrasechange", JsArray(List(JsString(currentPassphrase), JsString(newPassphrase))))
  }

  // Uses string:object pattern - CreateRawTransaction, GetBlockChainInfo, GetMemoryInfo, GetMemPoolAncestors, GetMemPoolDescendants, GetNetTotals, GetPeerInfo, GetTxOut, ImportMulti
  // Also Skipped: DecodeRawTransaction, DecodeScript, FundRawTransaction, GetBlock, GetBlockTemplate, GetRawMemPool, GetRawTransaction, GetTransaction, ImportAddress, ImportPrunedFunds, ListAccounts-Ping, SendFrom, SendMany, SetAccount, SetBan, SignMessage-SignRawTransaction, SubmitBlock, VerifyMessage
  // Not Yet Reached: Everything after and including ListLockUnspent
  // TODO: Overload calls with Option inputs?
  // TODO: Make bitcoindCall take care off JsArray(List(_))
  // TODO: Make an object extending Reads[InetAddress]
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

  def getChainTips: Future[Array[ChainTip]] = {
    bitcoindCall[Array[ChainTip]]("getchaintips")
  }

  def getNetworkInfo: Future[GetNetworkInfoResult] = {
    bitcoindCall[GetNetworkInfoResult]("getnetworkinfo")
  }

  def getNewAddress(account: String = ""): Future[Address] = {
    bitcoindCall[Address]("getnewaddress", JsArray(List(JsString(account))))
  }

  def getBlockHeaderRaw(headerHash: DoubleSha256Digest): Future[BlockHeader] = {
    bitcoindCall[BlockHeader]("getblockheader", JsArray(List(JsString(headerHash.hex), JsBoolean(false))))
  }

  def getBlockHeader(headerHash: DoubleSha256Digest): Future[GetBlockHeaderResult] = {
    bitcoindCall[GetBlockHeaderResult]("getblockheader", JsArray(List(JsString(headerHash.hex), JsBoolean(true))))
  }

  def generate(blocks: Int, maxTries: Int = 1000000): Future[Array[DoubleSha256Digest]] = {
    bitcoindCall[Array[DoubleSha256Digest]]("generate", JsArray(List(JsNumber(blocks), JsNumber(maxTries))))
  }

  private def bitcoindCall[T](command: String, parameters: JsArray = JsArray.empty)(implicit reader: Reads[T]): Future[T] = {
    val request = buildRequest(command, parameters)
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
