package org.bitcoins.rpc.client

import java.net.InetAddress

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.BitcoinSLogger
import play.api.libs.json._
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._

import scala.concurrent.{ExecutionContext, Future}

class RpcClient {
  private val resultKey = "result"
  private val logger = BitcoinSLogger.logger

  // TODO: WRITE TESTS

  def abandonTransaction(txid: DoubleSha256Digest)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Unit] = {
    bitcoindCall[Unit]("abandontransaction", JsArray(List(JsString(txid.hex))))
  }

  // Is String the right type for keys? KEYADDRESS
  def addMultiSigAddress(minSignatures: Int, keys: Array[String], account: String = "")(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Address] = {
    bitcoindCall[Address]("addmultisigaddress", JsArray(List(JsNumber(minSignatures), JsArray(keys.map(JsString)), JsString(account))))
  }

  def addNode(address: InetAddress, command: String)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Unit] = {
    bitcoindCall[Unit]("addnode", JsArray(List(JsString(address.toString), JsString(command))))
  }

  def addWitnessAddress(address: Address)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Address] = {
    bitcoindCall[Address]("addwitnessaddress", JsArray(List(JsString(address.toString))))
  }

  // Is String the correct type for destination? FILE
  def backupWallet(destination: String)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Unit] = {
    bitcoindCall[Unit]("backupwallet", JsArray(List(JsString(destination))))
  }

  // This needs a home
  case class BumpFeeResult(
                          txid: DoubleSha256Digest,
                          origfee: Bitcoins,
                          fee: Bitcoins,
                          warnings: String
                          )
  implicit val bumpFeeReads: Reads[BumpFeeResult] = Json.reads[BumpFeeResult]

  def bumpFee(txid: DoubleSha256Digest, confTarget: Int = 6, totalFee: Option[Satoshis], replaceable: Boolean = true)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[BumpFeeResult] = {
    val options =
    if (totalFee.nonEmpty)
      List(("confTarget", JsNumber(confTarget)), ("replaceable", JsBoolean(replaceable)))
    else
      List(("confTarget", JsNumber(confTarget)), ("totalFee", JsNumber(totalFee.get.toBigDecimal)), ("replaceable", JsBoolean(replaceable)))

    bitcoindCall[BumpFeeResult]("bumpfee", JsArray(List(JsString(txid.hex), JsObject(options))))
  }

  def clearBanned(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Unit] = {
    bitcoindCall[Unit]("clearbanned")
  }

  // This needs a home
  // Is String the right type for reedemScript? SCRIPT
  case class CreateMultiSigResult(address: Address, redeemScript: String)
  implicit val createMultiSigReads: Reads[CreateMultiSigResult] = Json.reads[CreateMultiSigResult]

  // Is String the correct type for keys? KEYADDRESS
  def createMultiSig(minSignatures: Int, keys: Array[String])(implicit m: ActorMaterializer, ec: ExecutionContext): Future[CreateMultiSigResult] = {
    bitcoindCall[CreateMultiSigResult]("createmultisig", JsArray(List(JsNumber(minSignatures), JsArray(keys.map(JsString)))))
  }

  def disconnectNode(address: InetAddress)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Unit] = {
    bitcoindCall[Unit]("disconnectnode", JsArray(List(JsString(address.toString))))
  }

  // Is String the correct return type? KEYADDRESS
  def dumpPrivKey(address: Address)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[String] = {
    bitcoindCall[String]("dumpprivkey", JsArray(List(JsString(address.toString))))
  }

  // Is String the right type for file? FILE
  def dumpWallet(file: String)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Unit] = {
    bitcoindCall[Unit]("dumpwallet", JsArray(List(JsString(file))))
  }

  def encryptWallet(passphrase: String)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[String] = {
    bitcoindCall[String]("encryptwallet", JsArray(List(JsString(passphrase))))
  }

  def estimateFee(blocks: Int)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("estimatefee", JsArray(List(JsNumber(blocks))))
  }

  // Is Double the correct return type? PRIORITY
  def estimatePriority(blocks: Int)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Double] = {
    bitcoindCall[Double]("estimatepriority", JsArray(List(JsNumber(blocks))))
  }

  def generateToAddress(blocks: Int, address: Address, maxTries: Int = 1000000)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[DoubleSha256Digest]] = {
    bitcoindCall[Array[DoubleSha256Digest]]("generatetoaddress", JsArray(List(JsNumber(blocks), JsString(address.toString), JsNumber(maxTries))))
  }

  def getAccountAddress(account: String = "")(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Address] = {
    bitcoindCall[Address]("getaccountaddress", JsArray(List(JsString(account))))
  }

  def getAccount(address: Address)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[String] = {
    bitcoindCall[String]("getaccount", JsArray(List(JsString(address.toString))))
  }

  // This needs a home
  case class Node(
                 addednode: InetAddress, // Need to add Reads[InetAddress]
                 connected: Option[Boolean],
                 addresses: Option[Array[NodeAddress]]
                 )
  case class NodeAddress(
                        address: InetAddress,
                        connected: String
                        )
  implicit val nodeAddressReads: Reads[NodeAddress] = Json.reads[NodeAddress]
  implicit val nodeReads: Reads[Node] = Json.reads[Node]

  def getAddedNodeInfo(details: Boolean, node: Option[InetAddress])(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[Node]] = {
    val params = if (node.isEmpty) List(JsBoolean(details)) else List(JsBoolean(details), JsString(node.get.toString))
    bitcoindCall[Array[Node]]("getaddednodeinfo", JsArray(params))
  }

  def getAddressByAccount(account: String)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[Address]] = {
    bitcoindCall[Array[Address]]("getaddressesbyaccount", JsArray(List(JsString(account))))
  }

  def getBalance(account: String = "*", minConfirmations: Int = 0, includeWatchOnly: Boolean = true)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getbalance", JsArray(List(JsString(account), JsNumber(minConfirmations), JsBoolean(includeWatchOnly))))
  }

  // Is BlockHeader the correct return type? SERIALIZEDBLOCK
  def getBlockRaw(headerHash: DoubleSha256Digest)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[BlockHeader] = {
    bitcoindCall[BlockHeader]("getblock", JsArray(List(JsString(headerHash.hex), JsNumber(0))))
  }

  def getBlockHash(height: Int)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getblockhash", JsArray(List(JsNumber(height))))
  }

  // Is Double the correct return type?
  def getDifficulty(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Double] = {
    bitcoindCall[Double]("getdifficulty")
  }

  //This needs a home
  // Is Double the correct type for priorities? PRIORITY
  case class GetMemPoolEntryResult(
                                  size: Int,
                                  fee: Bitcoins,
                                  modifiedfee: Bitcoins,
                                  time: UInt32,
                                  height: Int,
                                  startingpriority: Double,
                                  currentpriority: Double,
                                  descendantcount: Int,
                                  descendantsize: Int,
                                  descendantfees: Int,
                                  ancestorcount: Int,
                                  ancestorsize: Int,
                                  ancestorfees: Int,
                                  depends: Option[Array[DoubleSha256Digest]]
                                  )
  implicit val getMemPoolEntryResultReads: Reads[GetMemPoolEntryResult] = Json.reads[GetMemPoolEntryResult]

  def getMemPoolEntry(txid: DoubleSha256Digest)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetMemPoolEntryResult] = {
    bitcoindCall[GetMemPoolEntryResult]("getmempoolentry", JsArray(List(JsString(txid.hex))))
  }

  // This needs a home
  case class GetMemPoolInfoResult(
                                 size: Int,
                                 bytes: Int,
                                 usage: Int,
                                 maxmempool: Int,
                                 mempoolminfee: Bitcoins,
                                 minrelaytxfee: Bitcoins
                                 )
  implicit val getMemPoolInfoResultReads: Reads[GetMemPoolInfoResult] = Json.reads[GetMemPoolInfoResult]

  def getMemPoolInfo(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetMemPoolInfoResult] = {
    bitcoindCall[GetMemPoolInfoResult]("getmempoolinfo")
  }

  def getNetworkHashPS(blocks: Int = 120, height: Int = -1)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Int] = {
    bitcoindCall[Int]("getnetworkhashps", JsArray(List(JsNumber(blocks), JsNumber(height))))
  }

  // Is Address the correct return type? ADDRESS
  def getRawChangeAddress(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Address] = {
    bitcoindCall[Address]("getrawchangeaddress")
  }

  def getReceivedByAccount(account: String, minConfirmations: Int)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbyaccount", JsArray(List(JsString(account), JsNumber(minConfirmations))))
  }

  def getReceivedByAddress(address: Address, minConfirmations: Int)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbyaddress", JsArray(List(JsString(address.toString), JsNumber(minConfirmations))))
  }

  // Is String the correct return type?
  def getTxOutProof(txids: Array[DoubleSha256Digest], headerHash: Option[DoubleSha256Digest])(implicit m: ActorMaterializer, ec: ExecutionContext): Future[String] = {
    def params =
      if (headerHash.isEmpty)
        List(JsArray(txids.map(hash => JsString(hash.hex))))
      else
        List(JsArray(txids.map(hash => JsString(hash.hex))), JsString(headerHash.get.hex))
    bitcoindCall[String]("gettxoutproof", JsArray(params))
  }

  // This needs a home
  case class GetTxOutSetInfoResult(
                                  height: Int,
                                  bestblock: DoubleSha256Digest,
                                  transactions: Int,
                                  txouts: Int,
                                  bytes_serialized: Int,
                                  hash_serialized: DoubleSha256Digest,
                                  total_amount: Bitcoins
                                  )
  implicit val getTxOutSetInfoResultReads: Reads[GetTxOutSetInfoResult] = Json.reads[GetTxOutSetInfoResult]

  def getTxOutSetInfo(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetTxOutSetInfoResult] = {
    bitcoindCall[GetTxOutSetInfoResult]("gettxoutsetinfo")
  }

  def getUnconfirmedBalance(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getunconfirmedbalance")
  }

  // This needs a home
  case class GetWalletInfoResult(
                                walletname: String, // Is this right? FILE
                                walletversion: Int,
                                balance: Bitcoins,
                                unconfirmed_balance: Bitcoins,
                                immature_balance: Bitcoins,
                                txcount: Int,
                                keypoololdest: UInt32,
                                keypoolsize: Int,
                                keypoolsize_hd_internal: Int,
                                paytxfee: Bitcoins,
                                hdmasterkeyid: DoubleSha256Digest, // Is this right?
                                unlocked_until: Option[Int]
                                )
  implicit val getWalletInfoResultReads: Reads[GetWalletInfoResult] = Json.reads[GetWalletInfoResult]

  def getWalletInfo(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetWalletInfoResult] = {
    bitcoindCall[GetWalletInfoResult]("getwalletinfo")
  }

  def help(rpcName: String = "")(implicit m: ActorMaterializer, ec: ExecutionContext): Future[String] = {
    bitcoindCall[String]("help", JsArray(List(JsString(rpcName))))
  }

  // Is String the right type for key? KEYADDRESS
  def importPrivKey(key: String, account: String = "", rescan: Boolean = true)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Unit] = {
    bitcoindCall[Unit]("importprivkey", JsArray(List(JsString(key), JsString(account), JsBoolean(rescan))))
  }

  // Is String the right type for fileName? FILE
  def importWallet(fileName: String)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Unit] = {
    bitcoindCall[Unit]("importwallet", JsArray(List(JsString(fileName))))
  }

  def keyPoolRefill(keyPoolSize: Int = 0)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Unit] = {
    bitcoindCall[Unit]("keypoolrefill", JsArray(List(JsNumber(keyPoolSize))))
  }

  // This needs a home
  case class NodeBan(
                    address: InetAddress,
                    banned_until: UInt32,
                    ban_created: UInt32,
                    ban_reason: String
                    )
  implicit val nodeBanReads: Reads[NodeBan] = Json.reads[NodeBan]

  def listBanned(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[NodeBan]] = {
    bitcoindCall[Array[NodeBan]]("listbanned")
  }

  // Uses string:object pattern - CreateRawTransaction, GetBlockChainInfo, GetMemoryInfo, GetMemPoolAncestors, GetMemPoolDescendants, GetNetTotals, GetPeerInfo, GetTxOut, ImportMulti
  // Also Skipped: DecodeRawTransaction, DecodeScript, FundRawTransaction, GetBlock, GetBlockTemplate, GetRawMemPool, GetRawTransaction, GetTransaction, ImportAddress, ImportPrunedFunds, ListAccounts, ListAddressGroupings
  // Not Yet Reached: Everything after and including ListLockUnspent
  // TODO: Overload calls with Option inputs?
  // TODO: Make bitcoindCall take care off JsArray(List(_))
  // TODO: Make an object extending Reads[InetAddress]
  // --------------------------------------------------------------------------------
  // EVERYTHING BELOW THIS COMMENT HAS TESTS

  def getBestBlockHash(implicit m: ActorMaterializer, ec: ExecutionContext): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getbestblockhash")
  }

  def getBlockCount(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Int] = {
    bitcoindCall[Int]("getblockcount")
  }

  def getConnectionCount(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Int] = {
    bitcoindCall[Int]("getconnectioncount")
  }

  def getMiningInfo(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetMiningInfoResult] = {
    bitcoindCall[GetMiningInfoResult]("getmininginfo")
  }

  def getChainTips(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[ChainTip]] = {
    bitcoindCall[Array[ChainTip]]("getchaintips")
  }

  def getNetworkInfo(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetNetworkInfoResult] = {
    bitcoindCall[GetNetworkInfoResult]("getnetworkinfo")
  }

  def getNewAddress(account: String = "")(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Address] = {
    bitcoindCall[Address]("getnewaddress", JsArray(List(JsString(account))))
  }

  def getBlockHeaderRaw(headerHash: DoubleSha256Digest)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[BlockHeader] = {
    bitcoindCall[BlockHeader]("getblockheader", JsArray(List(JsString(headerHash.hex), JsBoolean(false))))
  }

  def getBlockHeader(headerHash: DoubleSha256Digest)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[GetBlockHeaderResult] = {
    bitcoindCall[GetBlockHeaderResult]("getblockheader", JsArray(List(JsString(headerHash.hex), JsBoolean(true))))
  }

  def generate(blocks: Int, maxTries: Int = 1000000)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[Array[DoubleSha256Digest]] = {
    bitcoindCall[Array[DoubleSha256Digest]]("generate", JsArray(List(JsNumber(blocks), JsNumber(maxTries))))
  }

  private def bitcoindCall[T](command: String, parameters: JsArray = JsArray.empty)
                             (implicit m: ActorMaterializer, ec: ExecutionContext, reader: Reads[T]): Future[T] = {
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
        throw new IllegalArgumentException(s"Could not parse JsResult: ${res}")
    }
  }

  private def getPayload(response: HttpResponse)(implicit m: ActorMaterializer, ec: ExecutionContext): Future[JsValue] = {
    val payloadF = response.entity.dataBytes.runFold(ByteString(""))(_ ++ _)

    payloadF.map { payload =>
      Json.parse(payload.decodeString(ByteString.UTF_8))
    }
  }

  def sendRequest(req: HttpRequest)(implicit m: ActorMaterializer): Future[HttpResponse] = {
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
