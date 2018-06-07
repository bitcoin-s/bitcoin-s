package org.bitcoins.rpc.client

import java.io.File
import java.net.InetAddress

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.{Address, BitcoinAddress, P2PKHAddress}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.core.util.BitcoinSLogger
import play.api.libs.json._
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._

import scala.concurrent.{ExecutionContext, Future}

class RpcClient()(
    implicit m: ActorMaterializer,
    ec: ExecutionContext,
    network: NetworkParameters) {
  private val resultKey = "result"
  private val logger = BitcoinSLogger.logger

  // TODO: WRITE TESTS

  def abandonTransaction(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("abandontransaction", List(JsString(txid.hex)))
  }

  def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      account: String = ""): Future[BitcoinAddress] = {
    def keyToString(key: Either[ECPublicKey, P2PKHAddress]): JsString =
      key match {
        case Right(k) => JsString(k.value)
        case Left(k)  => JsString(k.hex)
      }
    bitcoindCall[BitcoinAddress]("addmultisigaddress",
                                 List(JsNumber(minSignatures),
                                      JsArray(keys.map(keyToString)),
                                      JsString(account)))
  }

  def addNode(address: InetAddress, command: String): Future[Unit] = {
    bitcoindCall[Unit]("addnode",
                       List(JsString(address.toString), JsString(command)))
  }

  def addWitnessAddress(address: BitcoinAddress): Future[BitcoinAddress] = {
    bitcoindCall[BitcoinAddress]("addwitnessaddress",
                                 List(JsString(address.toString)))
  }

  def backupWallet(destination: File): Future[Unit] = {
    bitcoindCall[Unit]("backupwallet",
                       List(JsString(destination.getAbsolutePath)))
  }

  def bumpFee(
      txid: DoubleSha256Digest,
      confTarget: Int = 6,
      totalFee: Option[Satoshis],
      replaceable: Boolean = true): Future[BumpFeeResult] = {
    val options =
      if (totalFee.nonEmpty) {
        List(("confTarget", JsNumber(confTarget)),
             ("replaceable", JsBoolean(replaceable)))
      } else {
        List(("confTarget", JsNumber(confTarget)),
             ("totalFee", JsNumber(totalFee.get.toBigDecimal)),
             ("replaceable", JsBoolean(replaceable)))
      }

    bitcoindCall[BumpFeeResult]("bumpfee",
                                List(JsString(txid.hex), JsObject(options)))
  }

  def clearBanned: Future[Unit] = {
    bitcoindCall[Unit]("clearbanned")
  }

  def createMultiSig(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]]): Future[
    CreateMultiSigResult] = {
    def keyToString(key: Either[ECPublicKey, P2PKHAddress]): JsString =
      key match {
        case Right(k) => JsString(k.value)
        case Left(k)  => JsString(k.hex)
      }
    bitcoindCall[CreateMultiSigResult](
      "createmultisig",
      List(JsNumber(minSignatures), JsArray(keys.map(keyToString))))
  }

  def decodeScript(script: String): Future[DecodeScriptResult] = {
    bitcoindCall[DecodeScriptResult]("decodescript", List(JsString(script)))
  }

  def disconnectNode(address: InetAddress): Future[Unit] = {
    bitcoindCall[Unit]("disconnectnode", List(JsString(address.toString)))
  }

  def dumpPrivKey(address: P2PKHAddress): Future[ECPrivateKey] = {
    bitcoindCall[String]("dumpprivkey", List(JsString(address.value)))
      .map(ECPrivateKey.fromWIFToPrivateKey)
  }

  def dumpWallet(file: File): Future[Unit] = {
    bitcoindCall[Unit]("dumpwallet", List(JsString(file.getAbsolutePath)))
  }

  def encryptWallet(passphrase: String): Future[String] = {
    bitcoindCall[String]("encryptwallet", List(JsString(passphrase)))
  }

  def estimateSmartFee(
      blocks: Int,
      mode: String = "CONSERVATIVE"): Future[EstimateSmartFeeResult] = {
    bitcoindCall[EstimateSmartFeeResult]("estimatefee",
                                         List(JsNumber(blocks), JsString(mode)))
  }

  def fundRawTransaction(
      transaction: Transaction,
      options: Option[RpcOpts.FundRawTransactionOptions]): Future[
    FundRawTransactionResult] = {
    bitcoindCall[FundRawTransactionResult](
      "fundrawtransaction",
      List(JsString(transaction.hex), Json.toJson(options)))
  }

  def generateToAddress(
      blocks: Int,
      address: Address,
      maxTries: Int = 1000000): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "generatetoaddress",
      List(JsNumber(blocks), JsString(address.toString), JsNumber(maxTries)))
  }

  def getAddedNodeInfo(
      details: Boolean,
      node: Option[InetAddress]): Future[Vector[Node]] = {
    val params =
      if (node.isEmpty) {
        List(JsBoolean(details))
      } else {
        List(JsBoolean(details), JsString(node.get.toString))
      }
    bitcoindCall[Vector[Node]]("getaddednodeinfo", params)
  }

  def getAddressesByAccount(account: String): Future[Vector[Address]] = {
    bitcoindCall[Vector[Address]]("getaddressesbyaccount",
                                  List(JsString(account)))
  }

  def getRawTransactionRaw(txid: DoubleSha256Digest): Future[Transaction] = {
    bitcoindCall[Transaction]("getrawtransaction",
                              List(JsString(txid.hex), JsBoolean(false)))
  }

  // As is, RpcTransaction may not have enough data (add options?), also is RpcTransaction in the right place?
  def getRawTransaction(txid: DoubleSha256Digest): Future[RpcTransaction] = {
    bitcoindCall[RpcTransaction]("getrawtransaction",
                                 List(JsString(txid.hex), JsBoolean(true)))
  }

  def getTxOutProof(
      txids: Vector[DoubleSha256Digest],
      headerHash: Option[DoubleSha256Digest]): Future[MerkleBlock] = {
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

  def getWalletInfo: Future[GetWalletInfoResult] = {
    bitcoindCall[GetWalletInfoResult]("getwalletinfo")
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

  def importWallet(file: File): Future[Unit] = {
    bitcoindCall[Unit]("importwallet", List(JsString(file.getName)))
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

  def listReceivedByAddress(
      confirmations: Int = 1,
      includeEmpty: Boolean = false,
      includeWatchOnly: Boolean = false,
      addressFilter: Option[String] = None): Future[Vector[ReceivedAddress]] = {
    val params =
      if (addressFilter.isEmpty) {
        List(JsNumber(confirmations),
             JsBoolean(includeEmpty),
             JsBoolean(includeWatchOnly))
      } else {
        List(JsNumber(confirmations),
             JsBoolean(includeEmpty),
             JsBoolean(includeWatchOnly),
             JsString(addressFilter.get))
      }
    bitcoindCall[Vector[ReceivedAddress]]("listreceivedbyaddress", params)
  }

  def listUnspent(
      minConfirmations: Int = 1,
      maxConfirmations: Int = 9999999,
      addresses: Vector[BitcoinAddress]): Future[Vector[UnspentOutput]] = {
    val jsonAddresses = JsArray(
      addresses.map(address => JsString(address.value)))
    bitcoindCall[Vector[UnspentOutput]]("listunspent",
                                        List(JsNumber(minConfirmations),
                                             JsNumber(maxConfirmations),
                                             jsonAddresses))
  }

  def lockUnspent(
      unlock: Boolean,
      outputs: Vector[LockUnspentOutputParameter]): Future[Boolean] = {
    bitcoindCall[Boolean]("lockunspent",
                          List(JsBoolean(unlock), Json.toJson(outputs)))
  }

  def preciousBlock(headerHash: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("preciousblock", List(JsString(headerHash.hex)))
  }

  def prioritiseTransaction(
      txid: DoubleSha256Digest,
      dummy: BigDecimal = 0,
      feeDelta: Satoshis): Future[Boolean] = {
    bitcoindCall[Boolean](
      "prioritiseTransaction",
      List(JsString(txid.hex), JsNumber(dummy), JsNumber(feeDelta.toLong)))
  }

  def pruneBlockChain(height: Int): Future[Int] = {
    bitcoindCall[Int]("pruneblockchain", List(JsNumber(height)))
  }

  def removePrunedFunds(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("removeprunedfunds", List(JsString(txid.hex)))
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

  def setBan(
      address: InetAddress,
      command: String,
      banTime: Int = 86400,
      absolute: Boolean = false): Future[Unit] = {
    bitcoindCall[Unit]("setban",
                       List(JsString(address.getHostAddress),
                            JsString(command),
                            JsNumber(banTime),
                            JsBoolean(absolute)))
  }

  // Should be BitcoinFeeUnit
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

  def signRawTransactionWithKey(
      transaction: Transaction,
      keys: Vector[ECPrivateKey],
      utxoDeps: Option[Vector[RpcOpts.SignRawTransactionOutputParameter]] = None,
      sigHash: Option[String] = None): Future[SignRawTransactionResult] = {
    val params = {
      val jsonKeys = JsArray(keys.map(key => JsString(key.toWIF(network))))
      if (utxoDeps.isEmpty) {
        List(JsString(transaction.hex), jsonKeys)
      } else {
        val utxos = JsArray(utxoDeps.get.map(Json.toJson(_)))
        if (sigHash.isEmpty) {
          List(JsString(transaction.hex), jsonKeys, utxos)
        } else {
          List(JsString(transaction.hex),
               jsonKeys,
               utxos,
               JsString(sigHash.get))
        }
      }
    }

    bitcoindCall[SignRawTransactionResult]("signrawtransactionwithkey", params)
  }

  def stop: Future[String] = {
    bitcoindCall[String]("stop")
  }

  def submitBlock(block: Block, dummy: Option[JsValue] = None): Future[Unit] = {
    val params =
      if (dummy.isEmpty) {
        List(JsString(block.hex))
      } else {
        List(JsString(block.hex), dummy.get)
      }

    bitcoindCall[Unit]("submitblock", params)
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

  // Uses string:object pattern - GetMemoryInfo, GetMemPoolAncestors, GetMemPoolDescendants, GetNetTotals, GetPeerInfo, GetRawMemPoolWithTransactions, GetTxOut, ImportMulti, ListAccounts, SendMany
  // Also Skipped: GetBlockTemplate
  // TODO: Overload calls with Option inputs?
  // --------------------------------------------------------------------------------
  // EVERYTHING BELOW THIS COMMENT HAS TESTS

  def createRawTransaction(
      inputs: Vector[TransactionInput],
      outputs: Map[String, Bitcoins],
      locktime: Int = 0): Future[Transaction] = {
    bitcoindCall[Transaction](
      "createrawtransaction",
      List(Json.toJson(inputs), Json.toJson(outputs), JsNumber(locktime)))
  }

  def decodeRawTransaction(transaction: Transaction): Future[RpcTransaction] = {
    bitcoindCall[RpcTransaction]("decoderawtransaction",
                                 List(JsString(transaction.hex)))
  }

  def generate(
      blocks: Int,
      maxTries: Int = 1000000): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "generate",
      List(JsNumber(blocks), JsNumber(maxTries)))
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

  def getBlockHash(height: Int): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getblockhash", List(JsNumber(height)))
  }

  def getBlockChainInfo: Future[GetBlockChainInfoResult] = {
    bitcoindCall[GetBlockChainInfoResult]("getblockchaininfo")
  }

  def getBlockCount: Future[Int] = {
    bitcoindCall[Int]("getblockcount")
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

  def getConnectionCount: Future[Int] = {
    bitcoindCall[Int]("getconnectioncount")
  }

  def getDifficulty: Future[BigDecimal] = {
    bitcoindCall[BigDecimal]("getdifficulty")
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

  def getNetworkHashPS(blocks: Int = 120, height: Int = -1): Future[Int] = {
    bitcoindCall[Int]("getnetworkhashps",
                      List(JsNumber(blocks), JsNumber(height)))
  }

  def getNetworkInfo: Future[GetNetworkInfoResult] = {
    bitcoindCall[GetNetworkInfoResult]("getnetworkinfo")
  }

  def getNewAddress(account: String = ""): Future[BitcoinAddress] = {
    bitcoindCall[BitcoinAddress]("getnewaddress", List(JsString(account)))
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

  def getTxOutSetInfo: Future[GetTxOutSetInfoResult] = {
    bitcoindCall[GetTxOutSetInfoResult]("gettxoutsetinfo")
  }

  def getUnconfirmedBalance: Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getunconfirmedbalance")
  }

  def help(rpcName: String = ""): Future[String] = {
    bitcoindCall[String]("help", List(JsString(rpcName)))
  }

  def keyPoolRefill(keyPoolSize: Int = 100): Future[Unit] = {
    bitcoindCall[Unit]("keypoolrefill", List(JsNumber(keyPoolSize)))
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

  def ping: Future[Unit] = {
    bitcoindCall[Unit]("ping")
  }

  def sendRawTransaction(
      transaction: Transaction,
      allowHighFees: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendrawtransaction",
      List(JsString(transaction.hex), JsBoolean(allowHighFees)))
  }

  def setNetworkActive(activate: Boolean): Future[Unit] = {
    bitcoindCall[Unit]("setnetworkactive", List(JsBoolean(activate)))
  }

  def signRawTransactionWithWallet(
      transaction: Transaction,
      utxoDeps: Option[Vector[RpcOpts.SignRawTransactionOutputParameter]] = None,
      sigHash: Option[String] = None): Future[SignRawTransactionResult] = {
    val params =
      if (utxoDeps.isEmpty) {
        List(JsString(transaction.hex))
      } else {
        val utxos = JsArray(utxoDeps.get.map(Json.toJson(_)))
        if (sigHash.isEmpty) {
          List(JsString(transaction.hex), utxos)
        } else {
          List(JsString(transaction.hex), utxos, JsString(sigHash.get))
        }
      }

    bitcoindCall[SignRawTransactionResult]("signrawtransactionwithwallet",
                                           params)
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

  private def bitcoindCall[T](
      command: String,
      parameters: List[JsValue] = List.empty)(
      implicit reader: Reads[T]): Future[T] = {
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
    val m: Map[String, JsValue] = Map("method" -> JsString(methodName),
                                      "params" -> params,
                                      "id" -> JsString("")) // java.util.UUID
    val jsObject = JsObject(m)

    val uri = "http://localhost:18443"
    val username = "nadav"
    val password = "abc123"
    HttpRequest(
      method = HttpMethods.POST,
      uri,
      entity = HttpEntity(ContentTypes.`application/json`, jsObject.toString()))
      .addCredentials(
        HttpCredentials.createBasicHttpCredentials(username, password))
  }
}
