package org.bitcoins.rpc.client


import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.rpc.client.RpcOpts.AddressType
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.sys.process._


class BitcoindRpcClient(instance: BitcoindInstance)(
  implicit
  m: ActorMaterializer) extends Client
  with BitcoindCall
  with BlockchainRpc
  with P2PRpc
  with WalletRpc
  with MempoolRpc
  with MiningRpc
  with NodeRpc {
  override protected implicit val executor: ExecutionContext = m.executionContext

  def getDaemon: BitcoindInstance = instance

  def abandonTransaction(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("abandontransaction", List(JsString(txid.hex)))
  }


  def bumpFee(
    txid: DoubleSha256Digest,
    confTarget: Int = 6,
    totalFee: Option[Satoshis] = None,
    replaceable: Boolean = true,
    estimateMode: String = "UNSET"): Future[BumpFeeResult] = {
    val options =
      if (totalFee.isEmpty) {
        List(
          ("confTarget", JsNumber(confTarget)),
          ("replaceable", JsBoolean(replaceable)),
          ("estimate_mode", JsString(estimateMode)))
      } else {
        List(
          ("confTarget", JsNumber(confTarget)),
          ("totalFee", JsNumber(totalFee.get.toBigDecimal)),
          ("replaceable", JsBoolean(replaceable)),
          ("estimate_mode", JsString(estimateMode)))
      }

    bitcoindCall[BumpFeeResult](
      "bumpfee",
      List(JsString(txid.hex), JsObject(options)))
  }


  def combineRawTransaction(txs: Vector[Transaction]): Future[Transaction] = {
    bitcoindCall[Transaction]("combinerawtransaction", List(Json.toJson(txs)))
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
    bitcoindCall[RpcTransaction](
      "decoderawtransaction",
      List(JsString(transaction.hex)))
  }

  // TODO: add ScriptPubKey.asmHex
  def decodeScript(script: ScriptPubKey): Future[DecodeScriptResult] = {
    bitcoindCall[DecodeScriptResult](
      "decodescript",
      List(JsString(BitcoinSUtil.encodeHex(script.asmBytes))))
  }


  def dumpPrivKey(address: BitcoinAddress): Future[ECPrivateKey] = {
    bitcoindCall[String]("dumpprivkey", List(JsString(address.value)))
      .map(ECPrivateKey.fromWIFToPrivateKey)
  }

  // Needs manual testing!
  def estimateSmartFee(
    blocks: Int,
    mode: String = "CONSERVATIVE"): Future[EstimateSmartFeeResult] = {
    bitcoindCall[EstimateSmartFeeResult](
      "estimatesmartfee",
      List(JsNumber(blocks), JsString(mode)))
  }

  private def fundRawTransaction(
    transaction: Transaction,
    options: Option[RpcOpts.FundRawTransactionOptions]): Future[FundRawTransactionResult] = {
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
    options: RpcOpts.FundRawTransactionOptions): Future[FundRawTransactionResult] = fundRawTransaction(transaction, Some(options))


  def getNetTotals: Future[GetNetTotalsResult] = {
    bitcoindCall[GetNetTotalsResult]("getnettotals")
  }

  def getNetworkHashPS(
    blocks: Int = 120,
    height: Int = -1): Future[BigDecimal] = {
    bitcoindCall[BigDecimal](
      "getnetworkhashps",
      List(JsNumber(blocks), JsNumber(height)))
  }


  private def getNewAddress(
    account: String = "",
    addressType: Option[AddressType]): Future[BitcoinAddress] = {
    val params =
      if (addressType.isEmpty) {
        List(JsString(account))
      } else {
        List(
          JsString(account),
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


  def getRawTransaction(
    txid: DoubleSha256Digest): Future[GetRawTransactionResult] = {
    bitcoindCall[GetRawTransactionResult](
      "getrawtransaction",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getRawTransactionRaw(txid: DoubleSha256Digest): Future[Transaction] = {
    bitcoindCall[Transaction](
      "getrawtransaction",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def getReceivedByAccount(
    account: String,
    confirmations: Int = 1): Future[Bitcoins] = {
    bitcoindCall[Bitcoins](
      "getreceivedbyaccount",
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

  def importAddress(
    address: BitcoinAddress,
    account: String = "",
    rescan: Boolean = true,
    p2sh: Boolean = false): Future[Unit] = {
    bitcoindCall[Unit](
      "importaddress",
      List(
        JsString(address.value),
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


  def listLockUnspent: Future[Vector[TransactionOutPoint]] = {
    bitcoindCall[Vector[TransactionOutPoint]]("listlockunspent")
  }

  def listReceivedByAccount(
    confirmations: Int = 1,
    includeEmpty: Boolean = false,
    includeWatchOnly: Boolean = false): Future[Vector[ReceivedAccount]] = {
    bitcoindCall[Vector[ReceivedAccount]](
      "listreceivedbyaccount",
      List(
        JsNumber(confirmations),
        JsBoolean(includeEmpty),
        JsBoolean(includeWatchOnly)))
  }

  def listReceivedByAddress(
    confirmations: Int = 1,
    includeEmpty: Boolean = false,
    includeWatchOnly: Boolean = false): Future[Vector[ReceivedAddress]] = {
    bitcoindCall[Vector[ReceivedAddress]](
      "listreceivedbyaddress",
      List(
        JsNumber(confirmations),
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
        List(
          JsString(headerHash.get.hex),
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
    includeWatchOnly: Boolean = false): Future[Vector[ListTransactionsResult]] = {
    bitcoindCall[Vector[ListTransactionsResult]](
      "listtransactions",
      List(
        JsString(account),
        JsNumber(count),
        JsNumber(skip),
        JsBoolean(includeWatchOnly)))
  }

  private def listUnspent(
    minConfirmations: Int = 1,
    maxConfirmations: Int = 9999999,
    addresses: Option[Vector[BitcoinAddress]]): Future[Vector[UnspentOutput]] = {
    val params =
      if (addresses.isEmpty) {
        List(JsNumber(minConfirmations), JsNumber(maxConfirmations))
      } else {
        List(
          JsNumber(minConfirmations),
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


  def lockUnspent(
    unlock: Boolean,
    outputs: Vector[RpcOpts.LockUnspentOutputParameter]): Future[Boolean] = {
    bitcoindCall[Boolean](
      "lockunspent",
      List(JsBoolean(unlock), Json.toJson(outputs)))
  }

  def move(
    fromAccount: String,
    toAccount: String,
    amount: Bitcoins,
    comment: String = ""): Future[Boolean] = {
    bitcoindCall[Boolean](
      "move",
      List(
        JsString(fromAccount),
        JsString(toAccount),
        JsNumber(amount.toBigDecimal),
        JsNumber(6),
        JsString(comment)))
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


  def sendFrom(
    fromAccount: String,
    toAddress: BitcoinAddress,
    amount: Bitcoins,
    confirmations: Int = 1,
    comment: String = "",
    toComment: String = ""): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendfrom",
      List(
        JsString(fromAccount),
        JsString(toAddress.value),
        JsNumber(amount.toBigDecimal),
        JsNumber(confirmations),
        JsString(comment),
        JsString(toComment)))
  }

  def sendMany(
    amounts: Map[BitcoinAddress, Bitcoins],
    minconf: Int = 1,
    comment: String = "",
    subtractFeeFrom: Vector[BitcoinAddress] = Vector.empty): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendmany",
      List(
        JsString(""),
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
      List(
        JsString(address.toString),
        JsNumber(amount.toBigDecimal),
        JsString(localComment),
        JsString(toComment),
        JsBoolean(subractFeeFromAmount)))
  }

  def sendRawTransaction(
    transaction: Transaction,
    allowHighFees: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendrawtransaction",
      List(JsString(transaction.hex), JsBoolean(allowHighFees)))
  }

  def setAccount(address: BitcoinAddress, account: String): Future[Unit] = {
    bitcoindCall[Unit](
      "setaccount",
      List(JsString(address.value), JsString(account)))
  }



  // TODO: Should be BitcoinFeeUnit
  def setTxFee(feePerKB: Bitcoins): Future[Boolean] = {
    bitcoindCall[Boolean]("settxfee", List(JsNumber(feePerKB.toBigDecimal)))
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
    utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter]): Future[SignRawTransactionResult] =
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


  def submitBlock(block: Block): Future[Unit] = {
    bitcoindCall[Unit]("submitblock", List(JsString(block.hex)))
  }


  def validateAddress(
    address: BitcoinAddress): Future[ValidateAddressResult] = {
    bitcoindCall[ValidateAddressResult](
      "validateaddress",
      List(JsString(address.toString)))
  }

  def verifyChain(level: Int = 3, blocks: Int = 6): Future[Boolean] = {
    bitcoindCall[Boolean](
      "verifychain",
      List(JsNumber(level), JsNumber(blocks)))
  }


  def verifyTxOutProof(
    proof: MerkleBlock): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "verifytxoutproof",
      List(JsString(proof.hex)))
  }

  def start(): String = {
    val cmd = Seq(
      "bitcoind",
      "-datadir=" + instance.authCredentials.datadir,
      "-rpcport=" + instance.rpcUri.getPort,
      "-port=" + instance.uri.getPort)
    cmd.!!
  }
}
