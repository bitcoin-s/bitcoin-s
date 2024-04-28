package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddressType,
  WalletCreateFundedPsbtOptions,
  WalletFlag
}
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.crypto._
import play.api.libs.json._

import scala.concurrent.Future

/** RPC calls related to wallet management functionality in bitcoind
  */
trait WalletRpc { self: Client =>
  private val DEFAULT_WALLET = BitcoindRpcClient.DEFAULT_WALLET_NAME
  def backupWallet(
      destination: String,
      walletName: String = DEFAULT_WALLET
  ): Future[Unit] = {
    bitcoindCall[Unit](
      "backupwallet",
      List(JsString(destination)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def encryptWallet(
      passphrase: String,
      walletName: String = DEFAULT_WALLET
  ): Future[String] = {
    bitcoindCall[String](
      "encryptwallet",
      List(JsString(passphrase)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def getBalance: Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getbalance")
  }

  def getBalance(walletName: String): Future[Bitcoins] = {
    bitcoindCall[Bitcoins](
      "getbalance",
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def getReceivedByAddress(
      address: BitcoinAddress,
      minConfirmations: Int = 1,
      walletName: String = DEFAULT_WALLET
  ): Future[Bitcoins] = {
    bitcoindCall[Bitcoins](
      "getreceivedbyaddress",
      List(JsString(address.toString), JsNumber(minConfirmations)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def getUnconfirmedBalance: Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getunconfirmedbalance")
  }

  def getUnconfirmedBalance(walletName: String): Future[Bitcoins] = {
    bitcoindCall[Bitcoins](
      "getunconfirmedbalance",
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  private def getNewAddressInternal(
      accountOrLabel: String = "",
      addressType: Option[AddressType],
      walletName: String = DEFAULT_WALLET
  ): Future[BitcoinAddress] = {
    val params =
      List(JsString(accountOrLabel)) ++ addressType.map(Json.toJson(_)).toList

    bitcoindCall[BitcoinAddress](
      "getnewaddress",
      params,
      uriExtensionOpt = Some(walletExtension(walletName))
    ).map(addr =>
      BitcoinAddress.fromScriptPubKey(addr.scriptPubKey, instance.network))
  }

  def getNewAddress: Future[BitcoinAddress] =
    getNewAddressInternal(addressType = None)

  def getNewAddress(
      walletName: String,
      label: String = ""): Future[BitcoinAddress] =
    getNewAddressInternal(accountOrLabel = label,
                          addressType = None,
                          walletName = walletName)

  def getNewAddress(
      addressType: AddressType,
      walletName: String): Future[BitcoinAddress] =
    getNewAddressInternal(addressType = Some(addressType),
                          walletName = walletName)

  def getNewAddress(addressType: AddressType): Future[BitcoinAddress] =
    getNewAddressInternal(addressType = Some(addressType))

  // how to di
//  def getNewAddress(accountOrLabel: String): Future[BitcoinAddress] =
//    getNewAddressInternal(accountOrLabel, None)

  def getNewAddress(
      accountOrLabel: String,
      addressType: AddressType
  ): Future[BitcoinAddress] =
    getNewAddressInternal(accountOrLabel, Some(addressType))

  def getNewAddress(
      accountOrLabel: String,
      addressType: AddressType,
      walletName: String
  ): Future[BitcoinAddress] =
    getNewAddressInternal(accountOrLabel, Some(addressType), walletName)

  private def getRawChangeAddressInternal(
      addressType: Option[AddressType],
      walletName: String = DEFAULT_WALLET
  ): Future[BitcoinAddress] = {
    bitcoindCall[BitcoinAddress](
      "getrawchangeaddress",
      addressType.map(Json.toJson(_)).toList,
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def getRawChangeAddress: Future[BitcoinAddress] =
    getRawChangeAddressInternal(None)

  def getRawChangeAddress(addressType: AddressType): Future[BitcoinAddress] =
    getRawChangeAddressInternal(Some(addressType))

  def getRawChangeAddress(walletName: String): Future[BitcoinAddress] =
    getRawChangeAddressInternal(None, walletName)

  def getRawChangeAddress(
      addressType: AddressType,
      walletName: String
  ): Future[BitcoinAddress] =
    getRawChangeAddressInternal(Some(addressType), walletName)

  def getWalletInfo(
      walletName: String
  ): Future[GetWalletInfoResult] = {
    bitcoindCall[GetWalletInfoResultPostV22](
      "getwalletinfo",
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def getWalletInfo: Future[GetWalletInfoResult] = {
    getWalletInfo(DEFAULT_WALLET)
  }

  /** @return
    */
  def keyPoolRefill(
      keyPoolSize: Int = 100,
      walletName: String = DEFAULT_WALLET
  ): Future[Unit] = {
    bitcoindCall[Unit](
      "keypoolrefill",
      List(JsNumber(keyPoolSize)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def importMulti(
      requests: Vector[RpcOpts.ImportMultiRequest],
      rescan: Boolean = true,
      walletName: String = DEFAULT_WALLET
  ): Future[Vector[ImportMultiResult]] = {
    bitcoindCall[Vector[ImportMultiResult]](
      "importmulti",
      List(Json.toJson(requests), JsObject(Map("rescan" -> JsBoolean(rescan)))),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def importPrunedFunds(
      transaction: Transaction,
      txOutProof: MerkleBlock,
      walletName: String = DEFAULT_WALLET
  ): Future[Unit] = {
    bitcoindCall[Unit](
      "importprunedfunds",
      List(JsString(transaction.hex), JsString(txOutProof.hex)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def removePrunedFunds(
      txid: DoubleSha256DigestBE,
      walletName: String
  ): Future[Unit] = {
    bitcoindCall[Unit](
      "removeprunedfunds",
      List(JsString(txid.hex)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def removePrunedFunds(txid: DoubleSha256DigestBE): Future[Unit] = {
    removePrunedFunds(txid, DEFAULT_WALLET)
  }

  def removePrunedFunds(txid: DoubleSha256Digest): Future[Unit] = {
    removePrunedFunds(txid.flip, DEFAULT_WALLET)
  }

  def removePrunedFunds(
      txid: DoubleSha256Digest,
      walletName: String
  ): Future[Unit] = {
    removePrunedFunds(txid.flip, walletName)
  }

  def listAddressGroupings: Future[Vector[Vector[RpcAddress]]] = {
    bitcoindCall[Vector[Vector[RpcAddress]]]("listaddressgroupings")
  }

  def listAddressGroupings(
      walletName: String
  ): Future[Vector[Vector[RpcAddress]]] = {
    bitcoindCall[Vector[Vector[RpcAddress]]](
      "listaddressgroupings",
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def listReceivedByAddress(
      confirmations: Int = 1,
      includeEmpty: Boolean = false,
      includeWatchOnly: Boolean = false,
      walletName: String = DEFAULT_WALLET
  ): Future[Vector[ReceivedAddress]] = {
    bitcoindCall[Vector[ReceivedAddress]](
      "listreceivedbyaddress",
      List(
        JsNumber(confirmations),
        JsBoolean(includeEmpty),
        JsBoolean(includeWatchOnly)
      ),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def listWallets: Future[Vector[String]] = {
    bitcoindCall[Vector[String]]("listwallets")
  }

  def unloadWallet(filePath: String): Future[Unit] = {
    bitcoindCall[Unit]("unloadwallet", List(JsString(filePath)))
  }

  def loadWallet(filePath: String): Future[LoadWalletResult] = {
    bitcoindCall[LoadWalletResult]("loadwallet", List(JsString(filePath)))
  }

  /** Change the state of the given wallet flag for a wallet.
    */
  def setWalletFlag(
      flag: WalletFlag,
      value: Boolean,
      walletName: String = DEFAULT_WALLET
  ): Future[SetWalletFlagResult] = {
    bitcoindCall[SetWalletFlagResult](
      "setwalletflag",
      List(JsString(flag.toString), Json.toJson(value)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def getBalances: Future[GetBalancesResult] = {
    getBalances(BitcoindRpcClient.DEFAULT_WALLET_NAME)
  }

  def getBalances(walletName: String): Future[GetBalancesResult] = {
    bitcoindCall[GetBalancesResult](
      "getbalances",
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  // TODO: Should be BitcoinFeeUnit
  def setTxFee(
      feePerKB: Bitcoins,
      walletName: String = DEFAULT_WALLET
  ): Future[Boolean] = {
    bitcoindCall[Boolean](
      "settxfee",
      List(JsNumber(feePerKB.toBigDecimal)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def walletLock(): Future[Unit] = {
    bitcoindCall[Unit]("walletlock")
  }

  def walletLock(walletName: String): Future[Unit] = {
    bitcoindCall[Unit](
      "walletlock",
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def walletPassphrase(
      passphrase: String,
      seconds: Int,
      walletName: String = DEFAULT_WALLET
  ): Future[Unit] = {
    bitcoindCall[Unit](
      "walletpassphrase",
      List(JsString(passphrase), JsNumber(seconds)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def walletPassphraseChange(
      currentPassphrase: String,
      newPassphrase: String,
      walletName: String = DEFAULT_WALLET
  ): Future[Unit] = {
    bitcoindCall[Unit](
      "walletpassphrasechange",
      List(JsString(currentPassphrase), JsString(newPassphrase)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def signRawTransactionWithWallet(
      transaction: Transaction,
      walletName: String
  ): Future[SignRawTransactionWithWalletResult] = {
    bitcoindCall[SignRawTransactionWithWalletResult](
      "signrawtransactionwithwallet",
      List(JsString(transaction.hex)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def signRawTransactionWithWallet(
      transaction: Transaction
  ): Future[SignRawTransactionWithWalletResult] = {
    signRawTransactionWithWallet(transaction, DEFAULT_WALLET)
  }

  /** @param blank
    *   Not available to versions before v19
    * @param passphrase
    *   Not available to versions before v19
    * @return
    */
  def createWallet(
      walletName: String,
      disablePrivateKeys: Boolean = false,
      blank: Boolean = false,
      passphrase: String = "",
      avoidReuse: Boolean = false,
      descriptors: Boolean = true
  ): Future[CreateWalletResult] = {
    bitcoindCall[CreateWalletResult](
      "createwallet",
      List(
        JsString(walletName),
        JsBoolean(disablePrivateKeys),
        JsBoolean(blank),
        if (passphrase.isEmpty) JsNull else JsString(passphrase),
        JsBoolean(avoidReuse),
        JsBoolean(descriptors)
      )
    )
  }

  def getAddressInfo(
      address: BitcoinAddress,
      walletName: String = DEFAULT_WALLET
  ): Future[AddressInfoResult] = {
    bitcoindCall[AddressInfoResultPostV21](
      "getaddressinfo",
      List(JsString(address.value)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def sendMany(
      amounts: Map[BitcoinAddress, CurrencyUnit],
      minconf: Int = 1,
      comment: String = "",
      subtractFeeFrom: Vector[BitcoinAddress] = Vector.empty,
      walletName: String = DEFAULT_WALLET
  ): Future[DoubleSha256DigestBE] = {
    val jsonOutputs: JsValue = Json.toJson {
      amounts.map { case (addr, curr) =>
        addr -> Bitcoins(curr.satoshis)
      }
    }
    bitcoindCall[DoubleSha256DigestBE](
      "sendmany",
      List(
        JsString(""),
        jsonOutputs,
        JsNumber(minconf),
        JsString(comment),
        Json.toJson(subtractFeeFrom)
      ),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      localComment: String = "",
      toComment: String = "",
      subractFeeFromAmount: Boolean = false,
      walletName: String = DEFAULT_WALLET
  ): Future[DoubleSha256DigestBE] = {
    bitcoindCall[DoubleSha256DigestBE](
      "sendtoaddress",
      List(
        Json.toJson(address),
        Json.toJson(Bitcoins(amount.satoshis)),
        JsString(localComment),
        JsString(toComment),
        JsBoolean(subractFeeFromAmount)
      ),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def walletProcessPSBT(
      psbt: PSBT,
      sign: Boolean = true,
      sigHashType: HashType = HashType.sigHashAll,
      walletName: String = DEFAULT_WALLET
  ): Future[WalletProcessPsbtResult] = {
    bitcoindCall[WalletProcessPsbtResult](
      "walletprocesspsbt",
      List(JsString(psbt.base64), JsBoolean(sign), Json.toJson(sigHashType)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def walletCreateFundedPsbt(
      inputs: Vector[TransactionInput],
      outputs: Map[BitcoinAddress, CurrencyUnit],
      locktime: Int = 0,
      options: WalletCreateFundedPsbtOptions = WalletCreateFundedPsbtOptions(),
      bip32derivs: Boolean = false,
      walletName: String = DEFAULT_WALLET
  ): Future[WalletCreateFundedPsbtResult] = {
    val jsonOutputs =
      Json.toJson {
        outputs.map { case (addr, curr) => addr -> Bitcoins(curr.satoshis) }
      }
    bitcoindCall[WalletCreateFundedPsbtResult](
      "walletcreatefundedpsbt",
      List(
        Json.toJson(inputs),
        jsonOutputs,
        JsNumber(locktime),
        Json.toJson(options),
        Json.toJson(bip32derivs)
      ),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  /** $signRawTx
    *
    * This RPC call signs the raw transaction with keys found in the Bitcoin
    * Core wallet.
    */
  def signRawTransactionWithWallet(
      transaction: Transaction,
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter],
      sigHash: HashType = HashType.sigHashAll
  ): Future[SignRawTransactionResult] =
    bitcoindCall[SignRawTransactionResult](
      "signrawtransactionwithwallet",
      List(
        JsString(transaction.hex),
        Json.toJson(utxoDeps),
        Json.toJson(sigHash)
      )
    )

  /** $signRawTx
    *
    * This RPC call signs the raw transaction with keys provided manually.
    */
  def signRawTransactionWithKey(
      transaction: Transaction,
      keys: Vector[ECPrivateKey],
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter] =
        Vector.empty,
      sigHash: HashType = HashType.sigHashAll
  ): Future[SignRawTransactionResult] =
    bitcoindCall[SignRawTransactionResult](
      "signrawtransactionwithkey",
      List(
        JsString(transaction.hex),
        Json.toJson(keys),
        Json.toJson(utxoDeps),
        Json.toJson(sigHash)
      )
    )

  def simulateRawTransaction(
      tx: Transaction,
      includeWatchOnly: Boolean = true,
      walletName: String = BitcoindRpcClient.DEFAULT_WALLET_NAME
  ): Future[CurrencyUnit] = {
    simulateRawTransactions(Vector(tx), includeWatchOnly, walletName)
  }

  def simulateRawTransactions(
      txs: Vector[Transaction],
      includeWatchOnly: Boolean = true,
      walletName: String = BitcoindRpcClient.DEFAULT_WALLET_NAME
  ): Future[CurrencyUnit] = {
    val txsJson = JsArray(txs.map(tx => JsString(tx.hex)))
    val options = Json.obj("include_watchonly" -> includeWatchOnly)

    bitcoindCall[SimulateRawTransactionResult](
      "simulaterawtransaction",
      List(txsJson, options),
      uriExtensionOpt = Some(walletExtension(walletName))
    ).map(_.balance_change)
  }
}
