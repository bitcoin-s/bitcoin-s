package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddressType,
  WalletCreateFundedPsbtOptions,
  WalletFlag
}
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.crypto.ECPrivateKeyUtil
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey
}
import org.bitcoins.rpc.client.common.BitcoindVersion._
import play.api.libs.json._

import scala.concurrent.Future

/**
  * RPC calls related to wallet management
  * functionality in bitcoind
  */
trait WalletRpc { self: Client =>

  def backupWallet(
      destination: String,
      walletNameOpt: Option[String] = None): Future[Unit] = {
    bitcoindCall[Unit]("backupwallet",
                       List(JsString(destination)),
                       uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def dumpPrivKey(
      address: BitcoinAddress,
      walletNameOpt: Option[String] = None): Future[ECPrivateKey] = {
    bitcoindCall[String]("dumpprivkey",
                         List(JsString(address.value)),
                         uriExtensionOpt = walletNameOpt.map(walletExtension))
      .map(ECPrivateKeyUtil.fromWIFToPrivateKey)
  }

  def dumpWallet(
      filePath: String,
      walletNameOpt: Option[String] = None): Future[DumpWalletResult] = {
    bitcoindCall[DumpWalletResult]("dumpwallet",
                                   List(JsString(filePath)),
                                   uriExtensionOpt =
                                     walletNameOpt.map(walletExtension))
  }

  def encryptWallet(
      passphrase: String,
      walletNameOpt: Option[String] = None): Future[String] = {
    bitcoindCall[String]("encryptwallet",
                         List(JsString(passphrase)),
                         uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def getBalance: Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getbalance")
  }

  def getBalance(walletName: String): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getbalance",
                           uriExtensionOpt = Some(walletExtension(walletName)))
  }

  def getReceivedByAddress(
      address: BitcoinAddress,
      minConfirmations: Int = 1,
      walletNameOpt: Option[String] = None): Future[Bitcoins] = {
    bitcoindCall[Bitcoins](
      "getreceivedbyaddress",
      List(JsString(address.toString), JsNumber(minConfirmations)),
      uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def getUnconfirmedBalance: Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getunconfirmedbalance")
  }

  def getUnconfirmedBalance(walletName: String): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getunconfirmedbalance",
                           uriExtensionOpt = Some(walletExtension(walletName)))
  }

  def importAddress(
      address: BitcoinAddress,
      account: String = "",
      rescan: Boolean = true,
      p2sh: Boolean = false,
      walletNameOpt: Option[String] = None): Future[Unit] = {
    bitcoindCall[Unit]("importaddress",
                       List(JsString(address.value),
                            JsString(account),
                            JsBoolean(rescan),
                            JsBoolean(p2sh)),
                       uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  private def getNewAddressInternal(
      accountOrLabel: String = "",
      addressType: Option[AddressType],
      walletNameOpt: Option[String] = None): Future[BitcoinAddress] = {
    val params =
      List(JsString(accountOrLabel)) ++ addressType.map(Json.toJson(_)).toList

    bitcoindCall[BitcoinAddress](
      "getnewaddress",
      params,
      uriExtensionOpt = walletNameOpt.map(walletExtension)).map(addr =>
      BitcoinAddress.fromScriptPubKey(addr.scriptPubKey, instance.network))
  }

  def getNewAddress: Future[BitcoinAddress] =
    getNewAddressInternal(addressType = None)

  def getNewAddress(walletNameOpt: Option[String]): Future[BitcoinAddress] =
    getNewAddressInternal(addressType = None, walletNameOpt = walletNameOpt)

  def getNewAddress(addressType: AddressType): Future[BitcoinAddress] =
    getNewAddressInternal(addressType = Some(addressType))

  def getNewAddress(accountOrLabel: String): Future[BitcoinAddress] =
    getNewAddressInternal(accountOrLabel, None)

  def getNewAddress(
      accountOrLabel: String,
      addressType: AddressType): Future[BitcoinAddress] =
    getNewAddressInternal(accountOrLabel, Some(addressType))

  def getNewAddress(
      accountOrLabel: String,
      addressType: AddressType,
      walletName: String): Future[BitcoinAddress] =
    getNewAddressInternal(accountOrLabel, Some(addressType), Some(walletName))

  private def getRawChangeAddressInternal(
      addressType: Option[AddressType],
      walletNameOpt: Option[String] = None): Future[BitcoinAddress] = {
    bitcoindCall[BitcoinAddress]("getrawchangeaddress",
                                 addressType.map(Json.toJson(_)).toList,
                                 uriExtensionOpt =
                                   walletNameOpt.map(walletExtension))
  }

  def getRawChangeAddress: Future[BitcoinAddress] =
    getRawChangeAddressInternal(None)

  def getRawChangeAddress(addressType: AddressType): Future[BitcoinAddress] =
    getRawChangeAddressInternal(Some(addressType))

  def getRawChangeAddress(walletName: String): Future[BitcoinAddress] =
    getRawChangeAddressInternal(None, Some(walletName))

  def getRawChangeAddress(
      addressType: AddressType,
      walletName: String): Future[BitcoinAddress] =
    getRawChangeAddressInternal(Some(addressType), Some(walletName))

  def getWalletInfo: Future[GetWalletInfoResult] = {
    bitcoindCall[GetWalletInfoResult]("getwalletinfo")
  }

  def getWalletInfo(walletName: String): Future[GetWalletInfoResult] = {
    bitcoindCall[GetWalletInfoResult]("getwalletinfo",
                                      uriExtensionOpt =
                                        Some(walletExtension(walletName)))
  }

  def keyPoolRefill(
      keyPoolSize: Int = 100,
      walletNameOpt: Option[String] = None): Future[Unit] = {
    bitcoindCall[Unit]("keypoolrefill",
                       List(JsNumber(keyPoolSize)),
                       uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def importPubKey(
      pubKey: ECPublicKey,
      label: String = "",
      rescan: Boolean = true,
      walletNameOpt: Option[String] = None): Future[Unit] = {
    bitcoindCall[Unit](
      "importpubkey",
      List(JsString(pubKey.hex), JsString(label), JsBoolean(rescan)),
      uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def importPrivKey(
      key: ECPrivateKey,
      account: String = "",
      rescan: Boolean = true,
      walletNameOpt: Option[String] = None): Future[Unit] = {
    bitcoindCall[Unit](
      "importprivkey",
      List(JsString(ECPrivateKeyUtil.toWIF(key, network)),
           JsString(account),
           JsBoolean(rescan)),
      uriExtensionOpt = walletNameOpt.map(walletExtension)
    )
  }

  def importMulti(
      requests: Vector[RpcOpts.ImportMultiRequest],
      rescan: Boolean = true,
      walletNameOpt: Option[String] = None): Future[
    Vector[ImportMultiResult]] = {
    bitcoindCall[Vector[ImportMultiResult]](
      "importmulti",
      List(Json.toJson(requests), JsObject(Map("rescan" -> JsBoolean(rescan)))),
      uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def importPrunedFunds(
      transaction: Transaction,
      txOutProof: MerkleBlock,
      walletNameOpt: Option[String] = None): Future[Unit] = {
    bitcoindCall[Unit](
      "importprunedfunds",
      List(JsString(transaction.hex), JsString(txOutProof.hex)),
      uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def removePrunedFunds(
      txid: DoubleSha256DigestBE,
      walletNameOpt: Option[String]): Future[Unit] = {
    bitcoindCall[Unit]("removeprunedfunds",
                       List(JsString(txid.hex)),
                       uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def removePrunedFunds(txid: DoubleSha256DigestBE): Future[Unit] = {
    removePrunedFunds(txid, None)
  }

  def removePrunedFunds(txid: DoubleSha256Digest): Future[Unit] = {
    removePrunedFunds(txid.flip, None)
  }

  def removePrunedFunds(
      txid: DoubleSha256Digest,
      walletNameOpt: Option[String]): Future[Unit] = {
    removePrunedFunds(txid.flip, walletNameOpt)
  }

  def importWallet(
      filePath: String,
      walletNameOpt: Option[String] = None): Future[Unit] = {
    bitcoindCall[Unit]("importwallet",
                       List(JsString(filePath)),
                       uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def listAddressGroupings: Future[Vector[Vector[RpcAddress]]] = {
    bitcoindCall[Vector[Vector[RpcAddress]]]("listaddressgroupings")
  }

  def listAddressGroupings(
      walletName: String): Future[Vector[Vector[RpcAddress]]] = {
    bitcoindCall[Vector[Vector[RpcAddress]]](
      "listaddressgroupings",
      uriExtensionOpt = Some(walletExtension(walletName)))
  }

  def listReceivedByAddress(
      confirmations: Int = 1,
      includeEmpty: Boolean = false,
      includeWatchOnly: Boolean = false,
      walletNameOpt: Option[String] = None): Future[Vector[ReceivedAddress]] = {
    bitcoindCall[Vector[ReceivedAddress]](
      "listreceivedbyaddress",
      List(JsNumber(confirmations),
           JsBoolean(includeEmpty),
           JsBoolean(includeWatchOnly)),
      uriExtensionOpt = walletNameOpt.map(walletExtension)
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

  /**
    * Change the state of the given wallet flag for a wallet.
    */
  def setWalletFlag(
      flag: WalletFlag,
      value: Boolean,
      walletNameOpt: Option[String] = None
  ): Future[SetWalletFlagResult] = {

    self.version match {
      case V20 | V19 | Experimental | Unknown =>
        bitcoindCall[SetWalletFlagResult](
          "setwalletflag",
          List(JsString(flag.toString), Json.toJson(value)),
          uriExtensionOpt = walletNameOpt.map(walletExtension))
      case V16 | V17 | V18 =>
        Future.failed(
          new UnsupportedOperationException(
            "setwalletflag is not available for versions before 0.19"))
    }
  }

  def getBalances: Future[GetBalancesResult] = {
    self.version match {
      case V20 | V19 | Experimental | Unknown =>
        bitcoindCall[GetBalancesResult]("getbalances")
      case V16 | V17 | V18 =>
        Future.failed(
          new UnsupportedOperationException(
            "getbalances is not available for versions before 0.19"))
    }
  }

  def getBalances(walletName: String): Future[GetBalancesResult] = {
    self.version match {
      case V20 | V19 | Experimental | Unknown =>
        bitcoindCall[GetBalancesResult]("getbalances",
                                        uriExtensionOpt =
                                          Some(walletExtension(walletName)))
      case V16 | V17 | V18 =>
        Future.failed(
          new UnsupportedOperationException(
            "getbalances is not available for versions before 0.19"))
    }
  }

  // TODO: Should be BitcoinFeeUnit
  def setTxFee(
      feePerKB: Bitcoins,
      walletNameOpt: Option[String] = None): Future[Boolean] = {
    bitcoindCall[Boolean]("settxfee",
                          List(JsNumber(feePerKB.toBigDecimal)),
                          uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def walletLock(): Future[Unit] = {
    bitcoindCall[Unit]("walletlock")
  }

  def walletLock(walletName: String): Future[Unit] = {
    bitcoindCall[Unit]("walletlock",
                       uriExtensionOpt = Some(walletExtension(walletName)))
  }

  def walletPassphrase(
      passphrase: String,
      seconds: Int,
      walletNameOpt: Option[String] = None): Future[Unit] = {
    bitcoindCall[Unit]("walletpassphrase",
                       List(JsString(passphrase), JsNumber(seconds)),
                       uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def walletPassphraseChange(
      currentPassphrase: String,
      newPassphrase: String,
      walletNameOpt: Option[String] = None): Future[Unit] = {
    bitcoindCall[Unit](
      "walletpassphrasechange",
      List(JsString(currentPassphrase), JsString(newPassphrase)),
      uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def signRawTransactionWithWallet(
      transaction: Transaction,
      walletNameOpt: Option[String]): Future[
    SignRawTransactionWithWalletResult] = {
    bitcoindCall[SignRawTransactionWithWalletResult](
      "signrawtransactionwithwallet",
      List(JsString(transaction.hex)),
      uriExtensionOpt = walletNameOpt.map(walletExtension))
  }

  def signRawTransactionWithWallet(
      transaction: Transaction): Future[SignRawTransactionWithWalletResult] = {
    signRawTransactionWithWallet(transaction, None)
  }

  /**
    * @param blank Not available to versions before v19
    * @param passphrase Not available to versions before v19
    * @return
    */
  def createWallet(
      walletName: String,
      disablePrivateKeys: Boolean = false,
      blank: Boolean = false,
      passphrase: String = ""): Future[CreateWalletResult] =
    self.version match {
      case V20 | V19 | Experimental | Unknown =>
        bitcoindCall[CreateWalletResult]("createwallet",
                                         List(JsString(walletName),
                                              JsBoolean(disablePrivateKeys),
                                              JsBoolean(blank),
                                              JsString(passphrase)))
      case V16 | V17 | V18 =>
        require(passphrase.isEmpty,
                "passphrase should not be set for versions before v19")
        bitcoindCall[CreateWalletResult](
          "createwallet",
          List(JsString(walletName), JsBoolean(disablePrivateKeys)))
    }

  def getAddressInfo(
      address: BitcoinAddress,
      walletNameOpt: Option[String] = None): Future[AddressInfoResult] = {
    self.version match {
      case V16 | V17 =>
        bitcoindCall[AddressInfoResultPreV18](
          "getaddressinfo",
          List(JsString(address.value)),
          uriExtensionOpt = walletNameOpt.map(walletExtension))
      case V18 | V19 | V20 | Experimental | Unknown =>
        bitcoindCall[AddressInfoResultPostV18](
          "getaddressinfo",
          List(JsString(address.value)),
          uriExtensionOpt = walletNameOpt.map(walletExtension))
    }
  }

  def sendMany(
      amounts: Map[BitcoinAddress, CurrencyUnit],
      minconf: Int = 1,
      comment: String = "",
      subtractFeeFrom: Vector[BitcoinAddress] = Vector.empty,
      walletNameOpt: Option[String] = None): Future[DoubleSha256DigestBE] = {
    val jsonOutputs: JsValue = Json.toJson {
      amounts.map {
        case (addr, curr) => addr -> Bitcoins(curr.satoshis)
      }
    }
    bitcoindCall[DoubleSha256DigestBE](
      "sendmany",
      List(JsString(""),
           jsonOutputs,
           JsNumber(minconf),
           JsString(comment),
           Json.toJson(subtractFeeFrom)),
      uriExtensionOpt = walletNameOpt.map(walletExtension)
    )
  }

  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      localComment: String = "",
      toComment: String = "",
      subractFeeFromAmount: Boolean = false,
      walletNameOpt: Option[String] = None): Future[DoubleSha256DigestBE] = {
    bitcoindCall[DoubleSha256DigestBE](
      "sendtoaddress",
      List(Json.toJson(address),
           Json.toJson(Bitcoins(amount.satoshis)),
           JsString(localComment),
           JsString(toComment),
           JsBoolean(subractFeeFromAmount)),
      uriExtensionOpt = walletNameOpt.map(walletExtension)
    )
  }

  def walletProcessPSBT(
      psbt: PSBT,
      sign: Boolean = true,
      sigHashType: HashType = HashType.sigHashAll,
      walletNameOpt: Option[String] = None): Future[WalletProcessPsbtResult] = {
    bitcoindCall[WalletProcessPsbtResult](
      "walletprocesspsbt",
      List(JsString(psbt.base64), JsBoolean(sign), Json.toJson(sigHashType)),
      uriExtensionOpt = walletNameOpt.map(walletExtension)
    )
  }

  def walletCreateFundedPsbt(
      inputs: Vector[TransactionInput],
      outputs: Map[BitcoinAddress, CurrencyUnit],
      locktime: Int = 0,
      options: WalletCreateFundedPsbtOptions = WalletCreateFundedPsbtOptions(),
      bip32derivs: Boolean = false,
      walletNameOpt: Option[String] = None
  ): Future[WalletCreateFundedPsbtResult] = {
    val jsonOutputs =
      Json.toJson {
        outputs.map { case (addr, curr) => addr -> Bitcoins(curr.satoshis) }
      }
    bitcoindCall[WalletCreateFundedPsbtResult](
      "walletcreatefundedpsbt",
      List(Json.toJson(inputs),
           jsonOutputs,
           JsNumber(locktime),
           Json.toJson(options),
           Json.toJson(bip32derivs)),
      uriExtensionOpt = walletNameOpt.map(walletExtension)
    )
  }
}
