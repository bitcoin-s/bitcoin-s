package org.bitcoins.rpc.client

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.jsonmodels.{DumpWalletResult, GetWalletInfoResult}
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsNumber, JsString}

import scala.concurrent.Future

/**
  * RPC calls related to wallet functionality in bitcoind
  */
protected trait WalletRpc extends Client {

  def backupWallet(destination: String): Future[Unit] = {
    bitcoindCall[Unit]("backupwallet", List(JsString(destination)))
  }

  def dumpWallet(filePath: String): Future[DumpWalletResult] = {
    bitcoindCall[DumpWalletResult]("dumpwallet", List(JsString(filePath)))
  }

  def encryptWallet(passphrase: String): Future[String] = {
    bitcoindCall[String]("encryptwallet", List(JsString(passphrase)))
  }

  def getAccount(address: BitcoinAddress): Future[String] = {
    bitcoindCall[String]("getaccount", List(JsString(address.value)))
  }

  def getAccountAddress(account: String): Future[BitcoinAddress] = {
    bitcoindCall[BitcoinAddress]("getaccountaddress", List(JsString(account)))
  }

  def getAddressesByAccount(account: String): Future[Vector[BitcoinAddress]] = {
    bitcoindCall[Vector[BitcoinAddress]](
      "getaddressesbyaccount",
      List(JsString(account)))
  }

  def getBalance: Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getbalance")
  }

  def getWalletInfo: Future[GetWalletInfoResult] = {
    bitcoindCall[GetWalletInfoResult]("getwalletinfo")
  }

  def importWallet(filePath: String): Future[Unit] = {
    bitcoindCall[Unit]("importwallet", List(JsString(filePath)))
  }

  def listWallets: Future[Vector[String]] = {
    bitcoindCall[Vector[String]]("listwallets")
  }

  def walletLock(): Future[Unit] = {
    bitcoindCall[Unit]("walletlock")
  }

  def walletPassphrase(passphrase: String, seconds: Int): Future[Unit] = {
    bitcoindCall[Unit](
      "walletpassphrase",
      List(JsString(passphrase), JsNumber(seconds)))
  }

  def walletPassphraseChange(currentPassphrase: String,
                             newPassphrase: String): Future[Unit] = {
    bitcoindCall[Unit](
      "walletpassphrasechange",
      List(JsString(currentPassphrase), JsString(newPassphrase)))
  }
}
