package org.bitcoins.rpc.client

import org.bitcoins.rpc.jsonmodels.{DumpWalletResult, GetWalletInfoResult}
import play.api.libs.json.JsString

import scala.concurrent.Future

trait WalletCalls extends Client {

  def backupWallet(destination: String): Future[Unit] = {
    bitcoindCall[Unit]("backupwallet", List(JsString(destination)))
  }

  def dumpWallet(filePath: String): Future[DumpWalletResult] = {
    bitcoindCall[DumpWalletResult]("dumpwallet", List(JsString(filePath)))
  }

  def encryptWallet(passphrase: String): Future[String] = {
    bitcoindCall[String]("encryptwallet", List(JsString(passphrase)))
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
