package org.bitcoins.core.api.keymanager

import org.bitcoins.core.crypto.{ExtPrivateKey, ExtPublicKey}
import org.bitcoins.core.hd.{HDAccount, HDPath}
import org.bitcoins.core.wallet.keymanagement.KeyManagerParams

import scala.util.Try

trait KeyManagerApi

trait BIP39KeyManagerApi extends KeyManagerApi {
  def toSign(privKeyPath: HDPath): ExtPrivateKey
  def deriveXPub(account: HDAccount): Try[ExtPublicKey]
  def getRootXPub: ExtPublicKey
  def kmParams: KeyManagerParams
  def imported: Boolean
}
