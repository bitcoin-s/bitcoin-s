package org.bitcoins.core.api.keymanager

import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.hd.{HDAccount, HDPath}
import org.bitcoins.crypto.Sign

import scala.util.Try

trait KeyManagerApi

trait BIP39KeyManagerApi extends KeyManagerApi {
  def toSign(privKeyPath: HDPath): Sign
  def deriveXPub(account: HDAccount): Try[ExtPublicKey]
  def getRootXPub: ExtPublicKey
}
