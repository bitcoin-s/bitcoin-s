package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.crypto.{
  ExtKeyPrivVersion,
  ExtKeyPubVersion,
  ExtPublicKey
}
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.util.HDUtil

/** Represents the xpub at the account level, NOT the root xpub
  * that in conjunction with the path specified in hdAccount
  * can be used to generate the account level xpub
  * m / purpose' / coin_type' / account'
  *
  * @see https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#path-levels
  */
case class AccountDb(xpub: ExtPublicKey, hdAccount: HDAccount) {
  def xpubVersion: ExtKeyPubVersion = xpub.version

  def xprivVersion: ExtKeyPrivVersion =
    HDUtil.getMatchingExtKeyVersion(xpubVersion)

}
