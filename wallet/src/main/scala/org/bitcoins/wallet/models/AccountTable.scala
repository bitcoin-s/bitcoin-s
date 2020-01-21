package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.hd._
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}
import org.bitcoins.core.crypto._
import org.bitcoins.keymanager.util.HDUtil

/** Represents the xpub at the account level, NOT the root xpub
  * that in conjunction with the path specified in hdAccount
  * can be used to generate the account level xpub */
case class AccountDb(xpub: ExtPublicKey, hdAccount: HDAccount) {
  def xpubVersion: ExtKeyPubVersion = xpub.version

  def xprivVersion: ExtKeyPrivVersion =
    HDUtil.getMatchingExtKeyVersion(xpubVersion)

}

class AccountTable(tag: Tag) extends Table[AccountDb](tag, "wallet_accounts") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def xpub: Rep[ExtPublicKey] = column[ExtPublicKey]("xpub")

  def purpose: Rep[HDPurpose] = column[HDPurpose]("hd_purpose")

  def coinType: Rep[HDCoinType] = column[HDCoinType]("coin")

  def index: Rep[Int] = column[Int]("account_index")

  private type AccountTuple = (HDPurpose, ExtPublicKey, HDCoinType, Int)

  private val fromTuple: AccountTuple => AccountDb = {
    case (purpose, pub, coin, index) =>
      AccountDb(pub, HDAccount(HDCoin(purpose, coin), index))
  }

  private val toTuple: AccountDb => Option[AccountTuple] = account =>
    Some(
      (account.hdAccount.purpose,
       account.xpub,
       account.hdAccount.coin.coinType,
       account.hdAccount.index))

  def * : ProvenShape[AccountDb] =
    (purpose, xpub, coinType, index) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey("pk_account", sourceColumns = (purpose, coinType, index))

}
