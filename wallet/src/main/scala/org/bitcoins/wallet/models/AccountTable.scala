package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.hd._
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

case class AccountDb(xpub: ExtPublicKey, hdAccount: HDAccount) {}

class AccountTable(tag: Tag) extends Table[AccountDb](tag, "wallet_accounts") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def xpub: Rep[ExtPublicKey] = column[ExtPublicKey]("xpub")

  def coin: Rep[HDCoinType] = column[HDCoinType]("coin")

  def index: Rep[Int] = column[Int]("account_index")

  private type AccountTuple = (ExtPublicKey, HDCoinType, Int)

  private val fromTuple: AccountTuple => AccountDb = {
    case (pub, coin, index) =>
      AccountDb(pub, HDAccount(HDCoin(???, coin), index))
  }

  private val toTuple: AccountDb => Option[AccountTuple] = account =>
    Some(
      (account.xpub, account.hdAccount.coin.coinType, account.hdAccount.index))

  def * : ProvenShape[AccountDb] =
    (xpub, coin, index) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey("pk_account", (coin, index))
}
