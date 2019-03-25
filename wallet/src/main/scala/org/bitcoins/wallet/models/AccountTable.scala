package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.crypto.bip44.{BIP44Account, BIP44Coin}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

case class AccountDb(xpub: ExtPublicKey, bip44Account: BIP44Account) {}

class AccountTable(tag: Tag) extends Table[AccountDb](tag, "wallet_accounts") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def xpub: Rep[ExtPublicKey] = column[ExtPublicKey]("xpub")

  def coin: Rep[BIP44Coin] = column[BIP44Coin]("coin")

  def index: Rep[Int] = column[Int]("account_index")

  private type AccountTuple = (ExtPublicKey, BIP44Coin, Int)

  private val fromTuple: AccountTuple => AccountDb = {
    case (pub, coin, index) => AccountDb(pub, BIP44Account(coin, index))
  }

  private val toTuple: AccountDb => Option[AccountTuple] = account =>
    Some((account.xpub, account.bip44Account.coin, account.bip44Account.index))

  def * : ProvenShape[AccountDb] =
    (xpub, coin, index) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey("pk_account", (coin, index))
}
