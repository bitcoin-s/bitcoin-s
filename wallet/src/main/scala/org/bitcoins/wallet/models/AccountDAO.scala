package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.hd._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class AccountDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUD[AccountDb, (HDCoin, Int)]
    with SlickUtil[AccountDb, (HDCoin, Int)] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: TableQuery[AccountTable] = TableQuery[AccountTable]

  override def createAll(ts: Vector[AccountDb]): Future[Vector[AccountDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[(HDCoin, Int)]): Query[AccountTable, AccountDb, Seq] = ???

  override def findByPrimaryKey(
      id: (HDCoin, Int)): Query[AccountTable, AccountDb, Seq] = {
    val (coin, index) = id
    table
      .filter(_.coinType === coin.coinType)
      .filter(_.purpose === coin.purpose)
      .filter(_.index === index)
  }

  override def findAll(
      accounts: Vector[AccountDb]): Query[AccountTable, AccountDb, Seq] =
    findByPrimaryKeys(
      accounts.map(acc => (acc.hdAccount.coin, acc.hdAccount.index)))

  def findByAccount(account: HDAccount): Future[Option[AccountDb]] = {
    val q = table
      .filter(_.coinType === account.coin.coinType)
      .filter(_.purpose === account.purpose)
      .filter(_.index === account.index)

    database.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case accounts: Vector[AccountDb] =>
        //yikes, we should not have more the one account per coin type/purpose
        throw new RuntimeException(
          s"More than one account per account=${account}, got=${accounts}")
    }
  }

  class AccountTable(tag: Tag)
      extends Table[AccountDb](tag, schemaName, "wallet_accounts") {

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
      (purpose, xpub, coinType, index).<>(fromTuple, toTuple)

    def primaryKey: PrimaryKey =
      primaryKey("pk_account", sourceColumns = (purpose, coinType, index))

  }
}
