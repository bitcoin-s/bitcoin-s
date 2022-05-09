package org.bitcoins.wallet.models

import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config.WalletAppConfig
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class WalletStateDb(id: Int, isRescanning: Boolean) {
  require(id == 1, s"invalid wallet state ID: `$id`")
}

case class WalletStateDAO()(implicit
    ec: ExecutionContext,
    config: WalletAppConfig)
    extends CRUD[WalletStateDb, Int]
    with SlickUtil[WalletStateDb, Int] {

  import profile.api._

  def storeWalletState(ws: WalletStateDb): Future[WalletStateDb] =
    create(ws.copy(id = 1))

  def loadWalletState(): Future[Option[WalletStateDb]] =
    safeDatabase.run(findByPrimaryKey(1).result).map(_.headOption)

  def isRescanning: Future[Boolean] =
    loadWalletState().map(_.exists(_.isRescanning))

  def setRescanning(value: Boolean): Future[Boolean] = {
    val actions = for {
      dbs <- table.filter(_.id === 1).result
      res <- dbs.headOption match {
        case None =>
          val db = WalletStateDb(id = 1, isRescanning = value)
          table += db
        case Some(db) =>
          table.update(db.copy(isRescanning = value))
      }
    } yield res

    safeDatabase.run(actions).map(_ => value)
  }

  def compareAndSetRescanning(
      expectedValue: Boolean,
      newValue: Boolean): Future[Boolean] = {
    val actions = for {
      dbs <- table.filter(_.id === 1).result
      res <- dbs.headOption match {
        case None =>
          val db = WalletStateDb(id = 1, isRescanning = newValue)
          (table += db).flatMap(_ => DBIO.successful(true))
        case Some(db) =>
          if (db.isRescanning == expectedValue) {
            table
              .update(db.copy(isRescanning = newValue))
              .flatMap(_ => DBIO.successful(true))
          } else {
            DBIO.successful(false)
          }
      }
    } yield res

    safeDatabase.run(actions)
  }

  override def createAll(
      ts: Vector[WalletStateDb]): Future[Vector[WalletStateDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[Int]): Query[Table[WalletStateDb], WalletStateDb, Seq] =
    table.filter(_.id.inSet(ids))

  override protected def findAll(
      ts: Vector[WalletStateDb]): Query[Table[_], WalletStateDb, Seq] =
    findByPrimaryKeys(ts.map(_.id))

  /** The table inside our database we are inserting into */
  override val table: TableQuery[WalletStateTable] =
    TableQuery[WalletStateTable]

  class WalletStateTable(t: Tag)
      extends Table[WalletStateDb](t, schemaName, "wallet_state") {

    def id: Rep[Int] = column("id", O.PrimaryKey)

    def isRescanning: Rep[Boolean] = column("is_rescanning")

    override def * : ProvenShape[WalletStateDb] =
      (id, isRescanning) <> (WalletStateDb.tupled, WalletStateDb.unapply)

  }
}
