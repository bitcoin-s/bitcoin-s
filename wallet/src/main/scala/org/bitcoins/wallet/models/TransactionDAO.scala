package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

trait TxDAO[DbEntryType <: TxDB, DbTable <: TxTable[DbEntryType]]
    extends CRUD[DbEntryType, DoubleSha256DigestBE] {

  implicit val ec: ExecutionContext

  val appConfig: WalletAppConfig

  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[DbTable]

  override def createAll(ts: Vector[DbEntryType]): Future[Vector[DbEntryType]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(txIdBEs: Vector[
    DoubleSha256DigestBE]): Query[Table[_], DbEntryType, Seq] =
    table.filter(_.txIdBE.inSet(txIdBEs))

  override def findByPrimaryKey(
      txIdBE: DoubleSha256DigestBE): Query[Table[_], DbEntryType, Seq] = {
    table.filter(_.txIdBE === txIdBE)
  }

  override def findAll(
      txs: Vector[DbEntryType]): Query[Table[_], DbEntryType, Seq] =
    findByPrimaryKeys(txs.map(_.txIdBE))

  def findByTxId(txIdBE: DoubleSha256DigestBE): Future[Option[DbEntryType]] = {
    val q = table
      .filter(_.txIdBE === txIdBE)

    database.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case txs: Vector[DbEntryType] =>
        // yikes, we should not have more the one transaction per id
        throw new RuntimeException(
          s"More than one transaction per id=${txIdBE.hex}, got=$txs")
    }
  }

  def findByTxId(txId: DoubleSha256Digest): Future[Option[DbEntryType]] =
    findByTxId(txId.flip)
}

case class TransactionDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends TxDAO[TransactionDb, TransactionTable] {
  override val table = TableQuery[TransactionTable]
}
