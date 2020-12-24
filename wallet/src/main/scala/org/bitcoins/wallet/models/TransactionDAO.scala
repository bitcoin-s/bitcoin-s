package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{TransactionDb, TxDB}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

trait TxCRUDComponent[DbEntryType <: TxDB] {
  self: CRUD[DbEntryType, DoubleSha256DigestBE] =>
  import profile.api._

  abstract class TxTable[DbEntryType <: TxDB](
      tag: profile.api.Tag,
      schemaName: Option[String],
      tableName: String)
      extends Table[DbEntryType](tag, schemaName, tableName) {
    def txIdBE: Rep[DoubleSha256DigestBE]
  }
}

trait TxDAO[DbEntryType <: TxDB]
    extends CRUD[DbEntryType, DoubleSha256DigestBE]
    with TxCRUDComponent[DbEntryType]
    with SlickUtil[DbEntryType, DoubleSha256DigestBE] {
  import profile.api._
  implicit val ec: ExecutionContext

  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  type DbTable = TxTable[DbEntryType]
  override val table: TableQuery[_ <: DbTable]

  override def createAll(ts: Vector[DbEntryType]): Future[Vector[DbEntryType]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      txIdBEs: Vector[DoubleSha256DigestBE]): Query[DbTable, DbEntryType, Seq] =
    table.filter(_.txIdBE.inSet(txIdBEs))

  override def findByPrimaryKey(
      txIdBE: DoubleSha256DigestBE): Query[DbTable, DbEntryType, Seq] = {
    table.filter(_.txIdBE === txIdBE)
  }

  override def findAll(
      txs: Vector[DbEntryType]): Query[DbTable, DbEntryType, Seq] =
    findByPrimaryKeys(txs.map(_.txIdBE))

  def findByOutPoint(
      outPoint: TransactionOutPoint): Future[Option[DbEntryType]] = {
    findByTxId(outPoint.txId)
  }

  def findByTxIds(
      txIdBEs: Vector[DoubleSha256DigestBE]): Future[Vector[DbEntryType]] = {
    val q = table.filter(_.txIdBE.inSet(txIdBEs))

    safeDatabase.runVec(q.result.transactionally)
  }

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

  def findByTxIdBEs(
      txIdBEs: Vector[DoubleSha256DigestBE]): Future[Vector[DbEntryType]] = {
    database.run(findByPrimaryKeys(txIdBEs).result).map(_.toVector)
  }
}

case class TransactionDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends TxDAO[TransactionDb] {

  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table = TableQuery[TransactionTable]

  class TransactionTable(tag: Tag)
      extends TxTable[TransactionDb](tag, schemaName, "tx_table") {

    def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.Unique)

    def transaction: Rep[Transaction] = column("transaction")

    def unsignedTxIdBE: Rep[DoubleSha256DigestBE] = column("unsignedTxIdBE")

    def unsignedTx: Rep[Transaction] = column("unsignedTx")

    def wTxIdBEOpt: Rep[Option[DoubleSha256DigestBE]] =
      column("wTxIdBE")

    def totalOutput: Rep[CurrencyUnit] = column("totalOutput")

    def numInputs: Rep[Int] = column("numInputs")

    def numOutputs: Rep[Int] = column("numOutputs")

    def locktime: Rep[UInt32] = column("locktime")

    def * : ProvenShape[TransactionDb] =
      (txIdBE,
       transaction,
       unsignedTxIdBE,
       unsignedTx,
       wTxIdBEOpt,
       totalOutput,
       numInputs,
       numOutputs,
       locktime).<>(TransactionDb.tupled, TransactionDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey("pk_tx", sourceColumns = txIdBE)

  }
}
