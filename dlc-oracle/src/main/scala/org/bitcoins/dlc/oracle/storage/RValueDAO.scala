package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.crypto.{SchnorrDigitalSignature, SchnorrNonce}
import org.bitcoins.db.{AppConfig, CRUD, DbCommonsColumnMappers, SlickUtil}
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class RValueDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: AppConfig)
    extends CRUD[RValueDb, SchnorrNonce]
    with SlickUtil[RValueDb, SchnorrNonce] {

  import profile.api._

  private val mappers = new DbCommonsColumnMappers(profile)

  import mappers._

  override val table: TableQuery[RValueTable] = TableQuery[RValueTable]

  override def createAll(ts: Vector[RValueDb]): Future[Vector[RValueDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[SchnorrNonce]): Query[RValueTable, RValueDb, Seq] =
    table.filter(_.nonce.inSet(ids))

  override protected def findAll(
      ts: Vector[RValueDb]): Query[RValueTable, RValueDb, Seq] =
    findByPrimaryKeys(ts.map(_.nonce))

  def maxKeyIndex: Future[Option[Int]] = {
    val query = table.map(_.keyIndex).max

    safeDatabase.run(query.result.transactionally)
  }

  class RValueTable(tag: Tag)
      extends Table[RValueDb](tag, schemaName, "r_values") {

    def nonce: Rep[SchnorrNonce] = column("nonce", O.PrimaryKey)

    def label: Rep[String] = column("label", O.Unique)

    def purpose: Rep[HDPurpose] = column("hd_purpose")

    def coinType: Rep[HDCoinType] = column("coin")

    def accountIndex: Rep[Int] = column("account_index")

    def chainType: Rep[Int] = column("chain_type")

    def keyIndex: Rep[Int] = column("key_index")

    def commitmentSignature: Rep[SchnorrDigitalSignature] =
      column("commitment_signature")

    def * : ProvenShape[RValueDb] =
      (nonce,
       label,
       purpose,
       coinType,
       accountIndex,
       chainType,
       keyIndex,
       commitmentSignature) <> (RValueDb.tupled, RValueDb.unapply)
  }
}
