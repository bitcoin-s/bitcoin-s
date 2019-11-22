package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class DLCCETSignatureDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[DLCCETSignatureDb, (Sha256DigestBE, Sha256DigestBE)] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[DLCCETSignatureTable] =
    TableQuery[DLCCETSignatureTable]

  override def createAll(
      ts: Vector[DLCCETSignatureDb]): Future[Vector[DLCCETSignatureDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(ids: Vector[(
      Sha256DigestBE,
      Sha256DigestBE)]): Query[Table[_], DLCCETSignatureDb, Seq] =
    table
      .filter(_.eventId.inSet(ids.map(_._1)))
      .filter(_.outcomeHash.inSet(ids.map(_._2)))

  override def findByPrimaryKey(id: (Sha256DigestBE, Sha256DigestBE)): Query[
    Table[_],
    DLCCETSignatureDb,
    Seq] = {
    table
      .filter(_.eventId === id._1)
      .filter(_.outcomeHash === id._2)
  }

  override def findAll(dlcs: Vector[DLCCETSignatureDb]): Query[
    Table[_],
    DLCCETSignatureDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(sig => (sig.eventId, sig.outcomeHash)))

  def findByEventId(
      eventId: Sha256DigestBE): Future[Vector[DLCCETSignatureDb]] = {
    val q = table.filter(_.eventId === eventId)
    database.run(q.result).map(_.toVector)
  }

  def findByEventId(eventId: Sha256Digest): Future[Vector[DLCCETSignatureDb]] =
    findByEventId(eventId.flip)
}
