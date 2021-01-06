package org.bitcoins.wallet.models

import org.bitcoins.commons.jsonmodels.wallet.WalletStateDescriptorType._
import org.bitcoins.commons.jsonmodels.wallet.{
  SyncHeightDescriptor,
  WalletStateDescriptor,
  WalletStateDescriptorType
}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config.WalletAppConfig
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class WalletStateDescriptorDb(
    tpe: WalletStateDescriptorType,
    descriptor: WalletStateDescriptor) {
  require(descriptor.descriptorType == tpe)
}

case class WalletStateDescriptorDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUD[WalletStateDescriptorDb, WalletStateDescriptorType]
    with SlickUtil[WalletStateDescriptorDb, WalletStateDescriptorType] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: profile.api.TableQuery[WalletStateDescriptorTable] =
    TableQuery[WalletStateDescriptorTable]

  override def createAll(ts: Vector[WalletStateDescriptorDb]): Future[
    Vector[WalletStateDescriptorDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override def findByPrimaryKeys(ids: Vector[WalletStateDescriptorType]): Query[
    WalletStateDescriptorTable,
    WalletStateDescriptorDb,
    Seq] = {
    table.filter(_.tpe.inSet(ids))
  }

  override def findByPrimaryKey(id: WalletStateDescriptorType): Query[
    Table[_],
    WalletStateDescriptorDb,
    Seq] = {
    table.filter(_.tpe === id)
  }

  override def findAll(ts: Vector[WalletStateDescriptorDb]): Query[
    Table[_],
    WalletStateDescriptorDb,
    Seq] =
    findByPrimaryKeys(ts.map(_.tpe))

  def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]] = {
    read(SyncHeight).map {
      case Some(db) =>
        val desc = SyncHeightDescriptor.fromString(db.descriptor.toString)
        Some(desc)
      case None => None
    }
  }

  def updateSyncHeight(
      hash: DoubleSha256DigestBE,
      height: Int): Future[WalletStateDescriptorDb] = {
    getSyncDescriptorOpt().flatMap {
      case Some(old) =>
        if (old.height > height) {
          Future.successful(WalletStateDescriptorDb(SyncHeight, old))
        } else {
          val descriptor = SyncHeightDescriptor(hash, height)
          val newDb = WalletStateDescriptorDb(SyncHeight, descriptor)
          update(newDb)
        }
      case None =>
        val descriptor = SyncHeightDescriptor(hash, height)
        val db = WalletStateDescriptorDb(SyncHeight, descriptor)
        create(db)
    }
  }

  class WalletStateDescriptorTable(t: Tag)
      extends Table[WalletStateDescriptorDb](t,
                                             schemaName,
                                             "state_descriptors") {

    def tpe: Rep[WalletStateDescriptorType] = column("type", O.PrimaryKey)

    def descriptor: Rep[WalletStateDescriptor] = column("descriptor")

    override def * : ProvenShape[WalletStateDescriptorDb] =
      (tpe, descriptor).<>(WalletStateDescriptorDb.tupled,
                           WalletStateDescriptorDb.unapply)

  }
}
