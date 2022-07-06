package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.ChainStateDescriptorType.Sync
import org.bitcoins.db.{CRUD, SlickUtil}
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class ChainStateDescriptorDb(
    tpe: ChainStateDescriptorType,
    descriptor: ChainStateDescriptor) {
  require(descriptor.descriptorType == tpe)
}

case class ChainStateDescriptorDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: ChainAppConfig)
    extends CRUD[ChainStateDescriptorDb, ChainStateDescriptorType]
    with SlickUtil[ChainStateDescriptorDb, ChainStateDescriptorType] {
  import profile.api._

  implicit val chainStateDescriptorTypeMapper: BaseColumnType[
    ChainStateDescriptorType] =
    MappedColumnType.base[ChainStateDescriptorType, String](
      _.toString,
      ChainStateDescriptorType.fromString)

  implicit val chainStateDescriptorMapper: BaseColumnType[
    ChainStateDescriptor] =
    MappedColumnType.base[ChainStateDescriptor, String](
      _.toString,
      ChainStateDescriptor.fromString)

  override val table: profile.api.TableQuery[ChainStateDescriptorTable] =
    TableQuery[ChainStateDescriptorTable]

  override def createAll(ts: Vector[ChainStateDescriptorDb]): Future[
    Vector[ChainStateDescriptorDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override def findByPrimaryKeys(ids: Vector[ChainStateDescriptorType]): Query[
    ChainStateDescriptorTable,
    ChainStateDescriptorDb,
    Seq] = {
    table.filter(_.tpe.inSet(ids))
  }

  override def findByPrimaryKey(id: ChainStateDescriptorType): Query[
    Table[_],
    ChainStateDescriptorDb,
    Seq] = {
    table.filter(_.tpe === id)
  }

  override def findAll(ts: Vector[ChainStateDescriptorDb]): Query[
    Table[_],
    ChainStateDescriptorDb,
    Seq] =
    findByPrimaryKeys(ts.map(_.tpe))

  def getSync(): Future[Option[SyncDescriptor]] = {
    read(Sync).map {
      case Some(db) =>
        val desc = SyncDescriptor.fromString(db.descriptor.toString)
        Some(desc)
      case None => None
    }
  }

  def isSyncing: Future[Boolean] = getSync().map(_.exists(_.syncing))

  def updateSyncing(syncing: Boolean): Future[SyncDescriptor] = {
    val desc = SyncDescriptor(syncing)
    upsert(ChainStateDescriptorDb(desc.descriptorType, desc)).map(_ => desc)
  }

  class ChainStateDescriptorTable(t: Tag)
      extends Table[ChainStateDescriptorDb](t,
                                            schemaName,
                                            "state_descriptors") {

    def tpe: Rep[ChainStateDescriptorType] = column("type", O.PrimaryKey)

    def descriptor: Rep[ChainStateDescriptor] = column("descriptor")

    override def * : ProvenShape[ChainStateDescriptorDb] =
      (tpe, descriptor).<>(ChainStateDescriptorDb.tupled,
                           ChainStateDescriptorDb.unapply)

  }
}
