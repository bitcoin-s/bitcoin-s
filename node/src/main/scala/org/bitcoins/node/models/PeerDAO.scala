package org.bitcoins.node.models

import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.node.config.NodeAppConfig
import scodec.bits.ByteVector
import slick.lifted.ProvenShape

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class PeerDB(
    address: ByteVector,
    port: Int,
    lastSeen: Instant,
    firstSeen: Instant,
    networkId: Byte
)

case class PeerDAO()(implicit ec: ExecutionContext, appConfig: NodeAppConfig)
    extends CRUD[PeerDB, ByteVector]
    with SlickUtil[PeerDB, ByteVector] {

  import profile.api._
  val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: TableQuery[PeerTable] =
    TableQuery[PeerTable]

  override def createAll(ts: Vector[PeerDB]): Future[Vector[PeerDB]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[ByteVector]): Query[PeerTable, PeerDB, Seq] = {
    table.filter(_.address.inSet(ids))
  }

  override protected def findAll(
      ts: Vector[PeerDB]): Query[Table[_], PeerDB, Seq] = findByPrimaryKeys(
    ts.map(_.address))

  def deleteByKey(address: String): Future[Int] = {
    val bytes = ByteVector(address.getBytes)
    val q = table.filter(_.address === bytes)
    safeDatabase.run(q.delete)
  }

  def upsertPeer(
      address: ByteVector,
      port: Int,
      networkId: Byte,
      lastSeen: Instant = Instant.now): Future[PeerDB] = {
    logger.info(s"Adding peer to db $address")
    val existingF = read(address)
    existingF.flatMap {
      case Some(value) =>
        upsert(
          PeerDB(address,
                 port = port,
                 firstSeen = value.firstSeen,
                 lastSeen = lastSeen,
                 networkId = networkId))
      case None =>
        upsert(
          PeerDB(address,
                 port = port,
                 firstSeen = Instant.now,
                 lastSeen = lastSeen,
                 networkId = networkId))
    }
  }

  class PeerTable(tag: Tag) extends Table[PeerDB](tag, schemaName, "peers") {

    def address: Rep[ByteVector] = column("address", O.PrimaryKey)

    def port: Rep[Int] = column("port")

    def lastSeen: Rep[Instant] = column("last_seen")

    def firstSeen: Rep[Instant] = column("first_seen")

    def networkId: Rep[Byte] = column("network_id")

    def * : ProvenShape[PeerDB] =
      (address, port, lastSeen, firstSeen, networkId).<>(PeerDB.tupled,
                                                         PeerDB.unapply)
  }
}
