package org.bitcoins.node.models

import org.bitcoins.core.p2p.AddrV2Message
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.node.config.NodeAppConfig
import scodec.bits.ByteVector
import slick.lifted.ProvenShape

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class PeerDb(
    address: ByteVector,
    port: Int,
    lastSeen: Instant,
    firstSeen: Instant,
    networkId: Byte
)

case class PeerDAO()(implicit ec: ExecutionContext, appConfig: NodeAppConfig)
    extends CRUD[PeerDb, ByteVector]
    with SlickUtil[PeerDb, ByteVector] {

  import profile.api._
  val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: TableQuery[PeerTable] =
    TableQuery[PeerTable]

  override def createAll(ts: Vector[PeerDb]): Future[Vector[PeerDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[ByteVector]): Query[PeerTable, PeerDb, Seq] = {
    table.filter(_.address.inSet(ids))
  }

  override protected def findAll(
      ts: Vector[PeerDb]): Query[Table[_], PeerDb, Seq] = findByPrimaryKeys(
    ts.map(_.address))

  def deleteByKey(address: String): Future[Int] = {
    val bytes = ByteVector(address.getBytes)
    val q = table.filter(_.address === bytes)
    safeDatabase.run(q.delete)
  }

  /** returns only non onion addresses if tor is disabled, all otherwise */
  def findAllWithTorFilter(torEnabled: Boolean): Future[Vector[PeerDb]] = {
    val q = table.filterIf(!torEnabled)(
      _.networkId =!= AddrV2Message.TOR_V3_NETWORK_BYTE)
    safeDatabase.run(q.result).map(_.toVector)
  }

  def upsertPeer(
      address: ByteVector,
      port: Int,
      networkId: Byte): Future[PeerDb] = {
    val lastSeen: Instant = Instant.now
    val existingF = read(address)
    existingF.flatMap {
      case Some(value) =>
        upsert(
          PeerDb(address,
                 port,
                 firstSeen = value.firstSeen,
                 lastSeen = lastSeen,
                 networkId = networkId))
      case None =>
        upsert(
          PeerDb(address,
                 port,
                 firstSeen = Instant.now,
                 lastSeen = lastSeen,
                 networkId = networkId))
    }
  }

  class PeerTable(tag: Tag) extends Table[PeerDb](tag, schemaName, "peers") {

    def address: Rep[ByteVector] = column("address", O.PrimaryKey)

    def port: Rep[Int] = column("port")

    def lastSeen: Rep[Instant] = column("last_seen")

    def firstSeen: Rep[Instant] = column("first_seen")

    def networkId: Rep[Byte] = column("network_id")

    def * : ProvenShape[PeerDb] =
      (address, port, lastSeen, firstSeen, networkId).<>(PeerDb.tupled,
                                                         PeerDb.unapply)
  }
}
