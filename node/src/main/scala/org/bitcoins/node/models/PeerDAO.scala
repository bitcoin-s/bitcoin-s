package org.bitcoins.node.models

import org.bitcoins.core.p2p.AddrV2Message
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.node.config.NodeAppConfig
import slick.lifted.ProvenShape

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class PeerDB(
    address: String,
    lastSeen: Instant,
    firstSeen: Instant,
    networkId: Byte
)

case class PeerDAO()(implicit ec: ExecutionContext, appConfig: NodeAppConfig)
    extends CRUD[PeerDB, String]
    with SlickUtil[PeerDB, String] {

  import profile.api._

  override val table: TableQuery[PeerTable] =
    TableQuery[PeerTable]

  override def createAll(ts: Vector[PeerDB]): Future[Vector[PeerDB]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[String]): Query[PeerTable, PeerDB, Seq] = {
    table.filter(_.address.inSet(ids))
  }

  override protected def findAll(
      ts: Vector[PeerDB]): Query[Table[_], PeerDB, Seq] = findByPrimaryKeys(
    ts.map(_.address))

  def deleteByKey(address: String):Future[Int]={
    val q=table.filter(_.address===address)
    safeDatabase.run(q.delete)
  }

  def upsertPeer(
      address: String,
      lastSeen: Instant = Instant.now,
      networkId: Byte = AddrV2Message.IPV4_NETWORK_BYTE): Unit = {
    logger.info(s"Adding peer to db $address")
    val existingF = read(address)
    existingF.map {
      case Some(value) =>
        upsert(
          PeerDB(address,
                 firstSeen = value.firstSeen,
                 lastSeen = lastSeen,
                 networkId = networkId))
      case None =>
        upsert(
          PeerDB(address,
                 firstSeen = Instant.now,
                 lastSeen = lastSeen,
                 networkId = networkId))
    }
    ()
  }

  class PeerTable(tag: Tag) extends Table[PeerDB](tag, schemaName, "peers") {

    def address: Rep[String] = column("address", O.PrimaryKey)

    def lastSeen: Rep[Instant] = column("last_seen")

    def firstSeen: Rep[Instant] = column("first_seen")

    def networkId: Rep[Byte] = column("network_id")

    def * : ProvenShape[PeerDB] =
      (address, lastSeen, firstSeen, networkId).<>(PeerDB.tupled,
                                                   PeerDB.unapply)
  }
}
