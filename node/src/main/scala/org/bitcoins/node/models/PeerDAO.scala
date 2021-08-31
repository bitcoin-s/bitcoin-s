package org.bitcoins.node.models

import org.bitcoins.core.p2p.AddrV2Message
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.node.config.NodeAppConfig
import slick.lifted.ProvenShape

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class PeerDB(
    address: String,
    lastConnected: Instant,
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

  def upsertPeer(
      address: String,
      lastConnected: Instant = Instant.now,
      networkId: Byte = AddrV2Message.IPV4_NETWORK_BYTE): Unit = {
    val existingF = read(address)
    existingF.map {
      case Some(value) =>
        upsert(
          PeerDB(address,
                 firstSeen = value.firstSeen,
                 lastConnected = lastConnected,
                 networkId = networkId))
      case None =>
        upsert(
          PeerDB(address,
                 firstSeen = Instant.now,
                 lastConnected = lastConnected,
                 networkId = networkId))
    }
    ()
  }

  class PeerTable(tag: Tag) extends Table[PeerDB](tag, schemaName, "peers") {

    def address: Rep[String] = column("address", O.PrimaryKey)

    def lastConnected: Rep[Instant] = column("last_connected")

    def firstSeen: Rep[Instant] = column("first_seen")

    def networkId: Rep[Byte] = column("network_id")

    def * : ProvenShape[PeerDB] =
      (address, lastConnected, firstSeen, networkId).<>(PeerDB.tupled,
                                                        PeerDB.unapply)
  }
}
