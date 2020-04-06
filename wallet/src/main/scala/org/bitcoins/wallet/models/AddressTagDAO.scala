package org.bitcoins.wallet.models

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.utxo.AddressTag
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

abstract class AddressTagDAO[
    AddressTagType <: AddressTag,
    DbEntryType <: AddressTagDb[AddressTagType],
    DbTable <: AddressTagTable[AddressTagType, DbEntryType]](
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[DbEntryType, BitcoinAddress] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  val typeName: String

  override val table: TableQuery[DbTable]
  val spendingInfoTable: TableQuery[SpendingInfoTable]

  def create(address: BitcoinAddress, tag: AddressTag): Future[DbEntryType]

  override def createAll(ts: Vector[DbEntryType]): Future[Vector[DbEntryType]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(
      addresses: Vector[BitcoinAddress]): Query[Table[_], DbEntryType, Seq] =
    table.filter(_.address.inSet(addresses))

  override def findByPrimaryKey(
      address: BitcoinAddress): Query[Table[_], DbEntryType, Seq] = {
    table.filter(_.address === address)
  }

  override def findAll(
      txs: Vector[DbEntryType]): Query[Table[_], DbEntryType, Seq] =
    findByPrimaryKeys(txs.map(_.address))

  def findByTag(tag: AddressTagType): Future[Vector[DbEntryType]] = {
//    val q = table.filter(_.tag === tag) // fixme
    val q = table

    database.run(q.result).map(_.toVector).map(_.filter(_.tag == tag)) // fixme
  }

  def findTx(
      txIdBE: DoubleSha256DigestBE,
      network: NetworkParameters): Future[DbEntryType] = {
    val infoQuery = spendingInfoTable.filter(_.txid === txIdBE)
    val spendingInfosF = database.runVec(infoQuery.result)

    spendingInfosF.flatMap { spendingInfos =>
      val firstSpk = spendingInfos.head.output.scriptPubKey
      val address = BitcoinAddress.fromScriptPubKey(firstSpk, network).get

      val query = findByPrimaryKey(address)
      database.runVec(query.result).map(_.head)
    }
  }

  def findTx(
      txId: DoubleSha256Digest,
      network: NetworkParameters): Future[DbEntryType] =
    findTx(txId.flip, network)
}
