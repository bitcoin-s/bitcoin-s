package org.bitcoins.wallet.models

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.utxo.{
  AddressTag,
  AddressTagName,
  AddressTagType,
  InternalAddressTagType
}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config.WalletAppConfig
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class AddressTagDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUD[AddressTagDb, (BitcoinAddress, AddressTagType)]
    with SlickUtil[AddressTagDb, (BitcoinAddress, AddressTagType)] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: profile.api.TableQuery[AddressTagTable] =
    TableQuery[AddressTagTable]

  private lazy val spendingInfoTable: slick.lifted.TableQuery[
    SpendingInfoDAO#SpendingInfoTable] = {
    SpendingInfoDAO().table
  }

  private lazy val addressTable: slick.lifted.TableQuery[
    AddressDAO#AddressTable] = {
    AddressDAO().table
  }

  override def createAll(
      ts: Vector[AddressTagDb]): Future[Vector[AddressTagDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  /** Finds the rows that correlate to the given primary keys */
  override def findByPrimaryKeys(
      ids: Vector[(BitcoinAddress, AddressTagType)]): Query[
    AddressTagTable,
    AddressTagDb,
    Seq] = {
    val addresses = ids.map(_._1)
    val tagTypes = ids.map(_._2)
    table.filter(t => t.address.inSet(addresses) && t.tagType.inSet(tagTypes))
  }

  override def findByPrimaryKey(id: (BitcoinAddress, AddressTagType)): Query[
    Table[_],
    AddressTagDb,
    Seq] = {
    val (address, tagType) = id
    table
      .filter(_.address === address)
      .filter(_.tagType === tagType)
  }

  override def findAll(
      ts: Vector[AddressTagDb]): Query[Table[_], AddressTagDb, Seq] =
    findByPrimaryKeys(ts.map(t => (t.address, t.tagType)))

  def findByAddress(address: BitcoinAddress): Future[Vector[AddressTagDb]] = {
    val query = table.filter(_.address === address)

    safeDatabase.run(query.result).map(_.toVector)
  }

  def findByAddressAndTag(
      address: BitcoinAddress,
      tagType: AddressTagType): Future[Vector[AddressTagDb]] = {
    val query = table
      .filter(_.address === address)
      .filter(_.tagType === tagType)

    safeDatabase.runVec(query.result)
  }

  def findByTag(tag: AddressTag): Future[Vector[AddressTagDb]] = {
    val query = table
      .filter(_.tagName === tag.tagName)
      .filter(_.tagType === tag.tagType)

    safeDatabase.run(query.result).map(_.toVector)
  }

  def findByTagType(addressTagType: String): Future[Vector[AddressTagDb]] = {
    val tagType = InternalAddressTagType.fromString(addressTagType)
    findByTagType(tagType)
  }

  def findByTagType(tagType: AddressTagType): Future[Vector[AddressTagDb]] = {
    val query = table.filter(_.tagType === tagType)

    safeDatabase.run(query.result.transactionally).map(_.toVector)
  }

  def dropByTagType(tagType: AddressTagType): Future[Int] = {
    val query = table.filter(_.tagType === tagType)

    safeDatabase.run(query.delete)
  }

  def dropByAddressAndTag(
      address: BitcoinAddress,
      tagType: AddressTagType): Future[Int] = {
    val query = table
      .filter(_.address === address)
      .filter(_.tagType === tagType)

    safeDatabase.run(query.delete)
  }

  def findTx(
      tx: Transaction,
      network: NetworkParameters): Future[Vector[AddressTagDb]] = {
    val txIds = tx.inputs.map(_.previousOutput.txIdBE)
    val infoQuery = spendingInfoTable.filter(_.txid.inSet(txIds))
    val spendingInfosF = safeDatabase.runVec(infoQuery.result)

    spendingInfosF.flatMap { spendingInfos =>
      if (spendingInfos.isEmpty) {
        Future.successful(Vector.empty)
      } else {
        val spks = spendingInfos.map(_.output.scriptPubKey)
        val addresses =
          spks.map(spk => BitcoinAddress.fromScriptPubKey(spk, network))

        val findByAddressFs = addresses.map(address => findByAddress(address))
        Future.sequence(findByAddressFs).map(_.flatten)
      }
    }
  }

  class AddressTagTable(t: Tag)
      extends Table[AddressTagDb](t, "wallet_address_tags") {

    def address: Rep[BitcoinAddress] = column[BitcoinAddress]("address")

    def tagName: Rep[AddressTagName] = column[AddressTagName]("tag_name")

    def tagType: Rep[AddressTagType] = column[AddressTagType]("tag_type")

    private type AddressTagTuple =
      (BitcoinAddress, AddressTagName, AddressTagType)

    private val fromTuple: AddressTagTuple => AddressTagDb = {
      case (address, tagName, tagType) =>
        AddressTagDb(address, tagName, tagType)
    }

    private val toTuple: AddressTagDb => Option[AddressTagTuple] =
      addrTag => Some((addrTag.address, addrTag.tagName, addrTag.tagType))

    override def * : ProvenShape[AddressTagDb] =
      (address, tagName, tagType) <> (fromTuple, toTuple)

    def primaryKey: PrimaryKey =
      primaryKey("pk_address_tags", sourceColumns = (address, tagType))

    /** All tags must have an associated address */
    def fk_address: ForeignKeyQuery[_, AddressDb] = {
      foreignKey("fk_address",
                 sourceColumns = address,
                 targetTableQuery = addressTable)(_.address)
    }

  }
}
