package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db
import org.bitcoins.core.api.wallet.db.AddressTagDb
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
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class AddressTagDAO()(implicit
    override val ec: ExecutionContext,
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

  private lazy val spkTable: profile.api.TableQuery[
    ScriptPubKeyDAO#ScriptPubKeyTable] = {
    ScriptPubKeyDAO().table
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
    Table[AddressTagDb],
    AddressTagDb,
    Seq] = {
    val (address, tagType) = id
    table
      .filter(_.address === address)
      .filter(_.tagType === tagType)
  }

  override def findAll(
      ts: Vector[AddressTagDb]): Query[Table[AddressTagDb], AddressTagDb, Seq] =
    findByPrimaryKeys(ts.map(t => (t.address, t.tagType)))

  def findByAddressAction(address: BitcoinAddress): DBIOAction[
    Vector[AddressTagDb],
    NoStream,
    Effect.Read] = {
    table.filter(_.address === address).result.map(_.toVector)
  }

  def findByAddress(address: BitcoinAddress): Future[Vector[AddressTagDb]] = {
    safeDatabase.run(findByAddressAction(address))
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

    safeDatabase.run(query.result).map(_.toVector)
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

  def dropByAddressAndName(
      address: BitcoinAddress,
      tagName: AddressTagName): Future[Int] = {
    val query = table
      .filter(_.address === address)
      .filter(_.tagName === tagName)

    safeDatabase.run(query.delete)
  }

  def findTxAction(tx: Transaction, network: NetworkParameters): DBIOAction[
    Vector[AddressTagDb],
    NoStream,
    Effect.Read] = {
    val txIds = tx.inputs.map(_.previousOutput.txIdBE)

    val findUtxosA = {
      spendingInfoTable.filter(_.txid.inSet(txIds)).result.map(_.toVector)
    }

    def findSpks(ids: Vector[Long]) = {
      spkTable.filter(_.id.inSet(ids)).result.map(_.toVector)
    }

    val spendingInfosA =
      for {
        utxos <- findUtxosA
        spks <-
          if (utxos.isEmpty) DBIO.successful(Vector.empty)
          else findSpks(utxos.map(_.scriptPubKeyId))
      } yield {
        val spksMap = spks.map(spk => (spk.id.get, spk.scriptPubKey)).toMap
        utxos.map(utxo => utxo.toSpendingInfoDb(spksMap(utxo.scriptPubKeyId)))
      }

    spendingInfosA
      .flatMap { spendingInfos =>
        if (spendingInfos.isEmpty) {
          DBIO.successful(Vector.empty)
        } else {
          val spks = spendingInfos.map(_.output.scriptPubKey)
          val addresses =
            spks.map(spk => BitcoinAddress.fromScriptPubKey(spk, network))

          val findByAddressAs =
            addresses.map(address => findByAddressAction(address))
          DBIO.sequence(findByAddressAs).map(_.flatten)
        }
      }
      .map(_.toVector)
  }

  def findTx(
      tx: Transaction,
      network: NetworkParameters): Future[Vector[AddressTagDb]] = {
    safeDatabase.run(findTxAction(tx, network))
  }

  def deleteByAddressesAction(addresses: Vector[BitcoinAddress]): DBIOAction[
    Int,
    NoStream,
    Effect.Write] = {
    table.filter(t => t.address.inSet(addresses)).delete
  }

  def deleteByAddresses(addresses: Vector[BitcoinAddress]): Future[Int] = {
    val action = deleteByAddressesAction(addresses)
    safeDatabase.run(action)
  }

  class AddressTagTable(t: Tag)
      extends Table[AddressTagDb](t, schemaName, "wallet_address_tags") {

    def address: Rep[BitcoinAddress] = column[BitcoinAddress]("address")

    def tagName: Rep[AddressTagName] = column[AddressTagName]("tag_name")

    def tagType: Rep[AddressTagType] = column[AddressTagType]("tag_type")

    private type AddressTagTuple =
      (BitcoinAddress, AddressTagName, AddressTagType)

    private val fromTuple: AddressTagTuple => AddressTagDb = {
      case (address, tagName, tagType) =>
        db.AddressTagDb(address, tagName, tagType)
    }

    private val toTuple: AddressTagDb => Option[AddressTagTuple] =
      addrTag => Some((addrTag.address, addrTag.tagName, addrTag.tagType))

    override def * : ProvenShape[AddressTagDb] =
      (address, tagName, tagType).<>(fromTuple, toTuple)

    def primaryKey: PrimaryKey =
      primaryKey("pk_address_tags", sourceColumns = (address, tagName))

    /** All tags must have an associated address */
    def fk_address = {
      foreignKey("fk_address",
                 sourceColumns = address,
                 targetTableQuery = addressTable)(_.address)
    }

  }
}
