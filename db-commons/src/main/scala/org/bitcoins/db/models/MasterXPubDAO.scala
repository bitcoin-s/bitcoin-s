package org.bitcoins.db.models

import org.bitcoins.core.crypto.{ChainCode, ExtKeyPubVersion, ExtPublicKey}
import org.bitcoins.core.number.{UInt32, UInt8}
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.db.{CRUD, DbAppConfig, SlickUtil}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** The primary key type is the public key associated with the extended public key [[ExtPublicKey.key]] */
case class MasterXPubDAO()(implicit
    ec: ExecutionContext,
    appConfig: DbAppConfig)
    extends CRUD[ExtPublicKey, ECPublicKey]
    with SlickUtil[ExtPublicKey, ECPublicKey] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: TableQuery[MasterXpubTable] = TableQuery[MasterXpubTable]

  override def createAll(
      extKeys: Vector[ExtPublicKey]): Future[Vector[ExtPublicKey]] = {
    createAllNoAutoInc(extKeys, safeDatabase)
  }

  override def findByPrimaryKeys(
      pubkeys: Vector[ECPublicKey]): profile.api.Query[
    profile.api.Table[ExtPublicKey],
    ExtPublicKey,
    Seq] = {
    table.filter(_.key.inSet(pubkeys))
  }

  override def findAll(ts: Vector[ExtPublicKey]): profile.api.Query[
    profile.api.Table[_],
    ExtPublicKey,
    Seq] = {
    findByPrimaryKeys(ts.map(_.key))
  }

  /** Validates the stored public key against the given xpub
    * @throws RuntimeException if the keys are different
    */
  def validate(xpub: ExtPublicKey): Future[Unit] = {
    findAll().map { xpubs =>
      require(xpubs.length == 1,
              s"Only 1 master xpub should be stored, got=${xpubs.length}")
      if (xpub != xpubs.head) {
        sys.error(
          s"Keymanager xpub and stored xpub are different, stored=${xpubs.head}, keymanager=${xpub}")
      }
    }
  }

  def existsOneXpub(): Future[Boolean] = {
    safeDatabase.run(table.exists.result)
  }

  class MasterXpubTable(tag: Tag)
      extends Table[ExtPublicKey](tag, schemaName, "master_xpub") {

    def version = column[ExtKeyPubVersion]("extpubkey_version")

    def depth = column[UInt8]("depth")

    def fingerprint = column[ByteVector]("fingerprint")

    def childNum = column[UInt32]("childnum")

    def chaincode = column[ChainCode]("chaincode")

    def key = column[ECPublicKey]("key", O.PrimaryKey, O.Unique)

    def * = {
      (version, depth, fingerprint, childNum, chaincode, key).<>(
        ExtPublicKey.tupled,
        ExtPublicKey.unapply)
    }
  }
}
