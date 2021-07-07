package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.wallet.db.ContractTemplateDb
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.tlv.ContractDescriptorTLV
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

case class ContractTemplateDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[ContractTemplateDb, String]
    with SlickUtil[ContractTemplateDb, String] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)

  import mappers._
  import profile.api._

  override val table: TableQuery[ContractTemplateTable] =
    TableQuery[ContractTemplateTable]

  override def createAll(
      ts: Vector[ContractTemplateDb]): Future[Vector[ContractTemplateDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(ids: Vector[String]): Query[
    ContractTemplateTable,
    ContractTemplateDb,
    Seq] =
    table.filter(_.label.inSet(ids))

  override def findByPrimaryKey(
      id: String): Query[ContractTemplateTable, ContractTemplateDb, Seq] = {
    table
      .filter(_.label === id)
  }

  override def findAll(dbs: Vector[ContractTemplateDb]): Query[
    ContractTemplateTable,
    ContractTemplateDb,
    Seq] =
    findByPrimaryKeys(dbs.map(_.label))

  def deleteByLabel(label: String): Future[Int] = {
    val q = table.filter(_.label === label)
    safeDatabase.run(q.delete)
  }

  class ContractTemplateTable(tag: Tag)
      extends Table[ContractTemplateDb](tag, schemaName, "contract_templates") {

    def label: Rep[String] = column("label", O.PrimaryKey)

    def contractDescriptor: Rep[ContractDescriptorTLV] = column(
      "contract_descriptor")

    def totalCollateral: Rep[CurrencyUnit] = column("total_collateral")

    def * : ProvenShape[ContractTemplateDb] =
      (label, contractDescriptor, totalCollateral).<>(
        ContractTemplateDb.tupled,
        ContractTemplateDb.unapply)
  }
}
