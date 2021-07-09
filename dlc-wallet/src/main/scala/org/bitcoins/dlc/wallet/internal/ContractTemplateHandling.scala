package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.api.wallet
import org.bitcoins.core.api.wallet.db.ContractTemplateDb
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models._

import scala.concurrent.Future

/** Handles fetching and constructing different contract templates from the database */
trait ContractTemplateHandling {
  self: DLCWallet =>

  private[bitcoins] val contractTemplateDAO: ContractTemplateDAO =
    ContractTemplateDAO()

  override def createContractTemplate(
      label: String,
      descriptorTLV: ContractDescriptorTLV,
      totalCollateral: CurrencyUnit): Future[ContractTemplateDb] = {
    val db = wallet.db.ContractTemplateDb(label, descriptorTLV, totalCollateral)

    contractTemplateDAO.create(db)
  }

  override def findContractTemplate(
      label: String): Future[Option[ContractTemplateDb]] = {
    contractTemplateDAO.read(label)
  }

  override def getContractTemplates: Future[Vector[ContractTemplateDb]] = {
    contractTemplateDAO.findAll()
  }

  override def deleteContractTemplate(label: String): Future[Unit] = {
    contractTemplateDAO.deleteByLabel(label).map(_ => ())
  }
}
