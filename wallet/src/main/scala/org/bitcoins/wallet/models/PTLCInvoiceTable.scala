package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ptlc.PTLCMessage.PTLCInvoice
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

case class PTLCInvoiceDb(
    invoiceId: Sha256DigestBE,
    adaptorScalarOpt: Option[ECPrivateKey],
    adaptorPoint: ECPublicKey,
    amount: CurrencyUnit,
    pubkey: ECPublicKey,
    finalAddress: BitcoinAddress,
    timeout: UInt32) {

  def toPTLCInvoice: PTLCInvoice = {
    PTLCInvoice(adaptorPoint, amount, pubkey, finalAddress, timeout)
  }
}

object PTLCInvoiceDb {

  def fromPTLCInvoice(
      invoice: PTLCInvoice,
      adaptorScalarOpt: Option[ECPrivateKey]): PTLCInvoiceDb = {

    PTLCInvoiceDb(
      invoice.invoiceId,
      adaptorScalarOpt,
      invoice.adaptorPoint,
      invoice.amount,
      invoice.pubkey,
      invoice.finalAddress,
      invoice.timeout
    )
  }
}

class PTLCInvoiceTable(tag: Tag)
    extends Table[PTLCInvoiceDb](tag, "wallet_ptlc_invoices") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def invoiceId: Rep[Sha256DigestBE] = column("invoiceId", O.Unique)

  def adaptorScalarOpt: Rep[Option[ECPrivateKey]] = column("adaptorScalar")

  def adaptorPoint: Rep[ECPublicKey] = column("adaptorPoint")

  def amount: Rep[CurrencyUnit] = column("amount")

  def pubkey: Rep[ECPublicKey] = column("pubkey")

  def finalAddress: Rep[BitcoinAddress] = column("finalAddress")

  def timeout: Rep[UInt32] = column("timeout")

  private type PTLCInvoiceTuple = (
      Sha256DigestBE,
      Option[ECPrivateKey],
      ECPublicKey,
      CurrencyUnit,
      ECPublicKey,
      BitcoinAddress,
      UInt32)

  private val fromTuple: PTLCInvoiceTuple => PTLCInvoiceDb = {
    case (invoiceId,
          adaptorScalarOpt,
          adaptorPoint,
          amount,
          pubkey,
          finalAddress,
          timeout) =>
      PTLCInvoiceDb(invoiceId,
                    adaptorScalarOpt,
                    adaptorPoint,
                    amount,
                    pubkey,
                    finalAddress,
                    timeout)
  }

  private val toTuple: PTLCInvoiceDb => Option[PTLCInvoiceTuple] = ptlc =>
    Some(
      (ptlc.invoiceId,
       ptlc.adaptorScalarOpt,
       ptlc.adaptorPoint,
       ptlc.amount,
       ptlc.pubkey,
       ptlc.finalAddress,
       ptlc.timeout))

  def * : ProvenShape[PTLCInvoiceDb] =
    (invoiceId,
     adaptorScalarOpt,
     adaptorPoint,
     amount,
     pubkey,
     finalAddress,
     timeout) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_ptlc", sourceColumns = invoiceId)

  def fk: ForeignKeyQuery[PTLCTable, PTLCDb] =
    foreignKey("fk_invoiceId",
               sourceColumns = invoiceId,
               targetTableQuery = TableQuery[PTLCTable])(_.invoiceId)
}
