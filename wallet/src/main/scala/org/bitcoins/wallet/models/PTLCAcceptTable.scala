package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ptlc.PTLCMessage.PTLCAccept
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

case class PTLCAcceptDb(
    invoiceId: Sha256DigestBE,
    pubkey: ECPublicKey,
    unsignedTx: Transaction,
    adaptorSignature: ECAdaptorSignature,
    feeRate: SatoshisPerVirtualByte,
    refundAddress: BitcoinAddress) {

  def toPTLCAccept: PTLCAccept = {
    PTLCAccept(pubkey,
               unsignedTx,
               adaptorSignature,
               refundAddress,
               feeRate,
               invoiceId)
  }
}

object PTLCAcceptDb {

  def fromPTLCAccept(accept: PTLCAccept): PTLCAcceptDb = {

    PTLCAcceptDb(
      accept.invoiceId,
      accept.pubkey,
      accept.unsignedTx,
      accept.adaptorSignature,
      accept.feeRate,
      accept.refundAddress
    )
  }
}

class PTLCAcceptTable(tag: Tag)
    extends Table[PTLCAcceptDb](tag, "wallet_ptlc_accepts") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def invoiceId: Rep[Sha256DigestBE] = column("invoiceId", O.Unique)

  def pubkey: Rep[ECPublicKey] = column("pubkey")

  def unsignedTx: Rep[Transaction] = column("unsignedTx")

  def adaptorSignature: Rep[ECAdaptorSignature] = column("adaptorSignature")

  def feeRate: Rep[SatoshisPerVirtualByte] = column("feeRate")

  def refundAddress: Rep[BitcoinAddress] = column("refundAddress")

  private type PTLCAcceptTuple = (
      Sha256DigestBE,
      ECPublicKey,
      Transaction,
      ECAdaptorSignature,
      SatoshisPerVirtualByte,
      BitcoinAddress)

  private val fromTuple: PTLCAcceptTuple => PTLCAcceptDb = {
    case (invoiceId,
          pubkey,
          unsignedTx,
          adaptorSignature,
          feeRate,
          refundAddress) =>
      PTLCAcceptDb(invoiceId,
                   pubkey,
                   unsignedTx,
                   adaptorSignature,
                   feeRate,
                   refundAddress)
  }

  private val toTuple: PTLCAcceptDb => Option[PTLCAcceptTuple] = ptlc =>
    Some(
      (ptlc.invoiceId,
       ptlc.pubkey,
       ptlc.unsignedTx,
       ptlc.adaptorSignature,
       ptlc.feeRate,
       ptlc.refundAddress))

  def * : ProvenShape[PTLCAcceptDb] =
    (invoiceId, pubkey, unsignedTx, adaptorSignature, feeRate, refundAddress) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_ptlc", sourceColumns = invoiceId)

  def fk: ForeignKeyQuery[PTLCTable, PTLCDb] =
    foreignKey("fk_invoiceId",
               sourceColumns = invoiceId,
               targetTableQuery = TableQuery[PTLCTable])(_.invoiceId)
}
