package org.bitcoins.wallet.models

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

case class PTLCDb(
    invoiceId: Sha256DigestBE,
    network: BitcoinNetwork,
    isInitiator: Boolean,
    account: HDAccount,
    keyIndex: Int,
    refundSigOpt: Option[PartialSignature])

class PTLCTable(tag: Tag) extends Table[PTLCDb](tag, "wallet_ptlcs") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def invoiceId: Rep[Sha256DigestBE] = column("invoiceId", O.Unique)

  def network: Rep[BitcoinNetwork] = column("network")

  def isInitiator: Rep[Boolean] = column("isInitiator")

  def account: Rep[HDAccount] = column("account")

  def keyIndex: Rep[Int] = column("keyIndex")

  def refundSigOpt: Rep[Option[PartialSignature]] =
    column("refundSig")

  private type PTLCTuple = (
      Sha256DigestBE,
      BitcoinNetwork,
      Boolean,
      HDAccount,
      Int,
      Option[PartialSignature])

  private val fromTuple: PTLCTuple => PTLCDb = {
    case (invoiceId, network, isInitiator, account, keyIndex, refundSigOpt) =>
      PTLCDb(
        invoiceId,
        network,
        isInitiator,
        account,
        keyIndex,
        refundSigOpt
      )
  }

  private val toTuple: PTLCDb => Option[PTLCTuple] = ptlc =>
    Some(
      (ptlc.invoiceId,
       ptlc.network,
       ptlc.isInitiator,
       ptlc.account,
       ptlc.keyIndex,
       ptlc.refundSigOpt))

  def * : ProvenShape[PTLCDb] =
    (invoiceId, network, isInitiator, account, keyIndex, refundSigOpt) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_ptlc", sourceColumns = invoiceId)
}
