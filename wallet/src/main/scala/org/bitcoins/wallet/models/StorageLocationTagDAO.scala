package org.bitcoins.wallet.models

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.utxo.{AddressTag, StorageLocationTag}
import org.bitcoins.wallet.config.WalletAppConfig
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

case class StorageLocationTagDAO()(
    implicit override val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends AddressTagDAO[
      StorageLocationTag,
      StorageLocationTagDb,
      StorageLocationTagTable] {
  override val typeName: String = "StorageLocationTag"
  override val table: TableQuery[StorageLocationTagTable] =
    TableQuery[StorageLocationTagTable]
  override val spendingInfoTable: TableQuery[SpendingInfoTable] =
    TableQuery[SpendingInfoTable]

  def create(
      address: BitcoinAddress,
      tag: StorageLocationTag): Future[StorageLocationTagDb] = {
    val db = StorageLocationTagDb(address, tag)
    create(db)
  }

  override def create(
      address: BitcoinAddress,
      tag: AddressTag): Future[StorageLocationTagDb] = {
    assert(tag.typeName == "StorageLocationTag")
    val db = StorageLocationTagDb(address, tag.asInstanceOf[StorageLocationTag])
    create(db)
  }
}
