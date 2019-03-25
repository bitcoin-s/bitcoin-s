package org.bitcoins.wallet.config

import org.bitcoins.db.DbManagement
import org.bitcoins.wallet.models.{
  AccountTable,
  AddressTable,
  MnemonicCodeTable
}
import slick.jdbc.SQLiteProfile.api._

sealed abstract class WalletDbManagement extends DbManagement {
  private val accountTable = TableQuery[AccountTable]
  private val addressTable = TableQuery[AddressTable]
  private val mnemonicDAO = TableQuery[MnemonicCodeTable]

  override val allTables: List[TableQuery[_ <: Table[_]]] =
    List(accountTable, addressTable, mnemonicDAO)

}

object WalletDbManagement extends WalletDbManagement
