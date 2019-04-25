package org.bitcoins.wallet.db

import org.bitcoins.db.DbManagement
import org.bitcoins.wallet.models.{
  AccountTable,
  AddressTable,
  MnemonicCodeTable,
  UTXOSpendingInfoTable
}
import slick.jdbc.SQLiteProfile.api._

sealed abstract class WalletDbManagement extends DbManagement {
  private val accountTable = TableQuery[AccountTable]
  private val addressTable = TableQuery[AddressTable]
  private val mnemonicDAO = TableQuery[MnemonicCodeTable]
  private val utxoDAO = TableQuery[UTXOSpendingInfoTable]

  override val allTables: List[TableQuery[_ <: Table[_]]] =
    List(accountTable, addressTable, mnemonicDAO, utxoDAO)

}

object WalletDbManagement extends WalletDbManagement
