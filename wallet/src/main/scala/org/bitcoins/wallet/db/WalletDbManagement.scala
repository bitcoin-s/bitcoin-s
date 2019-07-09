package org.bitcoins.wallet.db

import org.bitcoins.db.DbManagement
import slick.jdbc.SQLiteProfile.api._
import org.bitcoins.wallet.models._

sealed abstract class WalletDbManagement extends DbManagement {
  private val accountTable = TableQuery[AccountTable]
  private val addressTable = TableQuery[AddressTable]
  private val utxoTable = TableQuery[SpendingInfoTable]

  override val allTables: List[TableQuery[_ <: Table[_]]] =
    List(accountTable, addressTable, utxoTable)

}

object WalletDbManagement extends WalletDbManagement
