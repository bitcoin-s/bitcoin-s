package org.bitcoins.node.db

import org.bitcoins.db.DbManagement
import slick.lifted.TableQuery
import org.bitcoins.node.models.BroadcastAbleTransactionTable

object NodeDbManagement extends DbManagement {

  private val txTable = TableQuery[BroadcastAbleTransactionTable]

  override val allTables = List(txTable)
}
