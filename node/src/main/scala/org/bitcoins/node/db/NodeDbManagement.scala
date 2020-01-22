package org.bitcoins.node.db

import org.bitcoins.db.DbManagement
import org.bitcoins.node.models.BroadcastAbleTransactionTable
import slick.lifted.TableQuery

object NodeDbManagement extends DbManagement {

  private val txTable = TableQuery[BroadcastAbleTransactionTable]

  override val allTables = List(txTable)

}
