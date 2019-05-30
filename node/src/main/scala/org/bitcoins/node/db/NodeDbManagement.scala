package org.bitcoins.node.db

import org.bitcoins.db.DbManagement
import org.bitcoins.node.models.InterestingPubKeyTable
import slick.lifted.TableQuery

object NodeDbManagement extends DbManagement {

  private val pubKeyTable = TableQuery[InterestingPubKeyTable]

  override val allTables = List(pubKeyTable)

}
