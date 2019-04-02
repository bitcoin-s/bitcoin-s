package org.bitcoins.node.db

import org.bitcoins.db.{DbConfig, DbManagement}
import slick.lifted.TableQuery
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

object NodeDbManagement extends DbManagement {

  override val allTables = List.empty
}
