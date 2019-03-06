package org.bitcoins.node.db

import org.bitcoins.db.{DbConfig, DbManagement}
import slick.jdbc.SQLiteProfile
import slick.lifted.TableQuery
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

object NodeDbManagement extends DbManagement {
  override def allTables: List[
    SQLiteProfile.api.TableQuery[_ <: SQLiteProfile.api.Table[_]]] = List.empty
}
