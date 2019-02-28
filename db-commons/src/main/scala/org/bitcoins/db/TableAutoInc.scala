package org.bitcoins.db

import slick.jdbc.SQLiteProfile.api._

/** Defines a table that has an auto incremented fields that is named id.
  * This is useful for things we want to store that don't have an
  * inherent id such as a hash.
  * @param tag
  * @param tableName
  * @tparam T
  */
abstract class TableAutoInc[T](tag: Tag, tableName: String)
    extends Table[T](tag, tableName) {

  def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)

}
