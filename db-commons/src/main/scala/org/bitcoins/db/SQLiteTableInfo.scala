package org.bitcoins.db

/** @param name The name of the table
  * @param sql The SQL executed to create the table
  */
case class SQLiteTableInfo(name: String, sql: String)
