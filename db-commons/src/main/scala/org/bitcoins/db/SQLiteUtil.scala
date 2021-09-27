package org.bitcoins.db

object SQLiteUtil {

  def backup(jdbcUrl: String, backupFileName: String): Unit = {
    val conn = java.sql.DriverManager.getConnection(jdbcUrl)
    try {
      val _ = conn.createStatement().execute(s"BACKUP TO $backupFileName")
    } finally conn.close()
  }

}
