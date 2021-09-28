package org.bitcoins.db

import java.io.IOException
import java.nio.file.{Files, Path}

object SQLiteUtil {

  def backup(jdbcUrl: String, backupFilePath: Path): Unit = {
    val conn = java.sql.DriverManager.getConnection(jdbcUrl)
    try {
      val existsAndWritable =
        Files.exists(backupFilePath) && Files.isWritable(backupFilePath)
      val doesntExistAndPArentIsWritable = !Files.exists(
        backupFilePath) && Files.isWritable(backupFilePath.getParent)
      if (existsAndWritable || doesntExistAndPArentIsWritable) {
        val _ =
          conn.createStatement().executeUpdate(s"BACKUP TO $backupFilePath")
      } else {
        throw new IOException(
          s"Backup destination is not writable: $backupFilePath")
      }
    } finally conn.close()
  }

}
