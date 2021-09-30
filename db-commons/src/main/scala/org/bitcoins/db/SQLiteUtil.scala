package org.bitcoins.db

import grizzled.slf4j.Logging

import java.io.IOException
import java.nio.file.{Files, Path}

object SQLiteUtil extends Logging {

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

  def setJournalMode(jdbcUrl: String, mode: String): Unit = {
    val conn = java.sql.DriverManager.getConnection(jdbcUrl)
    try {
      val _ =
        conn
          .createStatement()
          .executeUpdate(s"PRAGMA journal_mode=${mode.toUpperCase}")
    } finally conn.close()
  }

  def createDbFileIfDNE(dbPath: Path, dbName: String): Unit = {
    if (!Files.exists(dbPath)) {
      val _ = {
        logger.debug(s"Creating database directory=${dbPath}")
        Files.createDirectories(dbPath)
        val dbFilePath = dbPath.resolve(dbName)
        logger.debug(s"Creating database file=$dbFilePath")
        Files.createFile(dbFilePath)
      }
    }
  }

}
