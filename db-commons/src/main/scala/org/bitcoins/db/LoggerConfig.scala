package org.bitcoins.db

import java.nio.file.Path

import ch.qos.logback.classic.Level

abstract class LoggerConfig {

  /** Where logs are written to */
  def logLocation: Path

  /** The logging level for P2P logger */
  def p2pLogLevel: Level

  /** The logging level for chain verification logger */
  def verificationLogLevel: Level

  /** The logging level for key handling logger */
  def keyHandlingLogLevel: Level

  /** Logging level for wallet */
  def walletLogLevel: Level

  /** Logging level for HTTP RPC server */
  def httpLogLevel: Level

  /** Logging level for database logger */
  def databaseLogLevel: Level

  /** Use logback config instead */
  def useLogbackConf = true
}
