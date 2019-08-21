package org.bitcoins.db

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.core.rolling.RollingFileAppender
import ch.qos.logback.core.rolling.TimeBasedRollingPolicy
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.FileAppender
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.classic.joran.JoranConfigurator

/** Provides logging functionality for Bitcoin-S
  * app modules (i.e. the modules that are capable
  * of running on their own) */
private[bitcoins] trait AppLoggers {

  sealed private[bitcoins] trait LoggerKind
  protected[bitcoins] object LoggerKind {
    case object P2P extends LoggerKind
    case object ChainVerification extends LoggerKind
    case object KeyHandling extends LoggerKind
    case object Wallet extends LoggerKind
    case object Http extends LoggerKind
    case object Database extends LoggerKind
  }

  private val context = {
    val context = LoggerFactory.getILoggerFactory() match {
      case ctx: LoggerContext => ctx
      case other              => sys.error(s"Expected LoggerContext, got: $other")
    }

    // following three lines prevents Logback from reading XML conf files
    val configurator = new JoranConfigurator
    configurator.setContext(context)
    context.reset()

    context
  }

  /** Responsible for formatting our logs */
  private val encoder: Encoder[Nothing] = {
    val encoder = new PatternLayoutEncoder()
    // same date format as Bitcoin Core
    encoder.setPattern(
      s"%date{yyyy-MM-dd'T'HH:mm:ss,SSXXX} %level [%logger{0}] %msg%n")
    encoder.setContext(context)
    encoder.start()
    encoder.asInstanceOf[Encoder[Nothing]]
  }

  /** Responsible for writing to stdout
    *
    * TODO: Use different appender than file?
    */
  private lazy val consoleAppender: ConsoleAppender[ILoggingEvent] = {
    val appender = new ConsoleAppender()
    appender.setContext(context)
    appender.setName("console")
    appender.setEncoder(encoder)
    appender.asInstanceOf[ConsoleAppender[ILoggingEvent]]
  }

  /**
    * Responsible for writing to the log file
    */
  private lazy val fileAppender: FileAppender[ILoggingEvent] = {
    val logFileAppender = new RollingFileAppender()
    logFileAppender.setContext(context)
    logFileAppender.setName("logFile")
    logFileAppender.setEncoder(encoder)
    logFileAppender.setAppend(true)

    val logFilePolicy = new TimeBasedRollingPolicy()
    logFilePolicy.setContext(context)
    logFilePolicy.setParent(logFileAppender)
    logFilePolicy.setFileNamePattern("bitcoin-s-%d{yyyy-MM-dd_HH}.log")
    logFilePolicy.setMaxHistory(7)
    logFilePolicy.start()

    logFileAppender.setRollingPolicy(logFilePolicy)

    logFileAppender.asInstanceOf[FileAppender[ILoggingEvent]]
  }

  /** Stitches together the encoder, appenders and sets the correct
    * logging level
    */
  protected def getLoggerImpl(loggerKind: LoggerKind)(
      implicit conf: AppConfig): Logger = {
    import LoggerKind._

    val (name, level) = loggerKind match {
      case ChainVerification =>
        ("chain-verification", conf.verificationLogLevel)
      case KeyHandling => ("KEY-HANDLING", conf.keyHandlingLogLevel)
      case P2P         => ("P2P", conf.p2pLogLevel)
      case Wallet      => ("WALLET", conf.walletLogLeveL)
      case Http        => ("HTTP", conf.httpLogLevel)
      case Database    => ("DATABASE", conf.databaseLogLevel)
    }

    val logger = context.getLogger(name)

    if (!conf.disableFileLogging) {
      val logfile = conf.datadir.resolve(s"bitcoin-s.log")
      fileAppender.setFile(logfile.toString())
      fileAppender.start()
      logger.addAppender(fileAppender)
    }

    if (!conf.disableConsoleLogging) {
      consoleAppender.start()
      logger.addAppender(consoleAppender)
    }

    logger.setLevel(level)
    logger.setAdditive(true)

    logger
  }
}
