package org.bitcoins.db

import ch.qos.logback.classic.{Logger, LoggerContext}
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.rolling.{RollingFileAppender, TimeBasedRollingPolicy}
import ch.qos.logback.core.{ConsoleAppender, FileAppender}
import org.slf4j.LoggerFactory

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

  def conf: LoggerConfig

  private val context: LoggerContext = {
    val context = LoggerFactory.getILoggerFactory match {
      case ctx: LoggerContext => ctx
      case other              => sys.error(s"Expected LoggerContext, got: $other")
    }

    if (!conf.useLogbackConf) {
      // following three lines prevents Logback from reading XML conf files
      val configurator = new JoranConfigurator
      configurator.setContext(context)
      context.reset()
    }

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
    appender.setName("STDOUT")
    appender.setEncoder(encoder)
    appender.start()
    appender.asInstanceOf[ConsoleAppender[ILoggingEvent]]
  }

  /**
    * Responsible for writing to the log file
    */
  private lazy val fileAppender: FileAppender[ILoggingEvent] = {
    val logFileAppender = new RollingFileAppender()
    logFileAppender.setContext(context)
    logFileAppender.setName("FILE")
    logFileAppender.setEncoder(encoder)
    logFileAppender.setAppend(true)

    val logFilePolicy = new TimeBasedRollingPolicy()
    logFilePolicy.setContext(context)
    logFilePolicy.setParent(logFileAppender)
    logFilePolicy.setFileNamePattern("bitcoin-s-%d{yyyy-MM-dd_HH}.log")
    logFilePolicy.setMaxHistory(7)
    logFilePolicy.start()

    logFileAppender.setRollingPolicy(logFilePolicy)

    logFileAppender.setFile(conf.logFile.toString)
    logFileAppender.start()

    logFileAppender.asInstanceOf[FileAppender[ILoggingEvent]]
  }

  /** Stitches together the encoder, appenders and sets the correct
    * logging level
    */
  protected def getLoggerImpl(loggerKind: LoggerKind): Logger = {
    import LoggerKind._

    val (name, level) = loggerKind match {
      case ChainVerification =>
        ("chain-verification", conf.verificationLogLevel)
      case KeyHandling => ("KEY-HANDLING", conf.keyHandlingLogLevel)
      case P2P         => ("P2P", conf.p2pLogLevel)
      case Wallet      => ("WALLET", conf.walletLogLevel)
      case Http        => ("HTTP", conf.httpLogLevel)
      case Database    => ("DATABASE", conf.databaseLogLevel)
    }

    // This starts with "org.bitcoins" so all loggers have the same root logger for Bitcoin-S.
    // This also will make any logger using the class name fall under the same logger
    val logger: Logger = context.getLogger(s"org.bitcoins.$name")
    logger.setAdditive(true)
    logger.addAppender(fileAppender)
    logger.addAppender(consoleAppender)

    // Set level from config if we aren't using the logback.conf
    if (!conf.useLogbackConf) {
      logger.setLevel(level)
    }

    logger
  }
}
