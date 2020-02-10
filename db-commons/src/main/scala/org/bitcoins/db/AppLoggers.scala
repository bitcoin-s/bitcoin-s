package org.bitcoins.db

import ch.qos.logback.classic
import org.slf4j.{LoggerFactory, MDC}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.core.rolling.RollingFileAppender
import ch.qos.logback.core.rolling.TimeBasedRollingPolicy
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.FileAppender
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.classic.joran.JoranConfigurator
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}

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

  def conf: AppConfig

  private val context = {
    val context = LoggerFactory.getILoggerFactory match {
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
      s"%date{yyyy-MM-dd'T'HH:mm:ss,SSXXX} %level [%X{name}] %msg%n")
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
    appender.start()
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

    val baseDir = conf.baseDatadir
    val lastDirname = conf.network match {
      case MainNet  => "mainnet"
      case TestNet3 => "testnet3"
      case RegTest  => "regtest"
    }
    val logFile = baseDir.resolve(lastDirname).resolve(s"bitcoin-s.log")

    logFileAppender.setFile(logFile.toString)
    logFileAppender.start()

    logFileAppender.asInstanceOf[FileAppender[ILoggingEvent]]
  }

  private lazy val logger: classic.Logger = {
    val logger = context.getLogger("Bitcoin-S")
    logger.addAppender(fileAppender)
    logger.addAppender(consoleAppender)
    logger.setAdditive(true)
    logger
  }

  /** Stitches together the encoder, appenders and sets the correct
    * logging level
    */
  protected def getLoggerImpl(loggerKind: LoggerKind): MarkedLogger = {
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

    logger.setLevel(level)

    MarkedLogger(name, logger)
  }
}

case class MarkedLogger(name: String, logger: classic.Logger) {
  private def setContext(): Unit = {
    MDC.put("name", name)
  }

  def info(message: String): Unit = {
    setContext()
    logger.info(message)
  }

  def info(message: String, t: Throwable): Unit = {
    setContext()
    logger.info(message, t)
  }

  def debug(message: String): Unit = {
    setContext()
    logger.debug(message)
  }

  def error(message: String): Unit = {
    setContext()
    logger.error(message)
  }

  def error(message: String, t: Throwable): Unit = {
    setContext()
    logger.error(message, t)
  }

  def trace(message: String): Unit = {
    setContext()
    logger.trace(message)
  }

  def warn(message: String): Unit = {
    setContext()
    logger.warn(message)
  }

  def warn(message: String, t: Throwable): Unit = {
    setContext()
    logger.warn(message, t)
  }
}
