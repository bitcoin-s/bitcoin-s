package org.bitcoins.db

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantReadWriteLock

import ch.qos.logback.classic.{Logger, LoggerContext}
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.rolling.{RollingFileAppender, TimeBasedRollingPolicy}
import ch.qos.logback.core.{Appender, ConsoleAppender, FileAppender}
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

  private def context: LoggerContext = {
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
  private def encoder: Encoder[Nothing] = {
    val encoder = new PatternLayoutEncoder()
    // same date format as Bitcoin Core
    encoder.setPattern(
      s"%date{yyyy-MM-dd'T'HH:mm:ss,SSXXX} %level [%logger{0}] %msg%n")
    encoder.setContext(context)
    encoder.start()
    encoder.asInstanceOf[Encoder[Nothing]]
  }

  /** Responsible for writing to stdout
    */
  private def newConsoleAppender(
      logger: Logger): ConsoleAppender[ILoggingEvent] = {
    val appender = new ConsoleAppender()
    appender.setContext(context)
    appender.setName(s"STDOUT-${logger.hashCode}")
    appender.setEncoder(encoder)
    appender.start()
    appender.asInstanceOf[ConsoleAppender[ILoggingEvent]]
  }

  /**
    * Responsible for writing to the log file
    */
  private def newFileAppender(logger: Logger): FileAppender[ILoggingEvent] = {
    val logFileAppender = new RollingFileAppender()
    logFileAppender.setContext(context)
    logFileAppender.setName(s"FILE-${logger.hashCode}")
    logFileAppender.setEncoder(encoder)
    logFileAppender.setAppend(true)
    logFileAppender.setFile(conf.logFile.toString)

    val logFilePolicy = new TimeBasedRollingPolicy()
    logFilePolicy.setContext(context)
    logFilePolicy.setParent(logFileAppender)
    logFilePolicy.setFileNamePattern("bitcoin-s-%d{yyyy-MM-dd_HH}.log")
    logFilePolicy.setMaxHistory(7)
    logFilePolicy.start()

    logFileAppender.setRollingPolicy(logFilePolicy)

    logFileAppender.start()

    logFileAppender.asInstanceOf[FileAppender[ILoggingEvent]]
  }

  /** Stitches together the encoder, appenders and sets the correct
    * logging level
    *
    * For some reason the LoggerContext where our loggers are cached does not
    * keep appenders around, so they have to be attached _and_ started each time.
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
    if (!AppLoggers.fileAppenders.containsKey(logger.hashCode)) {
      val appender = newFileAppender(logger)
      AppLoggers.fileAppenders.put(logger.hashCode,
                                   AppLoggers.ConcurentAppender(appender))
    }
    if (!AppLoggers.consoleAppenders.containsKey(logger.hashCode)) {
      val appender = newConsoleAppender(logger)
      AppLoggers.consoleAppenders.put(logger.hashCode,
                                      AppLoggers.ConcurentAppender(appender))
    }
    val consoleAppender = AppLoggers.consoleAppenders.get(logger.hashCode)
    val fileAppender = AppLoggers.fileAppenders.get(logger.hashCode)

    logger.addAppender(consoleAppender.appender)
    logger.addAppender(fileAppender.appender)

    fileAppender.doSafeAction { appender =>
      val policy = appender
        .asInstanceOf[RollingFileAppender[ILoggingEvent]]
        .getRollingPolicy
      if (!policy.isStarted) {
        policy.start()
      }
    }

    consoleAppender.start()
    fileAppender.start()

    // Set level from config if we aren't using the logback.conf
    if (!conf.useLogbackConf) {
      logger.setLevel(level)
    }

    logger
  }
}

object AppLoggers {

  case class ConcurentAppender[T <: Appender[ILoggingEvent]](appender: T) {
    private val lock = new ReentrantReadWriteLock().writeLock()

    def isStarted: Boolean = appender.isStarted

    def start(): Unit = {
      lock.lock()
      if (!appender.isStarted) {
        try {
          appender.start()
        } finally {
          lock.unlock()
        }
      } else {
        lock.unlock()
      }
    }

    def doSafeAction(func: T => Unit): Unit = {
      lock.lock()
      try {
        func(appender)
      } finally {
        lock.unlock()
      }
    }
  }

  val fileAppenders: ConcurrentHashMap[
    Int,
    ConcurentAppender[FileAppender[ILoggingEvent]]] =
    new ConcurrentHashMap()

  val consoleAppenders: ConcurrentHashMap[
    Int,
    ConcurentAppender[ConsoleAppender[ILoggingEvent]]] =
    new ConcurrentHashMap()
}
