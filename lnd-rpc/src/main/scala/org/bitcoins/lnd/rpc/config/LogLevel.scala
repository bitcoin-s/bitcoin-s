package org.bitcoins.lnd.rpc.config

import org.bitcoins.crypto.StringFactory

sealed abstract class LogLevel

object LogLevel extends StringFactory[LogLevel] {
  case object Trace extends LogLevel
  case object Debug extends LogLevel
  case object Info extends LogLevel
  case object Warn extends LogLevel
  case object Error extends LogLevel
  case object Critical extends LogLevel

  val all = Vector(Trace, Debug, Info, Warn, Error, Critical)

  override def fromStringOpt(str: String): Option[LogLevel] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }

  override def fromString(string: String): LogLevel = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find LogLevel for string=$string")
    }
  }
}
