package org.bitcoins.eclair.rpc.config

import com.typesafe.config.Config

import scala.util.{Failure, Success, Try}

object ConfigUtil {

  def getStringOrElse(config: Config, path: String, default: String): String = {
    Try(config.getString(path)) match {
      case Success(str) => str
      case Failure(_)   => default
    }
  }

  def getString(config: Config, path: String): Option[String] = {
    Try(config.getString(path)).toOption
  }

  def getIntOrElse(config: Config, path: String, default: Int): Int = {
    Try(config.getInt(path)) match {
      case Success(num) => num
      case Failure(_)   => default
    }
  }
}
