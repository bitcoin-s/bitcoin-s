package org.bitcoins.eclair.rpc.config

import com.typesafe.config.Config

import scala.util.{Failure, Success, Try}

object ConfigUtil {

  def getIntOrElse(config: Config, path: String, default: Int): Int = {
    Try(config.getInt(path)) match {
      case Success(num) => num
      case Failure(_)   => default
    }
  }
}
