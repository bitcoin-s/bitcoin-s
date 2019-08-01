package org.bitcoins

import com.typesafe.config.Config
import com.typesafe.config.ConfigRenderOptions

package object db {

  implicit class ConfigOps(private val config: Config) extends AnyVal {

    def asReadableJson: String = {
      val options = ConfigRenderOptions.concise().setFormatted(true)
      config.root().render(options)
    }

    /** Returns the string at key or the given default value */
    def getStringOrElse(key: String, default: => String): String = {
      if (config.hasPath(key)) {
        config.getString(key)
      } else {
        default
      }
    }

    /** Returns the string at the given key, if it exists */
    def getStringOrNone(key: String): Option[String] = {
      if (config.hasPath(key)) {
        Some(config.getString(key))
      } else {
        None
      }
    }
  }

}
