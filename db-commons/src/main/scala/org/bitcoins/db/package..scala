package org.bitcoins

import com.typesafe.config.Config
import com.typesafe.config.ConfigRenderOptions

package object db {

  implicit class ConfigOps(private val config: Config) extends AnyVal {

    def asReadableJson: String = {
      val options = ConfigRenderOptions.concise().setFormatted(true)
      config.root().render(options)
    }
  }

}
