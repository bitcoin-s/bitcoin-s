package org.bitcoins.node.constant

import org.bitcoins.db.{DbConfig, UnitTestDbConfig}

/**
  * Created by chris on 9/11/16.
  */
trait TestConstants {

  /** Reads the configuration for the database specified inside of application.conf */
  def dbConfig: DbConfig = UnitTestDbConfig

}

object TestConstants extends TestConstants
