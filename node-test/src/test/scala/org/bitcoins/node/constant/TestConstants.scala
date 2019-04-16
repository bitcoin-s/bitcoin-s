package org.bitcoins.node.constant

import org.bitcoins.db.{DbConfig, UnitTestDbConfig}
import org.bitcoins.node.db.NodeUnitTestDbConfig

/**
  * Created by chris on 9/11/16.
  */
trait TestConstants {

  /** Reads the configuration for the database specified inside of application.conf */
  def dbConfig: DbConfig = NodeUnitTestDbConfig

}

object TestConstants extends TestConstants
