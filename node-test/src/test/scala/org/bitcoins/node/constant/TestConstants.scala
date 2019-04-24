package org.bitcoins.node.constant

import org.bitcoins.node.db.NodeDbConfig
import org.bitcoins.node.util.NodeTestUtil

/**
  * Created by chris on 9/11/16.
  */
trait TestConstants {

  /** Reads the configuration for the database specified inside of application.conf */
  def dbConfig: NodeDbConfig = NodeTestUtil.dbConfig

}

object TestConstants extends TestConstants
