package org.bitcoins.node.db

import org.bitcoins.db._

trait NodeDbConfig { this: DbConfig =>
  override val configPath: String = "node.conf"
}

object NodeMainNetDbConfig extends MainNetDbConfig with NodeDbConfig

object NodeTestNet3DbConfig extends TestNet3DbConfig with NodeDbConfig

object NodeRegTestDbConfig extends RegTestDbConfig with NodeDbConfig

object NodeUnitTestDbConfig extends UnitTestDbConfig with NodeDbConfig
