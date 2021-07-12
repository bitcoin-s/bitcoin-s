package org.bitcoins.explorer.env

import org.bitcoins.commons.jsonmodels.ExplorerEnv._
import org.bitcoins.commons.jsonmodels.ExplorerEnv
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ExplorerEnvTest extends BitcoinSUnitTest {

  behavior of "ExplorerEnv"

  it must "have all base uris envs end with a '/'" in {
    ExplorerEnv.all.foreach { e =>
      assert(e.baseUri.last == '/')
    }
  }

  it must "correctly parse from string" in {
    assert(ExplorerEnv.fromString("Production") == Production)
    assert(ExplorerEnv.fromString("Test") == Test)
    assert(ExplorerEnv.fromString("Local") == Local)
  }
}
