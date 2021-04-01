package org.bitcoins.explorer.env

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ExplorerEnvTest extends BitcoinSUnitTest {

  behavior of "ExplorerEnv"

  it must "have all base uris envs end with a '/'" in {
    ExplorerEnv.all.foreach { e =>
      assert(e.baseUri.last == '/')
    }
  }
}
