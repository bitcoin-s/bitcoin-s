package org.bitcoins.server

import org.bitcoins.testkit.util.BitcoinSAsyncTest

class ServerRunTest extends BitcoinSAsyncTest {

  // Clear log location property
  after {
    System.clearProperty("bitcoins.log.location")
  }

}
