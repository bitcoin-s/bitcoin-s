package org.bitcoins.server

import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.testkit.fixtures.BitcoinSAppConfigBitcoinFixtureNotStarted

/** Test starting bitcoin-s with bitcoind as the backend for app */
class BitcoinSServerMainBitcoindTest
    extends BitcoinSAppConfigBitcoinFixtureNotStarted {

  behavior of "BitcoinSServerMainBitcoin"

  it must "start our app server with bitcoind as a backend" in {
    config: FixtureParam =>
      val server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)

      server
        .start()
        .map(_ => succeed)
  }
}
