package org.bitcoins.testkit.server

import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.server.BitcoinSServerMain
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.scalatest.FutureOutcome

import scala.concurrent.Future

/** Starts an instnace of [[BitcoinSserverMain]] that is
  * using bitcoind as a backend
  */
trait BitcoinSServerMainBitcoindFixture
    extends BitcoinSFixture
    with EmbeddedPg
    with CachedBitcoindNewest {

  override type FixtureParam = BitcoinSServerMain

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[BitcoinSServerMain] = () => {
      for {
        bitcoind <- cachedBitcoindWithFundsF
        config = BitcoinSServerMainUtil.buildBitcoindBitcoinSAppConfig(bitcoind)
        server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)
        _ <- server.start()
      } yield server
    }

    val destroy: BitcoinSServerMain => Future[Unit] = { server =>
      val stopF = server.stop()
      for {
        _ <- stopF
        _ <- BitcoinSServerMainUtil.destroyBitcoinSAppConfig(server.conf)
      } yield ()
    }

    makeDependentFixture(builder, destroy)(test)
  }
}
