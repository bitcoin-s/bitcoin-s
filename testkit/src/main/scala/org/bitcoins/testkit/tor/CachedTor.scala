package org.bitcoins.testkit.tor

import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.util.{BitcoinSAkkaAsyncTest, TorUtil}
import org.bitcoins.testkit.util.TorUtil.verifyTorEnabled
import org.bitcoins.tor.client.TorClient
import org.bitcoins.tor.config.TorAppConfig

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/** A trait that holds a cached instance of a the tor daemon
  * This is useful for using with fixtures to avoid starting tor everytime a
  * new test is run.
  */
trait CachedTor {
  _: BitcoinSAkkaAsyncTest =>

  implicit protected lazy val torConfig: TorAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig().torConf

  protected val isTorStarted: AtomicBoolean = new AtomicBoolean(false)

  protected lazy val torF: Future[TorClient] = {
    val _ = isTorStarted.set(true)

    val torClient = torConfig.createClient

    for {
      _ <- torClient.startBinary()
      // wait for tor to startup
      _ <- TestAsyncUtil.nonBlockingSleep(5.seconds)
      _ = verifyTorEnabled()
    } yield torClient
  }

  override def beforeAll(): Unit = {
    if (TorUtil.torEnabled && !isTorStarted.get()) {
      Await.result(torF, duration)
    }
    ()
  }

  override def afterAll(): Unit = {
    if (isTorStarted.get()) {
      // if it was used, shut down the cached tor
      val stoppedF = for {
        tor <- torF
        _ <- tor.stopBinary()
      } yield ()

      Await.result(stoppedF, duration)
    } else () //do nothing since tor wasn't used
  }
}
