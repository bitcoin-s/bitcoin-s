package org.bitcoins.testkit.tor

import grizzled.slf4j.Logging
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.{BitcoinSAkkaAsyncTest, TorUtil}
import org.bitcoins.tor.config.TorAppConfig

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/** A trait that holds a cached instance of a the tor daemon
  * This is useful for using with fixtures to avoid starting tor everytime a
  * new test is run.
  */
trait CachedTor extends Logging {
  _: BitcoinSAkkaAsyncTest =>

  implicit protected lazy val torConfig: TorAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig(Vector.empty, None).torConf

  protected val isTorStarted: AtomicBoolean = new AtomicBoolean(false)

  protected lazy val torF: Future[Unit] = {
    val _ = isTorStarted.set(true)

    torConfig.start()
  }

  override def beforeAll(): Unit = {
    if (TorUtil.torEnabled && !isTorStarted.get()) {
      Await.result(torF, 70.seconds)
    }
    ()
  }

  override def afterAll(): Unit = {
    if (TorUtil.torEnabled && isTorStarted.get) {
      Await.result(torConfig.stop(), akkaTimeout.duration)
    }
    ()
  }
}

trait CachedTorCustomDatadir extends CachedTor { _: BitcoinSAkkaAsyncTest =>

  /** The specific datadir we should start the tor instance from */
  def customDatadir: Path

  implicit override protected lazy val torConfig: TorAppConfig = {
    TorAppConfig.fromDatadir(customDatadir, Vector.empty)
  }
}
