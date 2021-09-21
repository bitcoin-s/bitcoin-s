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

  implicit protected lazy val torConfigOpt: Option[TorAppConfig] = {
    if (TorUtil.torEnabled) {
      val torConf =
        BitcoinSTestAppConfig.getSpvTestConfig(Vector.empty, None).torConf
      Some(torConf)
    } else {
      None
    }
  }

  protected val isTorStarted: AtomicBoolean = new AtomicBoolean(false)

  protected lazy val torF: Future[Unit] = {
    val _ = isTorStarted.set(true)
    torConfigOpt match {
      case Some(torConf) => torConf.start()
      case None          => Future.unit
    }
  }

  override def beforeAll(): Unit = {
    if (TorUtil.torEnabled && !isTorStarted.get()) {
      Await.result(torF, 70.seconds)
    }
    ()
  }

  override def afterAll(): Unit = {
    if (TorUtil.torEnabled && isTorStarted.get) {
      val torConf = torConfigOpt match {
        case Some(torConf) => torConf
        case None =>
          sys.error(s"Tor enabled but no tor configuration?")
      }
      Await.result(torConf.stop(), akkaTimeout.duration)
    }
    ()
  }
}

trait CachedTorCustomDatadir extends CachedTor { _: BitcoinSAkkaAsyncTest =>

  /** The specific datadir we should start the tor instance from */
  def customDatadir: Path

  implicit override protected lazy val torConfigOpt: Option[TorAppConfig] = {
    if (TorUtil.torEnabled) {
      val torConf = TorAppConfig.fromDatadir(customDatadir, Vector.empty)
      Some(torConf)
    } else {
      None
    }

  }
}
