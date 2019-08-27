package org.bitcoins.chain.config

import akka.actor.ActorSystem
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome

class ChainAppConfigTest extends ChainUnitTest {

  val chainAppConfig = appConfig

  implicit override val system = ActorSystem("ChainAppConfigTest")

  behavior of "ChainAppConfig"

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainFixture(test)

  it must "initialize our chain project" in { _ =>
    val isInitF = chainAppConfig.isInitialized()

    for {
      isInit <- isInitF
      _ = assert(!isInit)
      _ <- chainAppConfig.initialize()
      isInitAgain <- chainAppConfig.isInitialized()
    } yield assert(isInitAgain)
  }

  override def afterAll: Unit = {

    FileUtil.deleteTmpDir(chainAppConfig.baseDatadir)
  }
}
