package org.bitcoins.testkit.node

import org.apache.pekko.actor.{ActorSystem, Cancellable}
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.PostgresTestDatabase
import org.bitcoins.testkit.chain.MockChainApi
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil
import scala.concurrent.duration.DurationInt

/** A base test trait for all the tests in our nodeTest module */
trait BaseNodeTest extends BitcoinSFixture with PostgresTestDatabase {

  /** Wallet config with data directory set to user temp directory */
  protected def getFreshConfig: BitcoinSAppConfig

  implicit override lazy val np: BitcoinNetwork = RegTest

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(getFreshConfig.nodeConf)
    super[PostgresTestDatabase].beforeAll()
  }

  override def afterAll(): Unit = {
    super[PostgresTestDatabase].afterAll()
    super[BitcoinSFixture].afterAll()
  }

  lazy val junkAddress: BitcoinAddress =
    BitcoinAddress("2NFyxovf6MyxfHqtVjstGzs6HeLqv92Nq4U")

  /** Helper method to generate blocks every interval
    * @return
    *   a cancellable that will stop generating blocks
    */
  def genBlockInterval(
      bitcoind: BitcoindRpcClient
  )(implicit system: ActorSystem): Cancellable = {

    var counter = 0
    val desiredBlocks = 5
    val interval = 500.millis

    val genBlock = new Runnable {
      override def run(): Unit = {
        if (counter < desiredBlocks) {
          bitcoind.generate(1)
          counter = counter + 1
        }
      }
    }

    system.scheduler.scheduleAtFixedRate(2.second, interval)(genBlock)
  }

  def getBIP39PasswordOpt(): Option[String] =
    KeyManagerTestUtil.bip39PasswordOpt

  val genesisChainApi: ChainApi = MockChainApi
}
