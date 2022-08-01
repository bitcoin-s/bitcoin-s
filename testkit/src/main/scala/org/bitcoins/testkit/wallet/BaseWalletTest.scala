package org.bitcoins.testkit.wallet

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.chain.MockChainQueryApi
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil
import org.bitcoins.testkit.util.BitcoinSAkkaAsyncTest
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.Suite

/** Base test trait for all the tests in our walletTest module */
trait BaseWalletTest extends EmbeddedPg { _: Suite with BitcoinSAkkaAsyncTest =>

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(getFreshConfig.walletConf)
    super[EmbeddedPg].beforeAll()
  }

  override def afterAll(): Unit = {
    super[EmbeddedPg].afterAll()
  }

  /** Wallet config with data directory set to user temp directory */
  protected def getFreshConfig: BitcoinSAppConfig = {
    val bipPasswordOpt = KeyManagerTestUtil.bip39PasswordOpt
    val bip39Config =
      BitcoinSWalletTest.buildBip39PasswordConfig(bipPasswordOpt)
    BaseWalletTest.getFreshConfig(pgUrl, Vector(bip39Config))
  }

  protected def getFreshWalletAppConfig: WalletAppConfig = {
    getFreshConfig.walletConf
  }

  def getBIP39PasswordOpt(): Option[String] =
    KeyManagerTestUtil.bip39PasswordOpt

  def chainQueryApi: ChainQueryApi = MockChainQueryApi

}

object BaseWalletTest {

  def getFreshConfig(pgUrl: () => Option[String], config: Vector[Config])(
      implicit system: ActorSystem): BitcoinSAppConfig = {
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl, config: _*)
  }

  def getFreshWalletAppConfig(
      pgUrl: () => Option[String],
      config: Vector[Config])(implicit system: ActorSystem): WalletAppConfig = {
    getFreshConfig(pgUrl, config).walletConf
  }

  val legacyWalletConf: Config =
    ConfigFactory.parseString("bitcoin-s.wallet.defaultAccountType = legacy")

  val segwitWalletConf: Config =
    ConfigFactory.parseString("bitcoin-s.wallet.defaultAccountType = segwit")

}
