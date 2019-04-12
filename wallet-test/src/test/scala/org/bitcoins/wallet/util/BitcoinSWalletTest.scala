package org.bitcoins.wallet.util

import akka.actor.ActorSystem
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.{CRUD, UnitTestDbConfig}
import org.bitcoins.wallet.config.WalletDbManagement
import org.bitcoins.wallet.models.{
  AccountDAO,
  AddressDAO,
  MnemonicCodeDAO,
  UTXOSpendingInfoDAO
}
import org.scalatest._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext}

trait BitcoinSWalletTest
    extends AsyncFlatSpec
    with BeforeAndAfterAll
    with BitcoinSLogger {
  implicit val actorSystem: ActorSystem = ActorSystem(getClass.getSimpleName)
  implicit val ec: ExecutionContext = actorSystem.dispatcher

  protected lazy val dbConfig: UnitTestDbConfig.type = UnitTestDbConfig
  protected val chainParams: ChainParams = WalletTestUtil.chainParams

  /** Timeout for async operations */
  protected val timeout: FiniteDuration = 10.seconds

  protected val networkParam: RegTest.type = WalletTestUtil.networkParam

}
