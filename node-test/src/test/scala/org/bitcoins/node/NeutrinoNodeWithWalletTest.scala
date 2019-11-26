package org.bitcoins.node

import akka.actor.Cancellable
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.currency._
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.util.AsyncUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.UsesExperimentalBitcoind
import org.bitcoins.testkit.node.NodeUnitTest.NeutrinoNodeFundedWalletBitcoind
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.SpendingInfoTable
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{DoNotDiscover, FutureOutcome}

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

@DoNotDiscover
class NeutrinoNodeWithWalletTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig()

  override type FixtureParam = NeutrinoNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNeutrinoNodeFundedWalletBitcoind(test,
                                         callbacks,
                                         Some(BitcoindVersion.Experimental))
  }

  private val assertionP: Promise[Boolean] = Promise()

  private val walletP: Promise[UnlockedWalletApi] = Promise()
  private val walletF: Future[UnlockedWalletApi] = walletP.future

  val amountFromBitcoind = 1.bitcoin

  def callbacks: SpvNodeCallbacks = {
    val onBlock: DataMessageHandler.OnBlockReceived = { block =>
      for {
        wallet <- walletF
        _ <- wallet.processBlock(block, confirmations = 0)
      } yield ()
    }
    SpvNodeCallbacks(
      onBlockReceived = Seq(onBlock)
    )
  }

  it must "receive information about received payments" taggedAs (UsesExperimentalBitcoind) in {
    param =>
      val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind) = param

      walletP.success(wallet)

      def clearSpendingInfoTable(): Future[Int] = {
        import slick.jdbc.SQLiteProfile.api._

        val conf: WalletAppConfig = wallet.walletConfig
        val table = TableQuery[SpendingInfoTable]
        conf.database.run(table.delete)
      }

      def condition(): Future[Boolean] = {
        for {
          balance <- wallet.getUnconfirmedBalance()
          addresses <- wallet.listAddresses()
          utxos <- wallet.listUtxos()
        } yield {
          balance == BitcoinSWalletTest.initialFunds + amountFromBitcoind &&
          utxos.size == 2 &&
          addresses.map(_.scriptPubKey) == utxos.map(_.output.scriptPubKey)
        }
      }

      for {
        addresses <- wallet.listAddresses()
        utxos <- wallet.listUtxos()
        _ = assert(addresses.size == 1)
        _ = assert(utxos.size == 1)

        _ <- node.sync()
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        address <- wallet.getNewAddress()
        _ <- bitcoind
          .sendToAddress(address, amountFromBitcoind)

        addresses <- wallet.listAddresses()
        utxos <- wallet.listUtxos()
        _ = assert(addresses.size == 2)
        _ = assert(utxos.size == 1)

        _ <- clearSpendingInfoTable()

        addresses <- wallet.listAddresses()
        utxos <- wallet.listUtxos()
        _ = assert(addresses.size == 2)
        _ = assert(utxos.size == 0)

        _ <- bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        addresses <- wallet.listAddresses()
        _ <- node.rescan(addresses.map(_.scriptPubKey))

        _ <- AsyncUtil.awaitConditionF(condition)
      } yield succeed
  }
}
