package org.bitcoins.node

import org.bitcoins.core.currency._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.node.networking.peer.DataMessageHandler.OnCompactFilterReceived
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
import org.scalatest.{DoNotDiscover, FutureOutcome}

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

  private var walletP: Promise[UnlockedWalletApi] = Promise()
  private var walletF: Future[UnlockedWalletApi] = walletP.future
  after {
    //reset assertion after a test runs, because we
    //are doing mutation to work around our callback
    //limitations, we can't currently modify callbacks
    //after a NeutrinoNode is constructed :-(
    walletP = Promise()
    walletF = walletP.future
  }

  val TestAmount = 1.bitcoin
  val FeeRate = SatoshisPerByte(10.sats)
  val TestFees = 2240.sats

  def callbacks: NodeCallbacks = {
    val onBlock: DataMessageHandler.OnBlockReceived = { block =>
      for {
        wallet <- walletF
        _ <- wallet.processBlock(block)
      } yield ()
    }
    val onCompactFilter: OnCompactFilterReceived = { (blockHash, blockFilter) =>
      for {
        wallet <- walletF
        _ <- wallet.processCompactFilter(blockHash, blockFilter)
      } yield ()
    }

    NodeCallbacks(
      onBlockReceived = Seq(onBlock),
      onCompactFilterReceived = Seq(onCompactFilter)
    )
  }

  it must "receive information about received payments" taggedAs (UsesExperimentalBitcoind) in {
    param =>
      val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind) = param

      walletP.success(wallet)

      def condition(
          expectedConfirmedAmount: CurrencyUnit,
          expectedUnconfirmedAmount: CurrencyUnit,
          expectedUtxos: Int,
          expectedAddresses: Int): Future[Boolean] = {
        for {
          confirmedBalance <- wallet.getConfirmedBalance()
          unconfirmedBalance <- wallet.getUnconfirmedBalance()
          addresses <- wallet.listAddresses()
          utxos <- wallet.listUtxos()
        } yield {
          (expectedConfirmedAmount == confirmedBalance) &&
          (expectedUnconfirmedAmount == unconfirmedBalance) &&
          (expectedAddresses == addresses.size) &&
          (expectedUtxos == utxos.size)
        }
      }

      val condition1 = () => {
        condition(
          expectedConfirmedAmount = 0.sats,
          expectedUnconfirmedAmount = BitcoinSWalletTest.initialFunds - TestAmount - TestFees,
          expectedUtxos = 1,
          expectedAddresses = 2
        )
      }
      val condition2 = { () =>
        condition(
          expectedConfirmedAmount = TestAmount,
          expectedUnconfirmedAmount = BitcoinSWalletTest.initialFunds - TestAmount - TestFees,
          expectedUtxos = 2,
          expectedAddresses = 3
        )
      }

      for {
        _ <- node.sync()
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        // send
        addr <- bitcoind.getNewAddress
        _ <- wallet.sendToAddress(addr, TestAmount, FeeRate)

        _ <- bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        _ <- AsyncUtil.awaitConditionF(condition1)

        // receive
        address <- wallet.getNewAddress()
        _ <- bitcoind
          .sendToAddress(address, TestAmount)

        _ <- bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        _ <- AsyncUtil.awaitConditionF(condition2)
      } yield succeed
  }

  it must "rescan and receive information about received payments" taggedAs (UsesExperimentalBitcoind) in {
    param =>
      val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind) = param

      walletP.success(wallet)

      def condition(): Future[Boolean] = {
        for {
          balance <- wallet.getConfirmedBalance()
          addresses <- wallet.listAddresses()
          utxos <- wallet.listUtxos()
        } yield {
          balance == BitcoinSWalletTest.initialFunds + TestAmount &&
          utxos.size == 2 &&
          addresses.map(_.scriptPubKey.hex).sorted == utxos
            .map(_.output.scriptPubKey.hex)
            .sorted
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
          .sendToAddress(address, TestAmount)

        addresses <- wallet.listAddresses()
        utxos <- wallet.listUtxos()
        _ = assert(addresses.size == 2)
        _ = assert(utxos.size == 1)

        _ <- wallet.clearUtxosAndAddresses()

        addresses <- wallet.listAddresses()
        utxos <- wallet.listUtxos()
        _ = assert(addresses.size == 2)
        _ = assert(utxos.size == 0)

        _ <- bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        _ <- wallet.rescanNeutrinoWallet(startOpt = None,
                                         endOpt = None,
                                         addressBatchSize = 2)

        _ <- AsyncUtil.awaitConditionF(condition)
      } yield succeed
  }
}
