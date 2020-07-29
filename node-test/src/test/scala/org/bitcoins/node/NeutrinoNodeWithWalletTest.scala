package org.bitcoins.node

import org.bitcoins.core.currency._
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.util.AsyncUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.UsesExperimentalBitcoind
import org.bitcoins.testkit.node.{
  NeutrinoNodeFundedWalletBitcoind,
  NodeTestUtil,
  NodeUnitTest
}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.Wallet
import org.scalatest.FutureOutcome

import scala.concurrent.{Future, Promise}

class NeutrinoNodeWithWalletTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    // We need to disable the test on non-linux CI runs
    // because we do not have a mac binary of the BIP 157
    // compatible version of bitcoin core
    if (EnvUtil.isCI && !EnvUtil.isLinux) {
      FutureOutcome.succeeded
    } else {
      withNeutrinoNodeFundedWalletBitcoind(
        test = test,
        callbacks = callbacks,
        bip39PasswordOpt = getBIP39PasswordOpt(),
        versionOpt = Some(BitcoindVersion.Experimental))
    }
  }

  private var walletP: Promise[Wallet] = Promise()
  private var walletF: Future[Wallet] = walletP.future
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
    val onBlock: OnBlockReceived = { block =>
      for {
        wallet <- walletF
        _ <- wallet.processBlock(block)
      } yield ()
    }
    val onCompactFilters: OnCompactFiltersReceived = { blockFilters =>
      for {
        wallet <- walletF
        _ <- wallet.processCompactFilters(blockFilters)
      } yield ()
    }

    NodeCallbacks(
      onBlockReceived = Vector(onBlock),
      onCompactFiltersReceived = Vector(onCompactFilters)
    )
  }

  it must "receive information about received payments" taggedAs UsesExperimentalBitcoind in {
    param =>
      val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param

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
          utxos <- wallet.listDefaultAccountUtxos()
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
          expectedUnconfirmedAmount =
            BitcoinSWalletTest.expectedDefaultAmt - TestAmount - TestFees,
          expectedUtxos = 3,
          expectedAddresses = 7
        )
      }
      val condition2 = { () =>
        condition(
          expectedConfirmedAmount = 0.sats,
          expectedUnconfirmedAmount =
            BitcoinSWalletTest.expectedDefaultAmt - TestFees,
          expectedUtxos = 4,
          expectedAddresses = 8
        )
      }

      for {
        _ <- node.sync()
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        // send
        addr <- bitcoind.getNewAddress
        _ <- wallet.sendToAddress(addr, TestAmount, Some(FeeRate))

        _ <-
          bitcoind.getNewAddress
            .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        _ <- AsyncUtil.awaitConditionF(condition1)

        // receive
        address <- wallet.getNewAddress()
        _ <-
          bitcoind
            .sendToAddress(address, TestAmount)

        _ <-
          bitcoind.getNewAddress
            .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        _ <- AsyncUtil.awaitConditionF(condition2)
      } yield succeed
  }

  it must "rescan and receive information about received payments" taggedAs UsesExperimentalBitcoind in {
    param =>
      val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param

      walletP.success(wallet)

      def condition(): Future[Boolean] = {
        for {
          balance <- wallet.getBalance()
          addresses <- wallet.listAddresses()
          utxos <- wallet.listUtxos()
        } yield {
          balance == BitcoinSWalletTest.expectedDefaultAmt + TestAmount &&
          utxos.size == 4 &&
          addresses.map(_.scriptPubKey.hex).sorted == utxos
            .map(_.output.scriptPubKey.hex)
            .sorted
        }
      }

      for {
        addresses <- wallet.listAddresses()
        utxos <- wallet.listDefaultAccountUtxos()
        _ = assert(addresses.size == 6)
        _ = assert(utxos.size == 3)

        _ <- node.sync()
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        address <- wallet.getNewAddress()
        _ <-
          bitcoind
            .sendToAddress(address, TestAmount)

        addresses <- wallet.listAddresses()
        utxos <- wallet.listDefaultAccountUtxos()
        _ = assert(addresses.size == 7)
        _ = assert(utxos.size == 3)

        _ <- wallet.clearAllUtxosAndAddresses()

        addresses <- wallet.listAddresses()
        utxos <- wallet.listDefaultAccountUtxos()
        _ = assert(addresses.isEmpty)
        _ = assert(utxos.isEmpty)

        _ <-
          bitcoind.getNewAddress
            .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        _ <- wallet.fullRescanNeutrinoWallet(addressBatchSize = 7)

        _ <- AsyncUtil.awaitConditionF(condition)
      } yield succeed
  }
}
