package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.script.MultiSignatureScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.node.{
  NeutrinoNodeFundedWalletBitcoind,
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest
}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.models.{AddressDAO, ScriptPubKeyDAO}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

class NeutrinoNodeWithWalletTest extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withNeutrinoNodeFundedWalletBitcoind(
        test = test,
        bip39PasswordOpt = getBIP39PasswordOpt(),
        bitcoind = bitcoind
      )(system, getFreshConfig)
      f <- outcome.toFuture
    } yield f

    new FutureOutcome(outcomeF)
  }

  val TestAmount = 1.bitcoin
  val FeeRate = SatoshisPerByte(10.sats)
  val TestFees: Satoshis = 2220.sats

  it must "receive information about received payments" in { param =>
    val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param

    def condition(
        expectedBalance: CurrencyUnit,
        expectedUtxos: Int,
        expectedAddresses: Int): Future[Boolean] = {
      for {
        balance <- wallet.getBalance()
        addresses <- wallet.listAddresses()
        utxos <- wallet.listDefaultAccountUtxos()
      } yield {
        // +- fee rate because signatures could vary in size
        (expectedBalance === balance +- FeeRate.currencyUnit) &&
        (expectedAddresses == addresses.size) &&
        (expectedUtxos == utxos.size)
      }
    }

    //default wallet utxos are 3BTC, 2BTC, 1BTC
    //our coin selection algorithm seems to be selecting
    //the 3BTC utxo to spend, so we should have
    //confirmed = 2BTC + 1BTC
    //unconfirmed = 3 BTC - TestAmount - TestFees
    val condition1 = () => {
      condition(
        expectedBalance = 6.bitcoin - TestAmount - TestFees,
        expectedUtxos = 3,
        expectedAddresses = 7
      )
    }

    //this is just sending TestAmount back to us
    //so everything should stay the same as above
    //expected we should have received TestAmount back
    //and have 1 more address/utxo
    val condition2 = { () =>
      condition(
        expectedBalance = (6.bitcoin - TestAmount - TestFees) + TestAmount,
        expectedUtxos = 4,
        expectedAddresses = 8
      )
    }

    for {
      // send
      addr <- bitcoind.getNewAddress
      _ <- wallet.sendToAddress(addr, TestAmount, Some(FeeRate))

      _ <- wallet.getConfirmedBalance()
      _ <- wallet.getUnconfirmedBalance()
      _ <- wallet.getBalance()
      _ <-
        bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
      _ <- NodeTestUtil.awaitSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)
      _ <- AsyncUtil.awaitConditionF(condition1, maxTries = 100) //10 seconds
      // receive
      address <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(address, TestAmount)
      expectedTx <- bitcoind.getRawTransactionRaw(txId)

      _ <-
        bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
      _ <- NodeTestUtil.awaitSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFilterHeadersSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)
      _ <- TestAsyncUtil.awaitConditionF(condition2)
      // assert we got the full tx with witness data
      txs <- wallet.listTransactions()
    } yield assert(txs.exists(_.transaction == expectedTx))
  }

  it must "watch an arbitrary SPK" in { param =>
    val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param

    def generateBlock() =
      for {
        _ <-
          bitcoind.getNewAddress
            .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)
      } yield ()

    val pk1 = ECPublicKey.freshPublicKey
    val pk2 = ECPublicKey.freshPublicKey
    val spk = MultiSignatureScriptPubKey(2, Vector(pk1, pk2))
    val sats = TestAmount
    val output = TransactionOutput(sats, spk)

    for {
      // start watching
      _ <- wallet.watchScriptPubKey(spk)

      // send
      txSent <- wallet.sendToOutputs(Vector(output), FeeRate)
      _ <- node.broadcastTransaction(txSent)

      // confirm
      _ <- generateBlock()
      _ <- generateBlock()
      _ <- generateBlock()

      // verify
      txs <- wallet.listTransactions()
    } yield assert(txs.exists(_.txIdBE == txSent.txIdBE))
  }

  it must "rescan information about received payments" in { param =>
    val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param

    implicit val wc = wallet.walletConfig

    for {
      addresses <- wallet.listAddresses()
      utxos <- wallet.listDefaultAccountUtxos()
      _ = assert(addresses.size == 6)
      _ = assert(utxos.size == 3)

      address <- wallet.getNewAddress()
      _ <-
        bitcoind
          .sendToAddress(address, TestAmount)

      addresses <- wallet.listAddresses()
      utxos <- wallet.listDefaultAccountUtxos()
      _ = assert(addresses.size == 7)
      _ = assert(utxos.size == 3)
      _ <-
        bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
      _ <- NodeTestUtil.awaitSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

      preRescanAddresses <- wallet.listAddresses().map(_.toSet)
      preRescanUtxos <- wallet.listDefaultAccountUtxos().map(_.toSet)

      // rescan a non empty wallet
      _ <- wallet.clearAllUtxos()
      addresses <- wallet.listAddresses()
      utxos <- wallet.listDefaultAccountUtxos()
      spkDao = ScriptPubKeyDAO()
      spkDbs <- spkDao.findAll()
      _ = assert(spkDbs.nonEmpty)
      _ = assert(addresses.nonEmpty)
      _ = assert(utxos.isEmpty)

      rescan <- wallet.isRescanning()
      _ = assert(!rescan)
      _ <- wallet.fullRescanNeutrinoWallet(addressBatchSize = 10)

      _ <- AsyncUtil.awaitConditionF(() => wallet.isRescanning().map(!_))
      balance <- wallet.getBalance()
      addresses <- wallet.listAddresses()
      utxos <- wallet.listDefaultAccountUtxos()
      spks = utxos.map(_.output.scriptPubKey)
      _ = assert(
        addresses.map(_.address.toString).sorted == preRescanAddresses.toVector
          .map(_.address.toString)
          .sorted)
      _ = assert(
        utxos.map(_.outPoint.toString).sorted == preRescanUtxos.toVector
          .map(_.outPoint.toString)
          .sorted)
      _ = assert(balance == BitcoinSWalletTest.expectedDefaultAmt + TestAmount)
      _ = assert(addresses.size == 7)
      _ = assert(utxos.size == 4)
      _ = assert(spks.forall(spk => addresses.exists(_.scriptPubKey == spk)))

      // rescan an empty wallet with the batch size > than number of addresses
      _ <- wallet.clearAllUtxos()
      addressDao = AddressDAO()
      addresses <- wallet.listAddresses()
      _ <- Future.sequence(addresses.map(addressDao.delete))
      utxos <- wallet.listDefaultAccountUtxos()
      addresses <- wallet.listAddresses()
      spkDbs <- spkDao.findAll()
      _ = assert(spkDbs.isEmpty)
      _ = assert(addresses.isEmpty)
      _ = assert(utxos.isEmpty)

      rescan <- wallet.isRescanning()
      _ = assert(!rescan)
      _ <- wallet.fullRescanNeutrinoWallet(addressBatchSize = 10)

      _ <- AsyncUtil.awaitConditionF(() => wallet.isRescanning().map(!_))
      balance <- wallet.getBalance()
      addresses <- wallet.listAddresses()
      utxos <- wallet.listDefaultAccountUtxos()
      spks = utxos.map(_.output.scriptPubKey)
      _ = assert(
        utxos.map(_.outPoint.toString).sorted == preRescanUtxos.toVector
          .map(_.outPoint.toString)
          .sorted)
      _ = assert(balance == BitcoinSWalletTest.expectedDefaultAmt + TestAmount)
      _ = assert(addresses.size == 220)
      _ = assert(utxos.size == 4)
      _ = assert(spks.forall(spk => addresses.exists(_.scriptPubKey == spk)))

      // rescan an empty wallet with the batch size < than number of addresses
      _ <- wallet.clearAllUtxos()
      addresses <- wallet.listAddresses()
      _ <- Future.sequence(addresses.map(addressDao.delete))
      utxos <- wallet.listDefaultAccountUtxos()
      addresses <- wallet.listAddresses()
      spkDbs <- spkDao.findAll()
      _ = assert(spkDbs.isEmpty)
      _ = assert(addresses.isEmpty)
      _ = assert(utxos.isEmpty)

      rescan <- wallet.isRescanning()
      _ = assert(!rescan)
      _ <- wallet.fullRescanNeutrinoWallet(addressBatchSize = 1)

      _ <- AsyncUtil.awaitConditionF(() => wallet.isRescanning().map(!_))
      balance <- wallet.getBalance()
      addresses <- wallet.listAddresses()
      utxos <- wallet.listDefaultAccountUtxos()
      spks = utxos.map(_.output.scriptPubKey)
      _ = assert(
        utxos.map(_.outPoint.toString).sorted == preRescanUtxos.toVector
          .map(_.outPoint.toString)
          .sorted)
      _ = assert(utxos.size == 4)
      _ = assert(addresses.size == 208)
      _ = assert(balance == BitcoinSWalletTest.expectedDefaultAmt + TestAmount)
      _ = assert(spks.forall(spk => addresses.exists(_.scriptPubKey == spk)))

    } yield succeed
  }

  it must "receive funds while the node is offline when we restart" in {
    param =>
      val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param

      val initBalanceF = wallet.getBalance()
      val receivedAddrF = wallet.getNewAddress()
      val bitcoindAddrF = bitcoind.getNewAddress
      val sendAmt = Bitcoins.one
      //stop the node to take us offline
      val stopF = node.stop()
      for {
        initBalance <- initBalanceF
        receiveAddr <- receivedAddrF
        bitcoindAddr <- bitcoindAddrF
        stoppedNode <- stopF
        //send money and generate a block to confirm the funds while we are offline
        _ <- bitcoind.sendToAddress(receiveAddr, sendAmt)
        //generate a block to confirm the tx
        _ <- bitcoind.generateToAddress(1, bitcoindAddr)
        //restart the node now that we have received funds
        startedNode <- stoppedNode.start()
        _ <- startedNode.sync()
        _ <- NodeTestUtil.awaitCompactFiltersSync(node = node, rpc = bitcoind)
        _ <- AsyncUtil.retryUntilSatisfiedF(() => {
          for {
            balance <- wallet.getBalance()
          } yield {
            balance == initBalance + sendAmt
          }
        })
        balance <- wallet.getBalance()
      } yield {
        assert(balance > initBalance)
      }
  }

  it must "recognize funds were spent while we were offline" in { param =>
    //useful test for the case where we are in a DLC
    //and the counterparty broadcasts the funding tx or a CET
    val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param
    val initBalanceF = wallet.getBalance()
    val bitcoindAddrF = bitcoind.getNewAddress
    val sendAmt = Bitcoins.one

    //stop the node to take us offline
    val stopF = node.stop()
    for {
      initBalance <- initBalanceF
      bitcoindAddr <- bitcoindAddrF
      stoppedNode <- stopF

      //create a transaction that spends to bitcoind with our wallet
      tx <- wallet.sendToAddress(bitcoindAddr, sendAmt, SatoshisPerByte.one)
      //broadcast tx
      _ <- bitcoind.sendRawTransaction(tx)
      _ <- bitcoind.generateToAddress(6, bitcoindAddr)

      //bring node back online
      startedNode <- stoppedNode.start()
      _ <- startedNode.sync()
      _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)
      balanceAfterSpend <- wallet.getBalance()
    } yield {
      assert(balanceAfterSpend < initBalance)
    }
  }
}
