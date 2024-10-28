package org.bitcoins.wallet

import org.bitcoins.core.currency.*
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.number.*
import org.bitcoins.core.protocol.script.EmptyScriptSignature
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.feeprovider.RandomFeeProvider
import org.bitcoins.commons.rpc.BitcoindException.InvalidAddressOrKey
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTestCachedBitcoindNewest,
  WalletTestUtil,
  WalletWithBitcoindRpc
}
import org.bitcoins.wallet.models.{
  IncomingTransactionDAO,
  OutgoingTransactionDAO
}
import org.bitcoins.wallet.util.WalletUtil
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

class WalletIntegrationTest extends BitcoinSWalletTestCachedBitcoindNewest {

  override type FixtureParam = WalletWithBitcoindRpc

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withNewWalletAndBitcoindCached(
        test = test,
        bitcoind = bitcoind
      )(getFreshWalletAppConfig)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  behavior of "Wallet - integration test"

  // the amount we're receiving from bitcoind
  val valueFromBitcoind: Bitcoins = Bitcoins.one

  // the amount we're sending to bitcoind
  val valueToBitcoind: Bitcoins = Bitcoins(0.5)

  it should ("create an address, receive funds to it from bitcoind, import the"
    + " UTXO and construct a valid, signed transaction that's"
    + " broadcast and confirmed by bitcoind") in { walletWithBitcoind =>
    val wallet = walletWithBitcoind.wallet
    val bitcoind = walletWithBitcoind.bitcoind
    val walletConfig = walletWithBitcoind.walletConfig

    val incomingDAO = IncomingTransactionDAO()(system.dispatcher, walletConfig)
    val outgoingDAO = OutgoingTransactionDAO()(system.dispatcher, walletConfig)
    for {
      addr <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      tx <- bitcoind.getRawTransactionRaw(txId)

      // before processing TX, wallet should be completely empty
      _ <- wallet.utxoHandling.listUtxos().map(utxos => assert(utxos.isEmpty))
      _ <- wallet.getBalance().map(confirmed => assert(confirmed == 0.bitcoin))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == 0.bitcoin))

      // after this, tx is unconfirmed in wallet
      _ <- wallet.transactionProcessing.processTransaction(tx, None)

      // we should now have one UTXO in the wallet
      // it should not be confirmed
      utxosPostAdd <- wallet.utxoHandling.listUtxos()
      _ = assert(utxosPostAdd.length == 1)
      _ <-
        wallet
          .getConfirmedBalance()
          .map(confirmed => assert(confirmed == 0.bitcoin))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == valueFromBitcoind))
      incomingTx <- incomingDAO.findByTxId(tx.txIdBE)
      _ = assert(incomingTx.isDefined)
      _ = assert(incomingTx.get.incomingAmount == valueFromBitcoind)

      _ <- bitcoind.generate(6)
      rawTx <- bitcoind.getRawTransaction(txId)

      // after this, tx should be confirmed
      blockHashWithConfsOpt <- WalletUtil.getBlockHashWithConfs(bitcoind,
                                                                rawTx.blockhash)
      _ <- wallet.transactionProcessing.processTransaction(
        tx,
        blockHashWithConfsOpt)
      _ <-
        wallet.utxoHandling
          .listUtxos()
          .map { utxos =>
            // we want to make sure no new utxos were added,
            // i.e. that we only modified an existing one
            assert(utxos.length == utxosPostAdd.length)
          }

      _ <-
        wallet
          .getBalance()
          .map(balance => assert(balance == valueFromBitcoind))
      _ <-
        wallet
          .getConfirmedBalance()
          .map(confirmed => assert(confirmed == valueFromBitcoind))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == 0.satoshis))

      signedTx <- bitcoind.getNewAddress.flatMap {
        wallet.sendFundsHandling.sendToAddress(_, valueToBitcoind, None)
      }

      feeRate =
        wallet.feeRateApi.asInstanceOf[RandomFeeProvider].lastFeeRate.get

      txid <- bitcoind.sendRawTransaction(signedTx)
      _ <- bitcoind.generate(1)
      tx <- bitcoind.getRawTransaction(txid)

      utxos <- wallet.utxoHandling.listUtxos()
      _ = utxos match {
        case utxo +: Vector() =>
          assert(utxo.privKeyPath.chain.chainType == HDChainType.Change)
        case other => fail(s"Found ${other.length} utxos!")
      }

      outgoingTx <- outgoingDAO.findByTxId(txid)
      _ = assert(outgoingTx.isDefined)
      _ = assert(outgoingTx.get.inputAmount == valueFromBitcoind)
      _ = assert(outgoingTx.get.sentAmount == valueToBitcoind)
      _ = assert(
        WalletTestUtil.isFeeRateCloseEnough(outgoingTx.get),
        s"Actual fee=${outgoingTx.get.feeRate} expectedFeeRate=$feeRate"
      )
      balancePostSend <- wallet.getBalance()
    } yield {
      assert(outgoingTx.get.expectedFee == feeRate.calc(signedTx))
      assert(
        outgoingTx.get.expectedFee === outgoingTx.get.actualFee +- feeRate.currencyUnit, // could be off by one due to rounding
        s"Expected fee rate not close enough, $feeRate"
      )
      // Safe to use utxos.head because we've already asserted that we only have our change output
      assert(
        outgoingTx.get.actualFee + outgoingTx.get.sentAmount == outgoingTx.get.inputAmount - utxos.head.output.value
      )

      // change UTXO should be smaller than what we had, but still have money in it
      assert(balancePostSend > 0.sats)
      assert(balancePostSend < valueFromBitcoind)
      assert(
        WalletTestUtil.isCloseEnough(
          balancePostSend,
          valueFromBitcoind - valueToBitcoind,
          delta = outgoingTx.get.actualFee
        )
      )
      assert(tx.confirmations.exists(_ > 0))
    }
  }

  it should "correctly bump fees with RBF" in { walletWithBitcoind =>
    val wallet = walletWithBitcoind.wallet
    val bitcoind = walletWithBitcoind.bitcoind
    for {
      // Fund wallet
      addr <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      rawTx <- bitcoind.getRawTransaction(txId)
      _ <- bitcoind.generate(6)
      blockHashWithConfsOpt <- WalletUtil.getBlockHashWithConfs(bitcoind,
                                                                rawTx.blockhash)
      _ <- wallet.transactionProcessing.processTransaction(
        rawTx.hex,
        blockHashWithConfsOpt)

      // Verify we funded the wallet
      balance <- wallet.getBalance()
      _ = assert(balance == valueFromBitcoind)

      // Create first tx
      tx1 <- bitcoind.getNewAddress.flatMap {
        wallet.sendFundsHandling.sendToAddress(_,
                                               valueToBitcoind,
                                               SatoshisPerVirtualByte.one)
      }
      _ <- bitcoind.sendRawTransaction(tx1)

      // Verify we sent to bitcoind
      bitcoindBal1 <- bitcoind.getUnconfirmedBalance
      _ = assert(bitcoindBal1 == valueToBitcoind)

      walletBal1 <- wallet.getBalance()

      // Create replacement tx
      newFeeRate = SatoshisPerVirtualByte.fromLong(20)
      replacementTx <- wallet.sendFundsHandling.bumpFeeRBF(tx1.txIdBE,
                                                           newFeeRate)

      // Check tx being replaced exists
      tx1Info <- bitcoind.getRawTransaction(tx1.txIdBE)
      _ = assert(tx1Info.blockhash.isEmpty)

      _ <- bitcoind.sendRawTransaction(replacementTx)

      // After replacement tx is broadcast, old tx should be gone
      _ <- recoverToSucceededIf[InvalidAddressOrKey](
        bitcoind.getRawTransaction(tx1.txIdBE)
      )

      // Check we didn't send extra to bitcoind
      bitcoindBal2 <- bitcoind.getUnconfirmedBalance
      _ = assert(bitcoindBal2 == valueToBitcoind)

      // Check we paid more in fees
      walletBal2 <- wallet.getBalance()
      _ = assert(walletBal1 > walletBal2)

      _ <- bitcoind.generate(6)

      replacementInfo <- bitcoind.getRawTransaction(replacementTx.txIdBE)

      utxos <- wallet.utxoHandling.findOutputsBeingSpent(replacementTx)
    } yield {
      assert(utxos.forall(_.spendingTxIdOpt.contains(replacementTx.txIdBE)))
      // Check correct one was confirmed
      assert(replacementInfo.blockhash.isDefined)
    }
  }

  it should "fail to RBF a confirmed transaction" in { walletWithBitcoind =>
    val wallet = walletWithBitcoind.wallet
    val bitcoind = walletWithBitcoind.bitcoind

    for {
      // Fund wallet
      addr <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      _ <- bitcoind.generate(6)
      rawTx <- bitcoind.getRawTransaction(txId)
      blockHashWithConfsOpt <- WalletUtil.getBlockHashWithConfs(bitcoind,
                                                                rawTx.blockhash)
      _ <- wallet.transactionProcessing.processTransaction(
        rawTx.hex,
        blockHashWithConfsOpt)

      // Verify we funded the wallet
      balance <- wallet.getBalance()
      _ = assert(balance == valueFromBitcoind)

      // Create rbf tx
      rbf <- bitcoind.getNewAddress.flatMap {
        wallet.sendFundsHandling.sendToAddress(_,
                                               valueToBitcoind,
                                               SatoshisPerVirtualByte.one)
      }
      _ <- bitcoind.sendRawTransaction(rbf)

      // Confirm transaction
      _ <- bitcoind.generate(1)
      rawTx1 <- bitcoind.getRawTransaction(rbf.txIdBE)
      _ = require(rawTx1.blockhash.isDefined)
      blockHashWithConfsOpt <- WalletUtil.getBlockHashWithConfs(bitcoind,
                                                                rawTx.blockhash)
      _ <- wallet.transactionProcessing.processTransaction(
        rbf,
        blockHashWithConfsOpt)

      // fail to RBF confirmed tx
      res <- recoverToSucceededIf[IllegalArgumentException] {
        wallet.sendFundsHandling.bumpFeeRBF(rbf.txIdBE,
                                            SatoshisPerVirtualByte.fromLong(20))
      }
    } yield res
  }

  it should "correctly bump fees with CPFP" in { walletWithBitcoind =>
    val wallet = walletWithBitcoind.wallet
    val bitcoind = walletWithBitcoind.bitcoind

    for {
      // Fund wallet
      addr <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      rawTx <- bitcoind.getRawTransaction(txId)
      _ <- bitcoind.generate(6)
      blockHashWithConfsOpt <- WalletUtil.getBlockHashWithConfs(bitcoind,
                                                                rawTx.blockhash)
      _ <- wallet.transactionProcessing.processTransaction(
        rawTx.hex,
        blockHashWithConfsOpt)

      // Verify we funded the wallet
      balance <- wallet.getBalance()
      _ = assert(balance == valueFromBitcoind)

      // Create parent tx
      parentTx <- bitcoind.getNewAddress.flatMap {
        wallet.sendFundsHandling.sendToAddress(_, valueToBitcoind, None)
      }
      _ <- bitcoind.sendRawTransaction(parentTx)

      // Verify we sent to bitcoind
      bitcoindBal1 <- bitcoind.getUnconfirmedBalance
      _ = assert(bitcoindBal1 == valueToBitcoind)

      walletBal1 <- wallet.getBalance()

      // Create child tx
      childFeeRate <- wallet.feeRateApi.getFeeRate()
      childTx <- wallet.sendFundsHandling.bumpFeeCPFP(parentTx.txIdBE,
                                                      childFeeRate)
      _ <- bitcoind.sendRawTransaction(childTx)

      // Check we didn't send again to bitcoind
      bitcoindBal2 <- bitcoind.getUnconfirmedBalance
      _ = assert(bitcoindBal2 == valueToBitcoind)

      walletBal2 <- wallet.getBalance()
    } yield assert(walletBal1 > walletBal2)
  }

  it should "correctly handle spending coinbase utxos" in {
    walletWithBitcoind =>
      val wallet = walletWithBitcoind.wallet
      val bitcoind = walletWithBitcoind.bitcoind

      // Makes fee rate for tx ~5 sat/vbyte
      val amountToSend = Bitcoins(49.99999000)

      for {
        // Mine to wallet
        addr <- wallet.getNewAddress()
        hash <- bitcoind.generateToAddress(1, addr).map(_.head)
        block <- bitcoind.getBlockRaw(hash)

        // Assert we mined to our address
        coinbaseTx = block.transactions.head
        _ = assert(
          coinbaseTx.outputs.exists(_.scriptPubKey == addr.scriptPubKey)
        )

        _ <- wallet.transactionProcessing.processBlock(block)

        // Verify we funded the wallet
        allUtxos <- wallet.utxoHandling.listUtxos()
        _ = assert(allUtxos.size == 1)
        utxos <- wallet.utxoHandling.listUtxos(TxoState.ImmatureCoinbase)
        _ = assert(utxos.size == 1)

        bitcoindAddr <- bitcoind.getNewAddress

        // Attempt to spend utxo
        _ <- recoverToSucceededIf[RuntimeException](
          wallet.sendFundsHandling.sendToAddress(bitcoindAddr,
                                                 valueToBitcoind,
                                                 None)
        )

        spendingTx = {
          val inputs = utxos.map { db =>
            TransactionInput(db.outPoint, EmptyScriptSignature, UInt32.zero)
          }
          val outputs =
            Vector(TransactionOutput(amountToSend, bitcoindAddr.scriptPubKey))
          BaseTransaction(Int32.two, inputs, outputs, UInt32.zero)
        }

        _ <- recoverToSucceededIf[RuntimeException](
          wallet.transactionProcessing.processTransaction(spendingTx, None)
        )

        // Make coinbase mature
        _ <- bitcoind.generateToAddress(101, bitcoindAddr)
        _ <- wallet.utxoHandling.updateUtxoPendingStates()

        // Create valid spending tx
        psbt = PSBT.fromUnsignedTx(spendingTx)
        signedPSBT <- wallet.sendFundsHandling.signPSBT(psbt)
        signedTx = signedPSBT.finalizePSBT
          .flatMap(_.extractTransactionAndValidate)
          .get

        // Process tx, validate correctly moved to
        _ <- wallet.transactionProcessing.processTransaction(signedTx, None)
        newCoinbaseUtxos <- wallet.utxoHandling.listUtxos(
          TxoState.ImmatureCoinbase)
        _ = assert(newCoinbaseUtxos.isEmpty)
        spentUtxos <- wallet.utxoHandling.listUtxos(TxoState.BroadcastSpent)
        _ = assert(spentUtxos.size == 1)

        // Assert spending tx valid to bitcoind
        oldBalance <- bitcoind.getBalance

        _ <- bitcoind.sendRawTransaction(signedTx)
        _ <- bitcoind.generateToAddress(1, bitcoindAddr)

        newBalance <- bitcoind.getBalance
      } yield assert(newBalance == oldBalance + amountToSend + Bitcoins(50))
  }
}
