package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.script.EmptyScriptSignature
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.rpc.BitcoindException.InvalidAddressOrKey
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.RandomFeeProvider
import org.bitcoins.testkit.wallet._
import org.scalatest.FutureOutcome

class WalletIntegrationTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)

  behavior of "Wallet - integration test"

  // the amount we're receiving from bitcoind
  val valueFromBitcoind: Bitcoins = Bitcoins.one

  // the amount we're sending to bitcoind
  val valueToBitcoind: Bitcoins = Bitcoins(0.5)

  it should ("create an address, receive funds to it from bitcoind, import the"
    + " UTXO and construct a valid, signed transaction that's"
    + " broadcast and confirmed by bitcoind") in { walletWithBitcoind =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = walletWithBitcoind

    for {
      addr <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      tx <- bitcoind.getRawTransactionRaw(txId)

      // before processing TX, wallet should be completely empty
      _ <- wallet.listUtxos().map(utxos => assert(utxos.isEmpty))
      _ <- wallet.getBalance().map(confirmed => assert(confirmed == 0.bitcoin))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == 0.bitcoin))

      // after this, tx is unconfirmed in wallet
      _ <- wallet.processTransaction(tx, None)

      // we should now have one UTXO in the wallet
      // it should not be confirmed
      utxosPostAdd <- wallet.listUtxos()
      _ = assert(utxosPostAdd.length == 1)
      _ <-
        wallet
          .getConfirmedBalance()
          .map(confirmed => assert(confirmed == 0.bitcoin))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == valueFromBitcoind))
      incomingTx <- wallet.incomingTxDAO.findByTxId(tx.txIdBE)
      _ = assert(incomingTx.isDefined)
      _ = assert(incomingTx.get.incomingAmount == valueFromBitcoind)

      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
      rawTx <- bitcoind.getRawTransaction(txId)

      // after this, tx should be confirmed
      _ <- wallet.processTransaction(tx, rawTx.blockhash)
      _ <-
        wallet
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
        wallet.sendToAddress(_, valueToBitcoind, None)
      }

      feeRate =
        wallet.feeRateApi.asInstanceOf[RandomFeeProvider].lastFeeRate.get

      txid <- bitcoind.sendRawTransaction(signedTx)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))
      tx <- bitcoind.getRawTransaction(txid)

      utxos <- wallet.listUtxos()
      _ = utxos match {
        case utxo +: Vector() =>
          assert(utxo.privKeyPath.chain.chainType == HDChainType.Change)
        case other => fail(s"Found ${other.length} utxos!")
      }

      outgoingTx <- wallet.outgoingTxDAO.findByTxId(txid)
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
        outgoingTx.get.actualFee + outgoingTx.get.sentAmount == outgoingTx.get.inputAmount - utxos.head.output.value)

      // change UTXO should be smaller than what we had, but still have money in it
      assert(balancePostSend > 0.sats)
      assert(balancePostSend < valueFromBitcoind)
      assert(
        WalletTestUtil.isCloseEnough(balancePostSend,
                                     valueFromBitcoind - valueToBitcoind,
                                     delta = outgoingTx.get.actualFee))
      assert(tx.confirmations.exists(_ > 0))
    }
  }

  it should "correctly bump fees with RBF" in { walletWithBitcoind =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = walletWithBitcoind

    for {
      // Fund wallet
      addr <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      rawTx <- bitcoind.getRawTransaction(txId)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
      _ <- wallet.processTransaction(rawTx.hex, rawTx.blockhash)

      // Verify we funded the wallet
      balance <- wallet.getBalance()
      _ = assert(balance == valueFromBitcoind)

      // Create first tx
      tx1 <- bitcoind.getNewAddress.flatMap {
        wallet.sendToAddress(_, valueToBitcoind, SatoshisPerVirtualByte.one)
      }
      _ <- bitcoind.sendRawTransaction(tx1)

      // Verify we sent to bitcoind
      bitcoindBal1 <- bitcoind.getUnconfirmedBalance
      _ = assert(bitcoindBal1 == valueToBitcoind)

      walletBal1 <- wallet.getBalance()

      // Create replacement tx
      newFeeRate = SatoshisPerVirtualByte.fromLong(20)
      replacementTx <- wallet.bumpFeeRBF(tx1.txIdBE, newFeeRate)

      // Check tx being replaced exists
      tx1Info <- bitcoind.getRawTransaction(tx1.txIdBE)
      _ = assert(tx1Info.blockhash.isEmpty)

      _ <- bitcoind.sendRawTransaction(replacementTx)

      // After replacement tx is broadcast, old tx should be gone
      _ <- recoverToSucceededIf[InvalidAddressOrKey](
        bitcoind.getRawTransaction(tx1.txIdBE))

      // Check we didn't send extra to bitcoind
      bitcoindBal2 <- bitcoind.getUnconfirmedBalance
      _ = assert(bitcoindBal2 == valueToBitcoind)

      // Check we paid more in fees
      walletBal2 <- wallet.getBalance()
      _ = assert(walletBal1 > walletBal2)

      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))

      replacementInfo <- bitcoind.getRawTransaction(replacementTx.txIdBE)

      utxos <- wallet.spendingInfoDAO.findOutputsBeingSpent(replacementTx)
    } yield {
      assert(utxos.forall(_.spendingTxIdOpt.contains(replacementTx.txIdBE)))
      // Check correct one was confirmed
      assert(replacementInfo.blockhash.isDefined)
    }
  }

  it should "fail to RBF a confirmed transaction" in { walletWithBitcoind =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = walletWithBitcoind

    for {
      // Fund wallet
      addr <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
      rawTx <- bitcoind.getRawTransaction(txId)
      _ <- wallet.processTransaction(rawTx.hex, rawTx.blockhash)

      // Verify we funded the wallet
      balance <- wallet.getBalance()
      _ = assert(balance == valueFromBitcoind)

      // Create rbf tx
      rbf <- bitcoind.getNewAddress.flatMap {
        wallet.sendToAddress(_, valueToBitcoind, SatoshisPerVirtualByte.one)
      }
      _ <- bitcoind.sendRawTransaction(rbf)

      // Confirm transaction
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))
      rawTx1 <- bitcoind.getRawTransaction(rbf.txIdBE)
      _ = require(rawTx1.blockhash.isDefined)
      _ <- wallet.processTransaction(rbf, rawTx1.blockhash)

      // fail to RBF confirmed tx
      res <- recoverToSucceededIf[IllegalArgumentException] {
        wallet.bumpFeeRBF(rbf.txIdBE, SatoshisPerVirtualByte.fromLong(20))
      }
    } yield res
  }

  it should "correctly bump fees with CPFP" in { walletWithBitcoind =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = walletWithBitcoind

    for {
      // Fund wallet
      addr <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      rawTx <- bitcoind.getRawTransaction(txId)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
      _ <- wallet.processTransaction(rawTx.hex, rawTx.blockhash)

      // Verify we funded the wallet
      balance <- wallet.getBalance()
      _ = assert(balance == valueFromBitcoind)

      // Create parent tx
      parentTx <- bitcoind.getNewAddress.flatMap {
        wallet.sendToAddress(_, valueToBitcoind, None)
      }
      _ <- bitcoind.sendRawTransaction(parentTx)

      // Verify we sent to bitcoind
      bitcoindBal1 <- bitcoind.getUnconfirmedBalance
      _ = assert(bitcoindBal1 == valueToBitcoind)

      walletBal1 <- wallet.getBalance()

      // Create child tx
      childFeeRate <- wallet.feeRateApi.getFeeRate
      childTx <- wallet.bumpFeeCPFP(parentTx.txIdBE, childFeeRate)
      _ <- bitcoind.sendRawTransaction(childTx)

      // Check we didn't send again to bitcoind
      bitcoindBal2 <- bitcoind.getUnconfirmedBalance
      _ = assert(bitcoindBal2 == valueToBitcoind)

      walletBal2 <- wallet.getBalance()
    } yield assert(walletBal1 > walletBal2)
  }

  it should "correctly handle spending coinbase utxos" in {
    walletWithBitcoind =>
      val WalletWithBitcoindRpc(wallet, bitcoind) = walletWithBitcoind

      val amountToSend = Bitcoins(49.99)

      for {
        // Mine to wallet
        addr <- wallet.getNewAddress()
        hash <- bitcoind.generateToAddress(1, addr).map(_.head)
        block <- bitcoind.getBlockRaw(hash)

        // Assert we mined to our address
        coinbaseTx = block.transactions.head
        _ = assert(
          coinbaseTx.outputs.exists(_.scriptPubKey == addr.scriptPubKey))

        _ <- wallet.processBlock(block)

        // Verify we funded the wallet
        allUtxos <- wallet.spendingInfoDAO.findAllSpendingInfos()
        _ = assert(allUtxos.size == 1)
        utxos <- wallet.listUtxos(TxoState.ImmatureCoinbase)
        _ = assert(utxos.size == 1)

        bitcoindAddr <- bitcoind.getNewAddress

        // Attempt to spend utxo
        _ <- recoverToSucceededIf[RuntimeException](
          wallet.sendToAddress(bitcoindAddr, valueToBitcoind, None))

        spendingTx = {
          val inputs = utxos.map { db =>
            TransactionInput(db.outPoint, EmptyScriptSignature, UInt32.zero)
          }
          val outputs =
            Vector(TransactionOutput(amountToSend, bitcoindAddr.scriptPubKey))
          BaseTransaction(Int32.two, inputs, outputs, UInt32.zero)
        }

        _ <- recoverToSucceededIf[RuntimeException](
          wallet.processTransaction(spendingTx, None))

        // Make coinbase mature
        _ <- bitcoind.generateToAddress(101, bitcoindAddr)
        _ <- wallet.updateUtxoPendingStates()

        // Create valid spending tx
        psbt = PSBT.fromUnsignedTx(spendingTx)
        signedPSBT <- wallet.signPSBT(psbt)
        signedTx = signedPSBT.finalizePSBT
          .flatMap(_.extractTransactionAndValidate)
          .get

        // Process tx, validate correctly moved to
        _ <- wallet.processTransaction(signedTx, None)
        newCoinbaseUtxos <- wallet.listUtxos(TxoState.ImmatureCoinbase)
        _ = assert(newCoinbaseUtxos.isEmpty)
        spentUtxos <- wallet.listUtxos(TxoState.BroadcastSpent)
        _ = assert(spentUtxos.size == 1)

        // Assert spending tx valid to bitcoind
        oldBalance <- bitcoind.getBalance
        _ = assert(oldBalance == Satoshis(510000000000L))

        _ <- bitcoind.sendRawTransaction(signedTx)
        _ <- bitcoind.generateToAddress(1, bitcoindAddr)

        newBalance <- bitcoind.getBalance
      } yield assert(newBalance == oldBalance + amountToSend + Bitcoins(50))
  }
}
