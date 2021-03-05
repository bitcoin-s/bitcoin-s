package org.bitcoins.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.core.wallet.utxo.TxoState._
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletWithBitcoind,
  WalletWithBitcoindRpc
}
import org.scalatest.FutureOutcome

class UTXOLifeCycleTest extends BitcoinSWalletTest {

  behavior of "Wallet Txo States"

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWalletAndBitcoind(test, getBIP39PasswordOpt())
  }

  it should "track a utxo state change to pending spent" in { param =>
    val WalletWithBitcoindRpc(wallet, _) = param

    for {
      oldTransactions <- wallet.listTransactions()
      tx <- wallet.sendToAddress(testAddr, Satoshis(3000), None)

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      newTransactions <- wallet.listTransactions()
    } yield {
      assert(updatedCoins.forall(_.state == TxoState.PendingConfirmationsSpent))
      assert(updatedCoins.forall(_.spendingTxIdOpt.contains(tx.txIdBE)))
      assert(!oldTransactions.map(_.transaction).contains(tx))
      assert(newTransactions.map(_.transaction).contains(tx))
    }
  }

  it should "track a utxo state change to confirmed spent" in { param =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = param

    for {
      oldTransactions <- wallet.listTransactions()
      tx <- wallet.sendToAddress(testAddr, Satoshis(3000), None)

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      newTransactions <- wallet.listTransactions()
      _ = assert(updatedCoins.forall(_.state == PendingConfirmationsSpent))
      _ = assert(!oldTransactions.map(_.transaction).contains(tx))
      _ = assert(newTransactions.map(_.transaction).contains(tx))

      // Give tx a fake hash so it can appear as it's in a block
      hash <- bitcoind.getBestBlockHash
      _ <- wallet.processTransaction(tx, Some(hash))

      pendingCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      _ <- wallet.updateUtxoPendingStates()
      _ = assert(pendingCoins.forall(_.state == PendingConfirmationsSpent))

      // Put confirmations on top of the tx's block
      _ <- bitcoind.getNewAddress.flatMap(
        bitcoind.generateToAddress(wallet.walletConfig.requiredConfirmations,
                                   _))
      // Need to call this to actually update the state, normally a node callback would do this
      _ <- wallet.updateUtxoPendingStates()
      confirmedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
    } yield {
      assert(confirmedCoins.forall(_.state == ConfirmedSpent))
      assert(confirmedCoins.forall(_.spendingTxIdOpt.contains(tx.txIdBE)))
    }
  }

  it should "handle an RBF transaction on unconfirmed coins" in { param =>
    val WalletWithBitcoindRpc(wallet, _) = param

    for {
      tx <- wallet.sendToAddress(testAddr,
                                 Satoshis(3000),
                                 Some(SatoshisPerByte.one))

      coins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      _ = assert(coins.forall(_.state == PendingConfirmationsSpent))
      _ = assert(coins.forall(_.spendingTxIdOpt.contains(tx.txIdBE)))

      rbf <- wallet.bumpFeeRBF(tx.txIdBE, SatoshisPerByte.fromLong(3))
      _ <- wallet.processTransaction(rbf, None)
      rbfCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(rbf)
    } yield {
      assert(rbfCoins.forall(_.state == PendingConfirmationsSpent))
      assert(rbfCoins.forall(_.spendingTxIdOpt.contains(rbf.txIdBE)))
    }
  }

  it should "handle attempting to spend an immature coinbase" in { param =>
    val WalletWithBitcoindRpc(wallet, _) = param

    for {
      tx <- wallet.sendToAddress(testAddr, Satoshis(3000), None)

      coins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)

      updatedCoins = coins.map(_.copyWithState(TxoState.ImmatureCoinbase))
      _ <- wallet.spendingInfoDAO.updateAllSpendingInfoDb(updatedCoins.toVector)

      // Create tx to spend immature coinbase utxos
      newTx = {
        val inputs = coins.map { db =>
          TransactionInput(db.outPoint, EmptyScriptSignature, UInt32.zero)
        }
        BaseTransaction(Int32.zero, inputs, Vector.empty, UInt32.zero)
      }

      res <- recoverToSucceededIf[RuntimeException](
        wallet.processTransaction(newTx, None))
    } yield res
  }

  it should "handle processing a new spending tx for a spent utxo" in { param =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = param

    for {
      oldTransactions <- wallet.listTransactions()
      tx <- wallet.sendToAddress(testAddr, Satoshis(3000), None)

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      newTransactions <- wallet.listTransactions()
      _ = assert(updatedCoins.forall(_.state == PendingConfirmationsSpent))
      _ = assert(!oldTransactions.map(_.transaction).contains(tx))
      _ = assert(newTransactions.map(_.transaction).contains(tx))

      // Give tx a fake hash so it can appear as it's in a block
      hash <- bitcoind.getBestBlockHash
      _ <- wallet.processTransaction(tx, Some(hash))

      pendingCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      _ <- wallet.updateUtxoPendingStates()
      _ = assert(pendingCoins.forall(_.state == PendingConfirmationsSpent))

      // Put confirmations on top of the tx's block
      _ <- bitcoind.getNewAddress.flatMap(
        bitcoind.generateToAddress(wallet.walletConfig.requiredConfirmations,
                                   _))
      // Need to call this to actually update the state, normally a node callback would do this
      _ <- wallet.updateUtxoPendingStates()
      confirmedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)

      // Assert tx is confirmed
      _ = assert(confirmedCoins.forall(_.state == ConfirmedSpent))
      _ = assert(confirmedCoins.forall(_.spendingTxIdOpt.contains(tx.txIdBE)))

      // Create tx to spend same utxos
      newTx = {
        val inputs = updatedCoins.map { db =>
          TransactionInput(db.outPoint, EmptyScriptSignature, UInt32.zero)
        }
        BaseTransaction(Int32.zero, inputs, Vector.empty, UInt32.zero)
      }

      res <- recoverToSucceededIf[RuntimeException](
        wallet.processTransaction(newTx, None))
    } yield res
  }

  it should "handle processing a new spending tx for a DNE utxo" in { param =>
    val WalletWithBitcoindRpc(wallet, _) = param

    for {
      tx <- wallet.sendToAddress(testAddr, Satoshis(3000), None)

      coins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)

      dneCoins = coins.map(_.copyWithState(TxoState.DoesNotExist))
      _ <- wallet.spendingInfoDAO.updateAllSpendingInfoDb(dneCoins.toVector)

      // Create tx to spend dne utxos
      newTx = {
        val inputs = coins.map { db =>
          TransactionInput(db.outPoint, EmptyScriptSignature, UInt32.zero)
        }
        BaseTransaction(Int32.zero, inputs, Vector.empty, UInt32.zero)
      }

      res <- recoverToSucceededIf[RuntimeException](
        wallet.processTransaction(newTx, None))
    } yield res
  }

  it should "track a utxo state change to pending received" in { param =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = param

    for {
      oldTransactions <- wallet.listTransactions()
      addr <- wallet.getNewAddress()

      txId <- bitcoind.sendToAddress(addr, Satoshis(3000))
      tx <- bitcoind.getRawTransactionRaw(txId)
      _ <- wallet.processOurTransaction(transaction = tx,
                                        feeRate = SatoshisPerByte(Satoshis(3)),
                                        inputAmount = Satoshis(4000),
                                        sentAmount = Satoshis(3000),
                                        blockHashOpt = None,
                                        newTags = Vector.empty)

      updatedCoin <-
        wallet.spendingInfoDAO.findByScriptPubKey(addr.scriptPubKey)
      newTransactions <- wallet.listTransactions()
    } yield {
      assert(
        updatedCoin.forall(_.state == TxoState.PendingConfirmationsReceived))
      assert(!oldTransactions.map(_.transaction).contains(tx))
      assert(newTransactions.map(_.transaction).contains(tx))
    }
  }

  it should "track a utxo state change to confirmed received" in { param =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = param

    for {
      oldTransactions <- wallet.listTransactions()
      addr <- wallet.getNewAddress()

      blockHash <- bitcoind.getBestBlockHash

      txId <- bitcoind.sendToAddress(addr, Satoshis(3000))
      tx <- bitcoind.getRawTransactionRaw(txId)
      _ <- wallet.processOurTransaction(
        transaction = tx,
        feeRate = SatoshisPerByte(Satoshis(3)),
        inputAmount = Satoshis(4000),
        sentAmount = Satoshis(3000),
        blockHashOpt = Some(blockHash), // give fake hash
        newTags = Vector.empty
      )

      updatedCoin <-
        wallet.spendingInfoDAO.findByScriptPubKey(addr.scriptPubKey)
      newTransactions <- wallet.listTransactions()
      _ = assert(updatedCoin.forall(_.state == PendingConfirmationsReceived))
      _ = assert(!oldTransactions.map(_.transaction).contains(tx))
      _ = assert(newTransactions.map(_.transaction).contains(tx))

      // Put confirmations on top of the tx's block
      _ <- bitcoind.getNewAddress.flatMap(
        bitcoind.generateToAddress(wallet.walletConfig.requiredConfirmations,
                                   _))
      // Need to call this to actually update the state, normally a node callback would do this
      _ <- wallet.updateUtxoPendingStates()
      confirmedCoins <-
        wallet.spendingInfoDAO.findByScriptPubKey(addr.scriptPubKey)
    } yield assert(confirmedCoins.forall(_.state == ConfirmedReceived))
  }

  it should "track a utxo state change to reserved" in { param =>
    val WalletWithBitcoindRpc(wallet, _) = param

    val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

    for {
      oldTransactions <- wallet.listTransactions()
      feeRate <- wallet.getFeeRate
      tx <- wallet.fundRawTransaction(Vector(dummyOutput),
                                      feeRate,
                                      fromTagOpt = None,
                                      markAsReserved = true)

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      reserved <- wallet.listUtxos(TxoState.Reserved)
      newTransactions <- wallet.listTransactions()
    } yield {
      assert(updatedCoins.forall(_.state == TxoState.Reserved))
      assert(updatedCoins.forall(reserved.contains))
      assert(updatedCoins.size == reserved.size)
      assert(!oldTransactions.map(_.transaction).contains(tx))
      assert(!newTransactions.map(_.transaction).contains(tx))
    }
  }

  it should "track a utxo state change to reserved and then to unreserved" in {
    param =>
      val WalletWithBitcoindRpc(wallet, _) = param

      val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

      for {
        oldTransactions <- wallet.listTransactions()
        feeRate <- wallet.getFeeRate
        tx <- wallet.fundRawTransaction(Vector(dummyOutput),
                                        feeRate,
                                        fromTagOpt = None,
                                        markAsReserved = true)

        reservedUtxos <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
        allReserved <- wallet.listUtxos(TxoState.Reserved)
        _ = assert(reservedUtxos.forall(_.state == TxoState.Reserved))
        _ = assert(reservedUtxos.forall(allReserved.contains))

        unreservedUtxos <- wallet.unmarkUTXOsAsReserved(reservedUtxos.toVector)
        newReserved <- wallet.listUtxos(TxoState.Reserved)
        newTransactions <- wallet.listTransactions()
      } yield {
        assert(unreservedUtxos.forall(_.state != TxoState.Reserved))
        assert(newReserved.isEmpty)
        assert(!oldTransactions.map(_.transaction).contains(tx))
        assert(!newTransactions.map(_.transaction).contains(tx))
      }
  }

  it should "track a utxo state change to reserved and then to unreserved using the transaction the utxo was included in" in {
    param =>
      val WalletWithBitcoindRpc(wallet, _) = param

      val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

      for {
        oldTransactions <- wallet.listTransactions()
        feeRate <- wallet.getFeeRate
        tx <- wallet.fundRawTransaction(Vector(dummyOutput),
                                        feeRate,
                                        fromTagOpt = None,
                                        markAsReserved = true)
        allReserved <- wallet.listUtxos(TxoState.Reserved)
        _ = assert(
          tx.inputs
            .map(_.previousOutput)
            .forall(allReserved.map(_.outPoint).contains))

        unreservedUtxos <- wallet.unmarkUTXOsAsReserved(tx)
        newTransactions <- wallet.listTransactions()
      } yield {
        assert(unreservedUtxos.forall(_.state != TxoState.Reserved))
        assert(!oldTransactions.map(_.transaction).contains(tx))
        assert(!newTransactions.map(_.transaction).contains(tx))
      }
  }

  it should "track a utxo state change to reserved and then to unreserved using a block" in {
    param =>
      val WalletWithBitcoindRpc(wallet, bitcoind) = param

      val dummyOutput =
        TransactionOutput(Satoshis(100000),
                          P2PKHScriptPubKey(ECPublicKey.freshPublicKey))

      for {
        oldTransactions <- wallet.listTransactions()
        tx <- wallet.sendToOutputs(Vector(dummyOutput), None)
        _ <- wallet.processTransaction(tx, None)
        _ <- wallet.markUTXOsAsReserved(tx)

        allReserved <- wallet.listUtxos(TxoState.Reserved)
        _ = assert(
          tx.inputs
            .map(_.previousOutput)
            .forall(allReserved.map(_.outPoint).contains))

        // Confirm tx in a block
        _ <- bitcoind.sendRawTransaction(tx)
        hash <-
          bitcoind.getNewAddress
            .flatMap(bitcoind.generateToAddress(1, _))
            .map(_.head)
        block <- bitcoind.getBlockRaw(hash)
        _ <- wallet.processBlock(block)

        newReserved <- wallet.listUtxos(TxoState.Reserved)
        newTransactions <- wallet.listTransactions()
      } yield {
        assert(newReserved.isEmpty)
        assert(!oldTransactions.map(_.transaction).contains(tx))
        assert(newTransactions.map(_.transaction).contains(tx))
      }
  }

}
