package org.bitcoins.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, P2PKHScriptPubKey}
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.TxoState
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

  val testAddr: BitcoinAddress =
    BitcoinAddress
      .fromString("bcrt1qlhctylgvdsvaanv539rg7hyn0sjkdm23y70kgq")

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWalletAndBitcoind(test, getBIP39PasswordOpt())
  }

  it should "track a utxo state change to pending spent" in { param =>
    val WalletWithBitcoindRpc(wallet, _) = param

    for {
      oldTransactions <- wallet.listTransactions()
      tx <- wallet.sendToAddress(testAddr,
                                 Satoshis(3000),
                                 Some(SatoshisPerByte(Satoshis(3))))

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      newTransactions <- wallet.listTransactions()
    } yield {
      assert(updatedCoins.forall(_.state == TxoState.PendingConfirmationsSpent))
      assert(!oldTransactions.map(_.transaction).contains(tx))
      assert(newTransactions.map(_.transaction).contains(tx))
    }
  }

  it should "track a utxo state change to pending recieved" in { param =>
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

  it should "track a utxo state change to reserved" in { param =>
    val WalletWithBitcoindRpc(wallet, _) = param

    val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

    for {
      oldTransactions <- wallet.listTransactions()
      tx <- wallet.fundRawTransaction(Vector(dummyOutput),
                                      SatoshisPerVirtualByte.one,
                                      fromTagOpt = None,
                                      markAsReserved = true)

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      reserved <- wallet.listUtxos(TxoState.Reserved)
      newTransactions <- wallet.listTransactions()
    } yield {
      assert(updatedCoins.forall(_.state == TxoState.Reserved))
      assert(updatedCoins.forall(reserved.contains))
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
        tx <- wallet.fundRawTransaction(Vector(dummyOutput),
                                        SatoshisPerVirtualByte.one,
                                        fromTagOpt = None,
                                        markAsReserved = true)

        reservedUtxos <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
        allReserved <- wallet.listUtxos(TxoState.Reserved)
        _ = assert(reservedUtxos.forall(_.state == TxoState.Reserved))
        _ = assert(reservedUtxos.forall(allReserved.contains))

        unreservedUtxos <- wallet.unmarkUTXOsAsReserved(reservedUtxos.toVector)
        newTransactions <- wallet.listTransactions()
      } yield {
        assert(unreservedUtxos.forall(_.state != TxoState.Reserved))
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
        tx <- wallet.fundRawTransaction(Vector(dummyOutput),
                                        SatoshisPerVirtualByte.one,
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
        tx <- wallet.sendToOutputs(Vector(dummyOutput),
                                   Some(SatoshisPerVirtualByte.one))
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
