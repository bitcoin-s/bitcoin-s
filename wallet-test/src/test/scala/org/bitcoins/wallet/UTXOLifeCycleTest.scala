package org.bitcoins.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.TxoState
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
      tx <- wallet.sendToAddress(testAddr,
                                 Satoshis(3000),
                                 Some(SatoshisPerByte(Satoshis(3))))

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
    } yield {
      assert(updatedCoins.forall(_.state == TxoState.PendingConfirmationsSpent))
    }
  }

  it should "track a utxo state change to pending recieved" in { param =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = param

    for {
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
    } yield {
      assert(
        updatedCoin.forall(_.state == TxoState.PendingConfirmationsReceived))
    }
  }

  it should "track a utxo state change to reserved" in { param =>
    val WalletWithBitcoindRpc(wallet, _) = param

    val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

    for {
      tx <- wallet.fundRawTransaction(Vector(dummyOutput),
                                      SatoshisPerVirtualByte.one,
                                      fromTagOpt = None,
                                      markAsReserved = true)

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
      reserved <- wallet.listUtxos(TxoState.Reserved)
    } yield {
      assert(updatedCoins.forall(_.state == TxoState.Reserved))
      assert(updatedCoins.forall(reserved.contains))
    }
  }

  it should "track a utxo state change to reserved and then to unreserved" in {
    param =>
      val WalletWithBitcoindRpc(wallet, _) = param

      val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

      for {
        tx <- wallet.fundRawTransaction(Vector(dummyOutput),
                                        SatoshisPerVirtualByte.one,
                                        fromTagOpt = None,
                                        markAsReserved = true)

        reservedUtxos <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
        allReserved <- wallet.listUtxos(TxoState.Reserved)
        _ = assert(reservedUtxos.forall(_.state == TxoState.Reserved))
        _ = assert(reservedUtxos.forall(allReserved.contains))

        unreservedUtxos <- wallet.unmarkUTXOsAsReserved(reservedUtxos.toVector)
      } yield {
        assert(unreservedUtxos.forall(_.state != TxoState.Reserved))
      }
  }

  it should "track a utxo state change to reserved and then to unreserved using the transaction the utxo was included in" in {
    param =>
      val WalletWithBitcoindRpc(wallet, _) = param

      val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

      for {
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
      } yield {
        assert(unreservedUtxos.forall(_.state != TxoState.Reserved))
      }
  }

}
