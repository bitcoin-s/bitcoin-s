package org.bitcoins.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.{
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
      .get

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWalletAndBitcoind(test)
  }

  it should "track a utxo state change to pending spent" in { param =>
    val WalletWithBitcoindRpc(wallet, _) = param

    for {
      tx <- wallet.sendToAddress(testAddr,
                                 Satoshis(3000),
                                 SatoshisPerByte(Satoshis(3)))

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
                                        blockHashOpt = None)

      updatedCoin <- wallet.spendingInfoDAO.findByScriptPubKey(
        addr.scriptPubKey)
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
                                      markAsReserved = true)

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
    } yield {
      assert(updatedCoins.forall(_.state == TxoState.Reserved))
    }
  }
}
