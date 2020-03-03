package org.bitcoins.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.WalletWithBitcoind
import org.scalatest.FutureOutcome

class UTXOLifeCycleTest extends BitcoinSWalletTest {

  behavior of "Wallet Txo States"

  override type FixtureParam = WalletWithBitcoind

  val testAddr: BitcoinAddress =
    BitcoinAddress.fromString("n4MN27Lk7Yh3pwfjCiAbRXtRVjs4Uk67fG").get

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWalletAndBitcoind(test)
  }

  it should "track a utxo state change to pending spent" in { param =>
    val WalletWithBitcoind(wallet, _) = param

    for {
      tx <- wallet.sendToAddress(testAddr,
                                 Satoshis(3000),
                                 SatoshisPerVirtualByte(Satoshis(3)))

      updatedCoins <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
    } yield {
      assert(updatedCoins.forall(_.state == TxoState.PendingConfirmationsSpent))
    }
  }

  it should "track a utxo state change to pending recieved" in { param =>
    val WalletWithBitcoind(wallet, bitcoind) = param

    for {
      addr <- wallet.getNewAddress()

      txId <- bitcoind.sendToAddress(addr, Satoshis(3000))
      tx <- bitcoind.getRawTransactionRaw(txId)
      _ <- wallet.processOurTransaction(tx, None)

      updatedCoin <- wallet.spendingInfoDAO.findByScriptPubKey(
        addr.scriptPubKey)
    } yield {
      assert(
        updatedCoin.forall(_.state == TxoState.PendingConfirmationsReceived))
    }
  }

  it should "track a utxo state change to reserved" in { param =>
    val WalletWithBitcoind(wallet, _) = param

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
