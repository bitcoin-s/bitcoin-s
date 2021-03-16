package org.bitcoins.wallet

import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.wallet.utxo.TxoState._
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.WalletTestUtil._
import org.scalatest.FutureOutcome

class UTXOHandlingTest extends BitcoinSWalletTest {

  behavior of "UTXOHandling"

  override type FixtureParam = Wallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNewWallet(test, getBIP39PasswordOpt())
  }

  it must "correctly update txo state based on confirmations" in { wallet =>
    val utxo = sampleSegwitUTXO(EmptyScriptPubKey)
    val requiredConfs = 6
    assert(wallet.walletConfig.requiredConfirmations == requiredConfs)

    val immatureCoinbase = utxo.copyWithState(ImmatureCoinbase)
    val pendingConfReceived = utxo.copyWithState(PendingConfirmationsReceived)
    val pendingConfSpent = utxo.copyWithState(PendingConfirmationsSpent)
    val confReceived = utxo.copyWithState(ConfirmedReceived)
    val confSpent = utxo.copyWithState(ConfirmedSpent)
    val reserved = utxo.copyWithState(Reserved)
    val dne = utxo.copyWithState(DoesNotExist)

    assert(wallet.updateTxoWithConfs(reserved, 1) == reserved)

    assert(wallet.updateTxoWithConfs(immatureCoinbase, 10) == immatureCoinbase)
    assert(wallet.updateTxoWithConfs(immatureCoinbase, 101) == confReceived)

    assert(
      wallet.updateTxoWithConfs(pendingConfReceived, 1) == pendingConfReceived)
    assert(
      wallet.updateTxoWithConfs(pendingConfReceived,
                                requiredConfs) == confReceived)

    assert(wallet.updateTxoWithConfs(pendingConfSpent, 1) == pendingConfSpent)
    assert(
      wallet.updateTxoWithConfs(pendingConfSpent, requiredConfs) == confSpent)

    assert(wallet.updateTxoWithConfs(dne, 1) == dne)
    assert(wallet.updateTxoWithConfs(dne, requiredConfs) == dne)

    assert(wallet.updateTxoWithConfs(confSpent, 1) == confSpent)
    assert(wallet.updateTxoWithConfs(confSpent, requiredConfs) == confSpent)

    assert(wallet.updateTxoWithConfs(confReceived, 1) == confReceived)
    assert(
      wallet.updateTxoWithConfs(confReceived, requiredConfs) == confReceived)
  }
}
