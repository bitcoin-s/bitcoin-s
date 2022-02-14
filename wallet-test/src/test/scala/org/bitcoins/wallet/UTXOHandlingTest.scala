package org.bitcoins.wallet

import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.core.wallet.utxo.TxoState._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.WalletTestUtil._
import org.scalatest.FutureOutcome

class UTXOHandlingTest extends BitcoinSWalletTest {

  behavior of "UTXOHandling"

  override type FixtureParam = Wallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNewWallet(test, getBIP39PasswordOpt())(getFreshWalletAppConfig)
  }

  it must "correctly update txo state based on confirmations" in { wallet =>
    val utxo = sampleSegwitUTXO(EmptyScriptPubKey,
                                state = TxoState.Reserved
    ) //state doesn't matter here
    val requiredConfs = 6
    assert(wallet.walletConfig.requiredConfirmations == requiredConfs)

    val immatureCoinbase = utxo.copyWithState(ImmatureCoinbase)
    val pendingConfReceived = utxo.copyWithState(PendingConfirmationsReceived)
    val spendingTxId = randomTXID
    val pendingConfSpent = utxo
      .copyWithSpendingTxId(spendingTxId)
      .copyWithState(PendingConfirmationsSpent)
    val confReceived = utxo
      .copyWithState(ConfirmedReceived)
    val confSpent = utxo
      .copyWithSpendingTxId(spendingTxId)
      .copyWithState(ConfirmedSpent)
    val reserved = utxo.copyWithState(Reserved)

    assert(wallet.updateReceivedTxoWithConfs(reserved, 1) == reserved)
    val withSpendingTxId =
      reserved.copyWithSpendingTxId(DoubleSha256DigestBE.empty)
    assert(
      wallet
        .updateSpentTxoWithConfs(withSpendingTxId, 1)
        .state == TxoState.PendingConfirmationsSpent)

    assert(
      wallet.updateReceivedTxoWithConfs(immatureCoinbase,
                                        10) == immatureCoinbase)
    assert(
      wallet.updateReceivedTxoWithConfs(immatureCoinbase, 101) == confReceived)

    assertThrows[RuntimeException] {
      //cannot have utxo spent from coinbase before 101 blocks
      wallet.updateSpentTxoWithConfs(immatureCoinbase, 100)
    }

    assert(
      wallet.updateReceivedTxoWithConfs(pendingConfReceived,
                                        1) == pendingConfReceived)

    val pendingConfReceivedWithTxId = pendingConfReceived
      .copyWithSpendingTxId(DoubleSha256DigestBE.empty)

    val expectedConfSpent = pendingConfReceived
      .copyWithSpendingTxId(DoubleSha256DigestBE.empty)
      .copyWithState(TxoState.ConfirmedSpent)

    val updated =
      wallet.updateSpentTxoWithConfs(pendingConfReceivedWithTxId, requiredConfs)
    assert(updated == expectedConfSpent)

    assert(
      wallet.updateSpentTxoWithConfs(pendingConfSpent, 1) == pendingConfSpent)
    assert(
      wallet.updateSpentTxoWithConfs(pendingConfSpent,
                                     requiredConfs) == confSpent)

    assert(wallet.updateReceivedTxoWithConfs(dne, 1) == dne)

    val expectedDNEWithTxId =
      dne.copyWithSpendingTxId(DoubleSha256DigestBE.empty)
    val expectedDNE = expectedDNEWithTxId.copyWithState(TxoState.ConfirmedSpent)
    assert(
      wallet.updateSpentTxoWithConfs(expectedDNEWithTxId,
                                     requiredConfs) == expectedDNE)

    assert(
      wallet.updateSpentTxoWithConfs(confSpent, 1) == confSpent.copyWithState(
        PendingConfirmationsSpent))
    assert(
      wallet.updateSpentTxoWithConfs(confSpent, requiredConfs) == confSpent)

    assert(wallet.updateReceivedTxoWithConfs(confReceived, 1) == confReceived)
    assert(
      wallet.updateReceivedTxoWithConfs(confReceived,
                                        requiredConfs) == confReceived)

    val expectedConfReceivedWithTxid =
      confReceived.copyWithSpendingTxId(DoubleSha256DigestBE.empty)

    val expectedConfReceived =
      expectedConfReceivedWithTxid.copyWithState(TxoState.ConfirmedSpent)
    assert(
      wallet.updateSpentTxoWithConfs(expectedConfReceivedWithTxid,
                                     requiredConfs) == expectedConfReceived)
  }
}
