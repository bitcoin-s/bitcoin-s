package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.builder.RawTxSigner
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.{InternalAddressTag, StorageLocationTag}
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletTestUtil,
  WalletWithBitcoind,
  WalletWithBitcoindRpc
}
import org.scalatest.FutureOutcome

class AddressTagIntegrationTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)

  behavior of "Address Tag - integration test"

  val feeRate: SatoshisPerByte = SatoshisPerByte(Satoshis.one)

  val exampleTag: InternalAddressTag = StorageLocationTag.HotStorage

  it should "correctly keep tagged utxos separated" in { walletWithBitcoind =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = walletWithBitcoind
    // the amount we're receiving from bitcoind
    val valueFromBitcoind = Bitcoins.one

    // the amount we're sending to bitcoind
    val valueToBitcoind = Bitcoins(0.5)

    for {
      addr <- wallet.getNewAddress()
      taggedAddr <- wallet.getNewAddress(Vector(exampleTag))
      txId <- bitcoind.sendMany(
        Map(addr -> valueFromBitcoind, taggedAddr -> valueFromBitcoind))
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
      _ = assert(utxosPostAdd.length == 2)
      _ <-
        wallet
          .getConfirmedBalance()
          .map(confirmed => assert(confirmed == 0.bitcoin))
      _ <-
        wallet
          .getUnconfirmedBalance()
          .map(unconfirmed => assert(unconfirmed == valueFromBitcoind * 2))
      incomingTx <- wallet.incomingTxDAO.findByTxId(tx.txIdBE)
      _ = assert(incomingTx.isDefined)
      _ = assert(incomingTx.get.incomingAmount == valueFromBitcoind * 2)

      taggedUtxosPostAdd <- wallet.listUtxos(exampleTag)
      _ = assert(taggedUtxosPostAdd.length == 1)
      _ <-
        wallet
          .getUnconfirmedBalance(exampleTag)
          .map(unconfirmed => assert(unconfirmed == valueFromBitcoind))

      account <- wallet.getDefaultAccount()
      (txBuilder, utxoInfos) <- bitcoind.getNewAddress.flatMap { addr =>
        val output = TransactionOutput(valueToBitcoind, addr.scriptPubKey)
        wallet
          .fundRawTransactionInternal(destinations = Vector(output),
                                      feeRate = feeRate,
                                      fromAccount = account,
                                      fromTagOpt = Some(exampleTag))
      }
      utx <- txBuilder.buildTx()
      signedTx <- RawTxSigner.sign(utx, utxoInfos, feeRate)
      _ <- wallet.processTransaction(signedTx, None)

      utxos <- wallet.listUtxos()
      balancePostSend <- wallet.getBalance()
      tagBalancePostSend <- wallet.getBalance(exampleTag)
    } yield {
      // One change one external
      assert(utxos.size == 2)
      assert(
        utxos.exists(_.privKeyPath.chain.chainType == HDChainType.External))
      assert(utxos.exists(_.privKeyPath.chain.chainType == HDChainType.Change))

      // untagged balance should be untouched
      assert(balancePostSend - tagBalancePostSend == valueFromBitcoind.satoshis)

      // change UTXO should be smaller than what we had, but still have money in it
      assert(tagBalancePostSend > 0.sats)
      assert(tagBalancePostSend < valueFromBitcoind)

      assert(
        WalletTestUtil.isCloseEnough(tagBalancePostSend,
                                     valueFromBitcoind - valueToBitcoind))
    }
  }
}
