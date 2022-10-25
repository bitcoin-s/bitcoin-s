package org.bitcoins.wallet

import grizzled.slf4j.Logging
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.builder.RawTxSigner
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.core.wallet.utxo.TxoState._
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPublicKey}
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTestCachedBitcoindNewest,
  WalletWithBitcoindRpc
}
import org.bitcoins.wallet.models.SpendingInfoDAO
import org.scalatest.{Assertion, FutureOutcome, Outcome}

import scala.concurrent.Future

class UTXOLifeCycleTest
    extends BitcoinSWalletTestCachedBitcoindNewest
    with Logging {

  behavior of "Wallet Txo States"

  override type FixtureParam = WalletWithBitcoindRpc

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withFundedWalletAndBitcoindCached(test, bitcoind)(
        getFreshWalletAppConfig)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  val testAddr: BitcoinAddress =
    BitcoinAddress
      .fromString("bcrt1qlhctylgvdsvaanv539rg7hyn0sjkdm23y70kgq")

  it should "track a utxo state change to broadcast spent" in { param =>
    val wallet = param.wallet

    for {
      oldTransactions <- wallet.listTransactions()
      tx <- wallet.sendToAddress(testAddr, Satoshis(3000), None)

      updatedCoins <- wallet.findOutputsBeingSpent(tx)
      newTransactions <- wallet.listTransactions()
    } yield {
      assert(updatedCoins.forall(_.state == TxoState.BroadcastSpent))
      assert(updatedCoins.forall(_.spendingTxIdOpt.contains(tx.txIdBE)))
      assert(!oldTransactions.map(_.transaction).contains(tx))
      assert(newTransactions.map(_.transaction).contains(tx))
    }
  }

  it should "track a utxo state change to confirmed spent" in { param =>
    val wallet = param.wallet
    val bitcoind = param.bitcoind
    val walletConfig = param.walletConfig
    for {
      oldTransactions <- wallet.listTransactions()
      tx <- wallet.sendToAddress(testAddr, Satoshis(3000), None)

      updatedCoins <- wallet.findOutputsBeingSpent(tx)
      newTransactions <- wallet.listTransactions()
      _ = assert(updatedCoins.forall(_.state == BroadcastSpent))
      _ = assert(!oldTransactions.map(_.transaction).contains(tx))
      _ = assert(newTransactions.map(_.transaction).contains(tx))

      // Give tx a fake hash so it can appear as it's in a block
      hash <- bitcoind.getBestBlockHash
      _ <- wallet.processTransaction(tx, Some(hash))

      _ <- wallet.updateUtxoPendingStates()
      pendingCoins <- wallet.findOutputsBeingSpent(tx)
      _ = assert(pendingCoins.forall(_.state == PendingConfirmationsSpent))

      // Put confirmations on top of the tx's block
      _ <- bitcoind.generate(walletConfig.requiredConfirmations)
      // Need to call this to actually update the state, normally a node callback would do this
      _ <- wallet.updateUtxoPendingStates()
      confirmedCoins <- wallet.findOutputsBeingSpent(tx)
    } yield {
      assert(confirmedCoins.forall(_.state == ConfirmedSpent))
      assert(confirmedCoins.forall(_.spendingTxIdOpt.contains(tx.txIdBE)))
    }
  }

  it should "track multiple utxos state change to confirmed spent" in { param =>
    val wallet = param.wallet
    val bitcoind = param.bitcoind

    def checkState(
        utxos: Vector[SpendingInfoDb],
        txid1: DoubleSha256DigestBE,
        txid2: DoubleSha256DigestBE,
        txid3: DoubleSha256DigestBE,
        state: TxoState): Assertion = {
      val utxo1 = utxos.find(_.txid == txid1).get
      assert(utxo1.state == state)
      val utxo2 = utxos.find(_.txid == txid2).get
      assert(utxo2.state == state)
      val utxo3 = utxos.find(_.txid == txid3).get
      assert(utxo3.state == state)
    }

    for {
      addr1 <- wallet.getNewAddress()
      addr2 <- wallet.getNewAddress()
      addr3 <- wallet.getNewAddress()

      oldUtxos <- wallet.listUtxos()

      txid1 <- bitcoind.sendToAddress(addr1, Satoshis(1000))
      txid2 <- bitcoind.sendToAddress(addr2, Satoshis(2000))
      txid3 <- bitcoind.sendToAddress(addr3, Satoshis(3000))

      tx1 <- wallet.findByTxId(txid1)
      _ = assert(tx1.isEmpty)
      tx2 <- wallet.findByTxId(txid2)
      _ = assert(tx2.isEmpty)
      tx3 <- wallet.findByTxId(txid3)
      _ = assert(tx3.isEmpty)

      tx1 <- bitcoind.getRawTransactionRaw(txid1).map(Option.apply).recover {
        case _: Throwable => None
      }
      _ = assert(tx1.nonEmpty)
      tx2 <- bitcoind.getRawTransactionRaw(txid2).map(Option.apply).recover {
        case _: Throwable => None
      }
      _ = assert(tx2.nonEmpty)
      tx3 <- bitcoind.getRawTransactionRaw(txid3).map(Option.apply).recover {
        case _: Throwable => None
      }
      _ = assert(tx3.nonEmpty)

      utxos <- wallet.listUtxos()
      _ = assert(oldUtxos == utxos)

      // process the transactions from mempool
      _ <- wallet.processTransaction(tx1.get, None)
      _ <- wallet.processTransaction(tx2.get, None)
      _ <- wallet.processTransaction(tx3.get, None)

      utxos <- wallet.listUtxos()
      _ = assert(oldUtxos.size + 3 == utxos.size)

      _ = checkState(utxos, txid1, txid2, txid3, TxoState.BroadcastReceived)

      minerAddr <- bitcoind.getNewAddress

      // confirm the transactions
      blockHashes <- bitcoind.generateToAddress(1, minerAddr)
      _ = assert(blockHashes.size == 1)
      blockHash = blockHashes.head
      block <- bitcoind.getBlockRaw(blockHash)
      _ <- wallet.processBlock(block)
      _ <- wallet.updateUtxoPendingStates()

      utxos <- wallet.listUtxos()
      _ = assert(oldUtxos.size + 3 == utxos.size)

      // mine the second block
      blockHashes <- bitcoind.generateToAddress(1, minerAddr)
      _ = assert(blockHashes.size == 1)
      blockHash = blockHashes.head
      block <- bitcoind.getBlockRaw(blockHash)
      _ <- wallet.processBlock(block)
      _ <- wallet.updateUtxoPendingStates()

      utxos <- wallet.listUtxos()
      _ = assert(oldUtxos.size + 3 == utxos.size)

      _ = checkState(utxos,
                     txid1,
                     txid2,
                     txid3,
                     TxoState.PendingConfirmationsReceived)

      // mine the third block
      blockHashes <- bitcoind.generateToAddress(1, minerAddr)
      _ = assert(blockHashes.size == 1)
      blockHash = blockHashes.head
      block <- bitcoind.getBlockRaw(blockHash)
      _ <- wallet.processBlock(block)
      _ <- wallet.updateUtxoPendingStates()

      utxos <- wallet.listUtxos()
      _ = assert(oldUtxos.size + 3 == utxos.size)

      _ = checkState(utxos,
                     txid1,
                     txid2,
                     txid3,
                     TxoState.PendingConfirmationsReceived)

      // mine the fourth block
      blockHashes <- bitcoind.generateToAddress(1, minerAddr)
      _ = assert(blockHashes.size == 1)
      blockHash = blockHashes.head
      block <- bitcoind.getBlockRaw(blockHash)
      _ <- wallet.processBlock(block)
      _ <- wallet.updateUtxoPendingStates()

      utxos <- wallet.listUtxos()
      _ = assert(oldUtxos.size + 3 == utxos.size)

      _ = checkState(utxos,
                     txid1,
                     txid2,
                     txid3,
                     TxoState.PendingConfirmationsReceived)

      // mine the fifth block
      blockHashes <- bitcoind.generateToAddress(1, minerAddr)
      _ = assert(blockHashes.size == 1)
      blockHash = blockHashes.head
      block <- bitcoind.getBlockRaw(blockHash)
      _ <- wallet.processBlock(block)
      _ <- wallet.updateUtxoPendingStates()

      utxos <- wallet.listUtxos()
      _ = assert(oldUtxos.size + 3 == utxos.size)

      _ = checkState(utxos,
                     txid1,
                     txid2,
                     txid3,
                     TxoState.PendingConfirmationsReceived)

      // mine the sixth block
      blockHashes <- bitcoind.generateToAddress(1, minerAddr)
      _ = assert(blockHashes.size == 1)
      blockHash = blockHashes.head
      block <- bitcoind.getBlockRaw(blockHash)
      _ <- wallet.processBlock(block)
      _ <- wallet.updateUtxoPendingStates()

      utxos <- wallet.listUtxos()
      _ = assert(oldUtxos.size + 3 == utxos.size)

      utxo1 = utxos.find(_.txid == txid1).get
      utxo2 = utxos.find(_.txid == txid2).get
      utxo3 = utxos.find(_.txid == txid3).get

    } yield {
      assert(utxo1.state == ConfirmedReceived)
      assert(utxo2.state == ConfirmedReceived)
      assert(utxo3.state == ConfirmedReceived)
    }
  }

  it should "handle an RBF transaction on unconfirmed coins" in { param =>
    val wallet = param.wallet

    for {
      tx <- wallet.sendToAddress(testAddr,
                                 Satoshis(3000),
                                 Some(SatoshisPerByte.one))

      coins <- wallet.findOutputsBeingSpent(tx)
      _ = assert(coins.forall(_.state == BroadcastSpent))
      _ = assert(coins.forall(_.spendingTxIdOpt.contains(tx.txIdBE)))

      rbf <- wallet.bumpFeeRBF(tx.txIdBE, SatoshisPerByte.fromLong(3))
      _ <- wallet.processTransaction(rbf, None)
      rbfCoins <- wallet.findOutputsBeingSpent(rbf)
    } yield {
      assert(rbfCoins.forall(_.state == BroadcastSpent))
      assert(rbfCoins.forall(_.spendingTxIdOpt.contains(rbf.txIdBE)))
    }
  }

  it should "handle attempting to spend an immature coinbase" in { param =>
    val wallet = param.wallet

    val spendingInfoDAO =
      SpendingInfoDAO()(system.dispatcher, param.walletConfig)
    for {
      tx <- wallet.sendToAddress(testAddr, Satoshis(3000), None)

      coins <- wallet.findOutputsBeingSpent(tx)

      updatedCoins = coins.map(_.copyWithState(TxoState.ImmatureCoinbase))
      _ <- spendingInfoDAO.updateAllSpendingInfoDb(updatedCoins)

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
    val wallet = param.wallet
    val bitcoind = param.bitcoind
    val walletConfig = param.walletConfig
    for {
      oldTransactions <- wallet.listTransactions()
      tx <- wallet.sendToAddress(testAddr, Satoshis(3000), None)

      updatedCoins <- wallet.findOutputsBeingSpent(tx)
      newTransactions <- wallet.listTransactions()
      _ = assert(updatedCoins.forall(_.state == BroadcastSpent))
      _ = assert(!oldTransactions.map(_.transaction).contains(tx))
      _ = assert(newTransactions.map(_.transaction).contains(tx))

      // Give tx a fake hash so it can appear as it's in a block
      hash <- bitcoind.getBestBlockHash
      _ <- wallet.processTransaction(tx, Some(hash))

      _ <- wallet.updateUtxoPendingStates()
      pendingCoins <- wallet.findOutputsBeingSpent(tx)
      _ = assert(pendingCoins.forall(_.state == PendingConfirmationsSpent))

      // Put confirmations on top of the tx's block
      _ <- bitcoind.generate(walletConfig.requiredConfirmations)
      // Need to call this to actually update the state, normally a node callback would do this
      _ <- wallet.updateUtxoPendingStates()
      confirmedCoins <- wallet.findOutputsBeingSpent(tx)

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

  it should "track a utxo state change to broadcast received" in { param =>
    val wallet = param.wallet
    val bitcoind = param.bitcoind

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
        wallet.findByScriptPubKey(addr.scriptPubKey)
      newTransactions <- wallet.listTransactions()
    } yield {
      assert(updatedCoin.forall(_.state == TxoState.BroadcastReceived))
      assert(!oldTransactions.map(_.transaction).contains(tx))
      assert(newTransactions.map(_.transaction).contains(tx))
    }
  }

  it should "track a utxo state change to pending received" in { param =>
    val wallet = param.wallet
    val bitcoind = param.bitcoind

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
        wallet.findByScriptPubKey(addr.scriptPubKey)
      newTransactions <- wallet.listTransactions()
      _ = assert(updatedCoin.forall(_.state == TxoState.BroadcastReceived))

      hash <- bitcoind.getNewAddress
        .flatMap(bitcoind.generateToAddress(1, _))
        .map(_.head)
      _ <- wallet.processTransaction(tx, Some(hash))

      pendingCoins <-
        wallet.findByScriptPubKey(addr.scriptPubKey)
    } yield {
      assert(
        pendingCoins.forall(_.state == TxoState.PendingConfirmationsReceived))
      assert(!oldTransactions.map(_.transaction).contains(tx))
      assert(newTransactions.map(_.transaction).contains(tx))
    }
  }

  it should "track a utxo state change to confirmed received" in { param =>
    val wallet = param.wallet
    val bitcoind = param.bitcoind
    val walletConfig = param.walletConfig
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
        wallet.findByScriptPubKey(addr.scriptPubKey)
      newTransactions <- wallet.listTransactions()
      _ = assert(updatedCoin.forall(_.state == PendingConfirmationsReceived))
      _ = assert(!oldTransactions.map(_.transaction).contains(tx))
      _ = assert(newTransactions.map(_.transaction).contains(tx))

      // Put confirmations on top of the tx's block
      _ <- bitcoind.generate(walletConfig.requiredConfirmations)
      // Need to call this to actually update the state, normally a node callback would do this
      _ <- wallet.updateUtxoPendingStates()
      confirmedCoins <-
        wallet.findByScriptPubKey(addr.scriptPubKey)
    } yield assert(confirmedCoins.forall(_.state == ConfirmedReceived))
  }

  it should "track a utxo state change to reserved" in { param =>
    val wallet = param.wallet

    val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

    for {
      oldTransactions <- wallet.listTransactions()
      feeRate <- wallet.getFeeRate()
      rawTxHelper <- wallet.fundRawTransaction(Vector(dummyOutput),
                                               feeRate,
                                               fromTagOpt = None,
                                               markAsReserved = true)

      tx = rawTxHelper.unsignedTx
      updatedCoins <- wallet.findOutputsBeingSpent(tx)
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
      val wallet = param.wallet

      val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

      for {
        oldTransactions <- wallet.listTransactions()
        feeRate <- wallet.getFeeRate()
        rawTxHelper <- wallet.fundRawTransaction(Vector(dummyOutput),
                                                 feeRate,
                                                 fromTagOpt = None,
                                                 markAsReserved = true)

        tx = rawTxHelper.unsignedTx
        reservedUtxos <- wallet.findOutputsBeingSpent(tx)
        allReserved <- wallet.listUtxos(TxoState.Reserved)
        _ = assert(reservedUtxos.forall(_.state == TxoState.Reserved))
        _ = assert(reservedUtxos.forall(allReserved.contains))

        unreservedUtxos <- wallet.unmarkUTXOsAsReserved(reservedUtxos)
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
      val wallet = param.wallet

      val dummyOutput = TransactionOutput(Satoshis(3000), EmptyScriptPubKey)

      for {
        oldTransactions <- wallet.listTransactions()
        feeRate <- wallet.getFeeRate()
        rawTxHelper <- wallet.fundRawTransaction(Vector(dummyOutput),
                                                 feeRate,
                                                 fromTagOpt = None,
                                                 markAsReserved = true)

        tx = rawTxHelper.unsignedTx
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
      val wallet = param.wallet
      val bitcoind = param.bitcoind
      val dummyOutput =
        TransactionOutput(Satoshis(100000),
                          P2PKHScriptPubKey(ECPublicKey.freshPublicKey))
      val accountF = wallet.getDefaultAccount()
      for {
        oldTransactions <- wallet.listTransactions()
        account <- accountF
        rawTxHelper <- wallet.fundRawTransaction(
          destinations = Vector(dummyOutput),
          feeRate = SatoshisPerVirtualByte.one,
          fromAccount = account,
          markAsReserved = true
        )
        builderResult = rawTxHelper.txBuilderWithFinalizer.builder.result()
        unsignedTx = rawTxHelper.txBuilderWithFinalizer.finalizer.buildTx(
          builderResult)
        tx = RawTxSigner.sign(unsignedTx, rawTxHelper.scriptSigParams)
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

  it should "handle a utxo being spent from a tx outside the wallet" in {
    param =>
      val wallet = param.wallet
      val bitcoind = param.bitcoind

      for {
        utxo <- wallet.listUtxos().map(_.head)
        changeAddr <- wallet.getNewChangeAddress()
        unsignedPSBT = {
          val input =
            TransactionInput(utxo.outPoint, EmptyScriptSignature, UInt32.zero)

          val amt = Satoshis(100000)
          val output =
            TransactionOutput(amt, testAddr.scriptPubKey)
          val changeOutput =
            TransactionOutput(utxo.output.value - amt - Satoshis(1000),
                              changeAddr.scriptPubKey)

          val tx = BaseTransaction(Int32.two,
                                   Vector(input),
                                   Vector(output, changeOutput),
                                   UInt32.zero)

          PSBT.fromUnsignedTx(tx)
        }

        psbt <- wallet.signPSBT(unsignedPSBT)

        tx <- Future.fromTry(
          psbt.finalizePSBT.flatMap(_.extractTransactionAndValidate))

        // Confirm tx in a block
        _ <- bitcoind.sendRawTransaction(tx)
        hash <- bitcoind.generateToAddress(1, testAddr).map(_.head)
        block <- bitcoind.getBlockRaw(hash)
        _ <- wallet.processBlock(block)

        updatedCoins <- wallet.findOutputsBeingSpent(tx)
      } yield {
        assert(
          updatedCoins.forall(_.state == TxoState.PendingConfirmationsSpent))
        assert(updatedCoins.forall(_.spendingTxIdOpt.contains(tx.txIdBE)))
      }
  }

  it must "fail to mark utxos as reserved if one of the utxos is already reserved" in {
    param =>
      val wallet = param.wallet
      val utxosF = wallet.listUtxos()

      val reservedUtxoF: Future[SpendingInfoDb] = for {
        utxos <- utxosF
        first = utxos.head
        //just reserve this one to start
        reserved <- wallet.markUTXOsAsReserved(Vector(first))
      } yield reserved.head

      val reserveFailedF = for {
        utxos <- utxosF
        _ <- reservedUtxoF
        //now try to reserve them all
        //this should fail as the first utxo is reserved
        _ <- wallet.markUTXOsAsReserved(utxos)
      } yield ()

      val assertionF = recoverToSucceededIf[RuntimeException](reserveFailedF)

      for {
        _ <- assertionF
        reserved <- reservedUtxoF
        utxos <- wallet.listUtxos(TxoState.Reserved)
      } yield {
        //make sure only 1 utxo is still reserved
        assert(utxos.length == 1)
        assert(reserved.outPoint == utxos.head.outPoint)
      }
  }

  it must "mark a utxo as reserved that is still receiving confirmations and not unreserve the utxo" in {
    param =>
      val wallet = param.wallet
      val bitcoind = param.bitcoind
      val addressF = wallet.getNewAddress()
      val txIdF =
        addressF.flatMap(addr => bitcoind.sendToAddress(addr, Bitcoins.one))
      val throwAwayAddrF = bitcoind.getNewAddress
      for {
        txId <- txIdF
        //generate a few blocks to make the utxo pending confirmations received
        throwAwayAddr <- throwAwayAddrF
        hashes <- bitcoind.generateToAddress(blocks = 1, throwAwayAddr)
        block <- bitcoind.getBlockRaw(hashes.head)
        _ <- wallet.processBlock(block)

        //make sure the utxo is pending confirmations received
        utxos <- wallet.listUtxos(TxoState.PendingConfirmationsReceived)
        _ = assert(utxos.length == 1)
        utxo = utxos.head
        _ = assert(utxo.txid == txId)
        _ = assert(utxo.state == TxoState.PendingConfirmationsReceived)
        //now mark the utxo as reserved
        _ <- wallet.markUTXOsAsReserved(Vector(utxo))
        //confirm it is reserved
        _ <- wallet
          .listUtxos(TxoState.Reserved)
          .map(utxos =>
            assert(utxos.contains(utxo.copyWithState(TxoState.Reserved))))

        //now process another block
        hashes2 <- bitcoind.generateToAddress(blocks = 1, throwAwayAddr)
        block2 <- bitcoind.getBlockRaw(hashes2.head)
        _ <- wallet.processBlock(block2)

        //the utxo should still be reserved
        reservedUtxos <- wallet.listUtxos(TxoState.Reserved)
        reservedUtxo = reservedUtxos.head
      } yield {
        assert(reservedUtxo.txid == txId)
        assert(reservedUtxo.state == TxoState.Reserved)
      }
  }

  it must "transition a reserved utxo to spent when we are offline" in {
    param =>
      val wallet = param.wallet
      val bitcoind = param.bitcoind
      val bitcoindAddrF = bitcoind.getNewAddress
      val amt = Satoshis(100000)
      val utxoCountF = wallet.listUtxos()
      for {
        bitcoindAdr <- bitcoindAddrF
        utxoCount <- utxoCountF
        //build a spending transaction
        tx <- wallet.sendToAddress(bitcoindAdr, amt, SatoshisPerVirtualByte.one)
        c <- wallet.listUtxos()
        _ = assert(c.length == utxoCount.length)
        txIdBE <- bitcoind.sendRawTransaction(tx)

        //find all utxos that we can use to fund a transaction
        utxos <- wallet
          .listUtxos()
          .map(_.filter(u => TxoState.receivedStates.contains(u.state)))
        broadcastReceived <- wallet.listUtxos(TxoState.BroadcastReceived)
        _ = assert(broadcastReceived.length == 1) //change output

        //mark all utxos as reserved
        _ <- wallet.markUTXOsAsReserved(utxos)
        newReservedUtxos <- wallet.listUtxos(TxoState.Reserved)

        //make sure all utxos are reserved
        _ = assert(newReservedUtxos.length == utxoCount.length)
        blockHash <- bitcoind.generateToAddress(1, bitcoindAdr).map(_.head)
        block <- bitcoind.getBlockRaw(blockHash)
        _ <- wallet.processBlock(block)
        broadcastSpentUtxo <- wallet.listUtxos(
          TxoState.PendingConfirmationsSpent)
        pendingConfirmationsReceivedUtxos <- wallet.listUtxos(
          TxoState.PendingConfirmationsReceived)
        finalReservedUtxos <- wallet.listUtxos(TxoState.Reserved)
      } yield {
        assert(newReservedUtxos == finalReservedUtxos)
        assert(pendingConfirmationsReceivedUtxos.isEmpty)
        assert(broadcastSpentUtxo.length == 1)
        //make sure spendingTxId got set correctly
        assert(broadcastSpentUtxo.head.spendingTxIdOpt.get == txIdBE)
        //make sure no utxos get unreserved when processing the block
        assert(finalReservedUtxos.length == newReservedUtxos.length)
      }
  }

  it should "track a utxo state change to broadcast spent and then to pending confirmations received (the spend transaction gets confirmed together with the receive transaction))" in {
    param =>
      val wallet = param.wallet
      val bitcoind = param.bitcoind

      val receiveValue = Bitcoins(8)
      val sendValue = Bitcoins(4)

      for {
        // receive a UTXO from an external wallet
        walletAddress <- wallet.getNewAddress()
        txId <- bitcoind.sendToAddress(walletAddress, receiveValue)
        receiveTx <- bitcoind.getRawTransactionRaw(txId)
        _ <- wallet.processTransaction(receiveTx, None)
        receiveOutPointPair = receiveTx.outputs.zipWithIndex
          .find(_._1.value == receiveValue)
          .map(out => (receiveTx.txId, out._2))
          .get

        receiveOutPoint = TransactionOutPoint(receiveOutPointPair._1,
                                              UInt32(receiveOutPointPair._2))

        receivedUtxo <- wallet.findByOutPoints(Vector(receiveOutPoint))
        _ = assert(receivedUtxo.size == 1)
        _ = assert(receivedUtxo.head.state == BroadcastReceived)

        // spend and broadcast unconfirmed
        feeRate <- wallet.feeRateApi.getFeeRate()
        sendTx <- wallet.sendWithAlgo(
          testAddr,
          sendValue,
          feeRate,
          CoinSelectionAlgo.SelectedUtxos(Set(receiveOutPointPair)))
        _ <- wallet.broadcastTransaction(sendTx)

        receivedUtxo <- wallet.findByOutPoints(Vector(receiveOutPoint))
        _ = assert(receivedUtxo.size == 1)
        _ = assert(receivedUtxo.head.state == BroadcastSpent)

        // confirm receive and spend
        blockHashes <- bitcoind.generate(1)
        block <- bitcoind.getBlockRaw(blockHashes.head)
        _ <- wallet.processBlock(block)

        receivedUtxo <- wallet.findByOutPoints(Vector(receiveOutPoint))
      } yield {
        assert(receivedUtxo.size == 1)
        assert(receivedUtxo.head.state == PendingConfirmationsSpent)
      }
  }

  it should "track a utxo state change to broadcast spent and then to pending confirmations received (the spend transaction gets confirmed after the receive transaction))" in {
    param =>
      val wallet = param.wallet
      val bitcoind = param.bitcoind

      val receiveValue = Bitcoins(8)
      val sendValue = Bitcoins(4)

      for {
        // receive a UTXO from an external wallet
        walletAddress <- wallet.getNewAddress()
        txId <- bitcoind.sendToAddress(walletAddress, receiveValue)
        receiveTx <- bitcoind.getRawTransactionRaw(txId)
        _ <- wallet.processTransaction(receiveTx, None)
        receiveOutPointPair = receiveTx.outputs.zipWithIndex
          .find(_._1.value == receiveValue)
          .map(out => (receiveTx.txId, out._2))
          .get

        receiveOutPoint = TransactionOutPoint(receiveOutPointPair._1,
                                              UInt32(receiveOutPointPair._2))

        receivedUtxo <- wallet.findByOutPoints(Vector(receiveOutPoint))
        _ = assert(receivedUtxo.size == 1)
        _ = assert(receivedUtxo.head.state == BroadcastReceived)

        // spend unconfirmed
        feeRate <- wallet.feeRateApi.getFeeRate()
        sendTx <- wallet.sendWithAlgo(
          testAddr,
          sendValue,
          feeRate,
          CoinSelectionAlgo.SelectedUtxos(Set(receiveOutPointPair)))

        receivedUtxo <- wallet.findByOutPoints(Vector(receiveOutPoint))
        _ = assert(receivedUtxo.size == 1)
        _ = assert(receivedUtxo.head.state == BroadcastSpent)

        // confirm receive
        blockHashes <- bitcoind.generate(1)
        block <- bitcoind.getBlockRaw(blockHashes.head)
        _ <- wallet.processBlock(block)

        receivedUtxo <- wallet.findByOutPoints(Vector(receiveOutPoint))
        _ = assert(receivedUtxo.size == 1)
        _ = assert(receivedUtxo.head.state == BroadcastSpent)

        // broadcast and confirm spend
        _ <- wallet.broadcastTransaction(sendTx)
        blockHashes <- bitcoind.generate(1)
        block <- bitcoind.getBlockRaw(blockHashes.head)
        _ <- wallet.processBlock(block)

        receivedUtxo <- wallet.findByOutPoints(Vector(receiveOutPoint))
      } yield {
        assert(receivedUtxo.size == 1)
        assert(receivedUtxo.head.state == PendingConfirmationsSpent)
      }
  }
}
