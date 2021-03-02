package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{
  LegacySpendingInfo,
  NestedSegwitV0SpendingInfo,
  SegwitV0SpendingInfo
}
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  TransactionInput
}
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.WalletTestUtil
import org.bitcoins.testkit.wallet.WalletTestUtil._

class SpendingInfoDAOTest extends WalletDAOFixture {
  behavior of "SpendingInfoDAO"

  it should "preserve public key scripts" in { daos =>
    val addressDAO = daos.addressDAO
    val spendingInfoDAO = daos.utxoDAO

    val addr1 = WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb,
                                            addressIndex = 0)
    val addr2 = WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb,
                                            addressIndex = 1)
    assert(addr1.scriptPubKey != addr2.scriptPubKey)

    for {
      createdAddr1 <- addressDAO.create(addr1)
      createdAddr2 <- addressDAO.create(addr2)

      u1 = sampleLegacyUTXO(addr1.scriptPubKey)
      _ <- insertDummyIncomingTransaction(daos, u1)
      utxo1 <- daos.utxoDAO.create(u1)

      u2 = WalletTestUtil.sampleSegwitUTXO(addr2.scriptPubKey)
      _ <- insertDummyIncomingTransaction(daos, u2)
      utxo2 <- daos.utxoDAO.create(u2)

      utxos = Vector(utxo1, utxo2)
      changed = utxos.map(_.copyWithState(TxoState.DoesNotExist))
      updated <- spendingInfoDAO.updateAllSpendingInfoDb(changed)
    } yield {
      assert(updated == changed)
      assert(addr1 == createdAddr1)
      assert(addr2 == createdAddr2)
      assert(addr1.scriptPubKey == utxo1.output.scriptPubKey)
      assert(addr2.scriptPubKey == utxo2.output.scriptPubKey)
    }
  }

  it must "be able to update multiple utxos" in { daos =>
    val addressDAO = daos.addressDAO
    val spendingInfoDAO = daos.utxoDAO

    for {
      account <- daos.accountDAO.create(WalletTestUtil.firstAccountDb)
      addr <- addressDAO.create(getAddressDb(account))

      u1 = sampleLegacyUTXO(addr.scriptPubKey)
      _ <- insertDummyIncomingTransaction(daos, u1)
      utxo1 <- daos.utxoDAO.create(u1)

      u2 = WalletTestUtil.sampleSegwitUTXO(addr.scriptPubKey)
      _ <- insertDummyIncomingTransaction(daos, u2)
      utxo2 <- daos.utxoDAO.create(u2)

      utxos = Vector(utxo1, utxo2)
      changed = utxos.map(_.copyWithState(TxoState.DoesNotExist))
      updated <- spendingInfoDAO.updateAllSpendingInfoDb(changed)
    } yield assert(updated == changed)
  }

  it should "insert a segwit UTXO and read it" in { daos =>
    val utxoDAO = daos.utxoDAO

    for {
      created <- WalletTestUtil.insertSegWitUTXO(daos)
      spk = created.output.scriptPubKey
      read <- utxoDAO.read(created.id.get).map(_.map(_.toSpendingInfoDb(spk)))
    } yield read match {
      case None                          => fail(s"Did not read back a UTXO")
      case Some(_: SegwitV0SpendingInfo) => succeed
      case Some(other)                   => fail(s"did not get segwit UTXO: $other")
    }
  }

  it should "insert a legacy UTXO and read it" in { daos =>
    val utxoDAO = daos.utxoDAO
    for {
      created <- WalletTestUtil.insertLegacyUTXO(daos)
      spk = created.output.scriptPubKey
      read <- utxoDAO.read(created.id.get).map(_.map(_.toSpendingInfoDb(spk)))
    } yield read match {
      case None                        => fail(s"Did not read back a UTXO")
      case Some(_: LegacySpendingInfo) => succeed
      case Some(other)                 => fail(s"did not get a legacy UTXO: $other")
    }
  }

  it should "find incoming outputs being spent, given a TX" in { daos =>
    val utxoDAO = daos.utxoDAO

    for {
      utxo <- WalletTestUtil.insertLegacyUTXO(daos)
      transaction = {
        val randomTX = TransactionGenerators.transaction
          .suchThat(_.inputs.nonEmpty)
          .sampleSome

        val inputs = {
          val head = randomTX.inputs.head
          val ourInput =
            TransactionInput(utxo.outPoint,
                             ScriptSignature.empty,
                             head.sequence)

          ourInput +: randomTX.inputs.tail
        }

        BaseTransaction(randomTX.version,
                        inputs = inputs,
                        outputs = randomTX.outputs,
                        lockTime = randomTX.lockTime)

      }
      txos <- utxoDAO.findOutputsBeingSpent(transaction)
    } yield {
      txos.map { case txo =>
        assert(transaction.inputs.exists(_.previousOutput == txo.outPoint))
      }.toAssertion
    }
  }

  it must "insert an unspent TXO and then mark it as spent" in { daos =>
    val spendingInfoDAO = daos.utxoDAO
    for {
      utxo <- WalletTestUtil.insertSegWitUTXO(daos)
      _ <- spendingInfoDAO.update(
        utxo.copy(state = TxoState.PendingConfirmationsReceived))
      unspent <- spendingInfoDAO.findAllUnspent()
      updated <-
        spendingInfoDAO.updateTxoState(outputs = unspent.map(_.output),
                                       state =
                                         TxoState.PendingConfirmationsSpent)
      unspentPostUpdate <- spendingInfoDAO.findAllUnspent()
    } yield {
      assert(unspent.nonEmpty)
      assert(updated.length == unspent.length)
      assert(unspentPostUpdate.isEmpty)
    }
  }

  it must "insert an unspent TXO and find it as unspent" in { daos =>
    val spendingInfoDAO = daos.utxoDAO
    for {
      utxo <- WalletTestUtil.insertLegacyUTXO(daos)
      state = utxo.copy(state = TxoState.PendingConfirmationsReceived)
      updated <- spendingInfoDAO.update(state)
      unspent <- spendingInfoDAO.findAllUnspent()
    } yield {
      assert(unspent.length == 1)
      val ourUnspent = unspent.head
      assert(ourUnspent == updated)
    }

  }

  it must "insert a spent TXO and NOT find it as unspent" in { daos =>
    val spendingInfoDAO = daos.utxoDAO
    for {
      utxo <- WalletTestUtil.insertLegacyUTXO(daos)
      state = utxo.copy(state = TxoState.PendingConfirmationsSpent)
      _ <- spendingInfoDAO.update(state)
      unspent <- spendingInfoDAO.findAllUnspent()
    } yield assert(unspent.isEmpty)
  }

  it must "insert a TXO and read it back with through a TXID " in { daos =>
    val spendingInfoDAO = daos.utxoDAO

    for {
      utxo <- WalletTestUtil.insertLegacyUTXO(daos)
      foundTxos <- spendingInfoDAO.findTx(utxo.txid)
    } yield assert(foundTxos.contains(utxo))

  }

  it should "insert a nested segwit UTXO and read it" in { daos =>
    val utxoDAO = daos.utxoDAO
    for {
      created <- WalletTestUtil.insertNestedSegWitUTXO(daos)
      spk = created.output.scriptPubKey
      read <- utxoDAO.read(created.id.get).map(_.map(_.toSpendingInfoDb(spk)))
    } yield read match {
      case None                                => fail(s"Did not read back a UTXO")
      case Some(_: NestedSegwitV0SpendingInfo) => succeed
      case Some(other)                         => fail(s"did not get a nested segwit UTXO: $other")
    }
  }

  it should "find incoming outputs dbs being spent, given a TX" in { daos =>
    val utxoDAO = daos.utxoDAO

    for {
      created <- WalletTestUtil.insertNestedSegWitUTXO(daos)
      db <- utxoDAO.read(created.id.get)

      account <- daos.accountDAO.create(WalletTestUtil.firstAccountDb)
      addr <- daos.addressDAO.create(getAddressDb(account))

      // Add another utxo
      u2 = WalletTestUtil.sampleSegwitUTXO(addr.scriptPubKey)
      _ <- insertDummyIncomingTransaction(daos, u2)
      _ <- utxoDAO.create(u2)

      dbs <- utxoDAO.findDbsForTx(created.txid)
    } yield {
      assert(dbs.size == 1)
      assert(db.isDefined)

      assert(dbs == Vector(db.get))
    }
  }
}
