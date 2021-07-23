package org.bitcoins.dlc.wallet

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.wallet.db.TransactionDbHelper
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  TransactionConstants,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.crypto.{ECAdaptorSignature, ECPublicKey, Sha256Digest}
import org.bitcoins.db.CRUD
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.testkit.fixtures.DLCDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, DLCWalletUtil}
import org.scalatest.Assertion

import scala.concurrent.Future

class DLCDAOTest extends BitcoinSWalletTest with DLCDAOFixture {

  behavior of "DLCDAO"

  val dlcDb: DLCDb = DLCWalletUtil.sampleDLCDb

  val dlcId: Sha256Digest = dlcDb.dlcId

  def verifyDatabaseInsertion[ElementType, KeyType](
      element: ElementType,
      key: KeyType,
      dao: CRUD[ElementType, KeyType],
      dlcDAO: DLCDAO): Future[Assertion] = {
    for {
      _ <- dlcDAO.create(dlcDb)
      _ <- dao.upsert(element) //upsert in case we are testing the dlcDAO

      read <- dao.read(key)
    } yield {
      assert(read.contains(element))
    }
  }

  it should "correctly insert a DLC into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    verifyDatabaseInsertion(dlcDb, dlcId, dlcDAO, dlcDAO)
  }

  it should "correctly insert a DLCOffer into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val offerDAO = daos.dlcOfferDAO

    val offerDb =
      DLCOfferDbHelper.fromDLCOffer(dlcId, DLCWalletUtil.sampleDLCOffer)

    verifyDatabaseInsertion(offerDb, dlcId, offerDAO, dlcDAO)
  }

  it should "correctly insert a DLCAccept into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val acceptDAO = daos.dlcAcceptDAO

    val acceptDb =
      DLCAcceptDbHelper.fromDLCAccept(dlcId, DLCWalletUtil.sampleDLCAccept)

    verifyDatabaseInsertion(acceptDb, dlcId, acceptDAO, dlcDAO)
  }

  it should "correctly insert funding inputs into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val inputsDAO = daos.dlcInputsDAO

    val input = DLCFundingInputDb(
      dlcId = dlcId,
      isInitiator = true,
      inputSerialId = UInt64.zero,
      outPoint = TransactionOutPoint(testBlockHash, UInt32.zero),
      output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
      nSequence = TransactionConstants.enableRBFSequence,
      maxWitnessLength = 107,
      redeemScriptOpt = None,
      witnessScriptOpt = Some(DLCWalletUtil.dummyScriptWitness)
    )

    verifyDatabaseInsertion(input, input.outPoint, inputsDAO, dlcDAO)
  }

  it should "correctly find funding inputs by dlcId and isInitiator" in {
    daos =>
      val inputsDAO = daos.dlcInputsDAO
      val dlcDAO = daos.dlcDAO

      val inputs = Vector(
        DLCFundingInputDb(
          dlcId = dlcId,
          isInitiator = true,
          inputSerialId = UInt64.zero,
          outPoint = TransactionOutPoint(testBlockHash, UInt32.zero),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          nSequence = TransactionConstants.enableRBFSequence,
          maxWitnessLength = 107,
          redeemScriptOpt = None,
          witnessScriptOpt = Some(DLCWalletUtil.dummyScriptWitness)
        ),
        DLCFundingInputDb(
          dlcId = dlcId,
          isInitiator = false,
          inputSerialId = UInt64.one,
          outPoint = TransactionOutPoint(testBlockHash, UInt32.one),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          nSequence = TransactionConstants.enableRBFSequence,
          maxWitnessLength = 107,
          redeemScriptOpt = None,
          witnessScriptOpt = Some(DLCWalletUtil.dummyScriptWitness)
        ),
        DLCFundingInputDb(
          dlcId = dlcId,
          isInitiator = true,
          inputSerialId = UInt64(2),
          outPoint = TransactionOutPoint(testBlockHash, UInt32(3)),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          nSequence = TransactionConstants.enableRBFSequence,
          maxWitnessLength = 107,
          redeemScriptOpt = None,
          witnessScriptOpt = Some(DLCWalletUtil.dummyScriptWitness)
        )
      )

      for {
        _ <- dlcDAO.create(dlcDb)
        _ <- inputsDAO.createAll(inputs)

        readInput <- inputsDAO.findByDLCId(dlcId, isInitiator = true)
      } yield assert(readInput.size == 2)
  }

  it should "correctly insert enum outcome CET signatures into the database" in {
    daos =>
      val dlcDAO = daos.dlcDAO
      val sigsDAO = daos.dlcSigsDAO

      val sig = DLCCETSignaturesDb(
        dlcId = dlcId,
        sigPoint = ECPublicKey.freshPublicKey,
        index = 0,
        accepterSig = ECAdaptorSignature.dummy,
        initiatorSig = None
      )

      verifyDatabaseInsertion(sig,
                              DLCCETSignaturesPrimaryKey(sig.dlcId, sig.index),
                              sigsDAO,
                              dlcDAO)
  }

  it should "correctly insert unsigned numeric outcome CET signatures into the database" in {
    daos =>
      val dlcDAO = daos.dlcDAO
      val sigsDAO = daos.dlcSigsDAO

      val sig = DLCCETSignaturesDb(
        dlcId = dlcId,
        sigPoint = ECPublicKey.freshPublicKey,
        index = 1,
        accepterSig = ECAdaptorSignature.dummy,
        initiatorSig = None
      )

      verifyDatabaseInsertion(sig,
                              DLCCETSignaturesPrimaryKey(sig.dlcId, sig.index),
                              sigsDAO,
                              dlcDAO)
  }

  it should "correctly find CET signatures by dlcId" in { daos =>
    val dlcDAO = daos.dlcDAO
    val sigsDAO = daos.dlcSigsDAO

    val sigs = Vector(
      DLCCETSignaturesDb(
        dlcId = dlcId,
        sigPoint = ECPublicKey.freshPublicKey,
        index = 2,
        accepterSig = ECAdaptorSignature.dummy,
        initiatorSig = Some(ECAdaptorSignature.dummy)
      ),
      DLCCETSignaturesDb(
        dlcId = dlcId,
        sigPoint = ECPublicKey.freshPublicKey,
        index = 3,
        accepterSig = ECAdaptorSignature.dummy,
        initiatorSig = Some(ECAdaptorSignature.dummy)
      )
    )

    for {
      _ <- dlcDAO.create(dlcDb)
      _ <- sigsDAO.createAll(sigs)

      readInput <- sigsDAO.findByDLCId(dlcId)
    } yield {
      assert(readInput.size == 2)
      // Do it this way so ordering doesn't matter
      assert(readInput.contains(sigs.head))
      assert(readInput.contains(sigs.last))
    }
  }

  it should "correctly insert txs into the database" in { daos =>
    val remoteTxDAO = daos.dlcRemoteTxDAO

    val tx =
      TransactionDbHelper.fromTransaction(DLCWalletUtil.dummyPrevTx, None)

    verifyDatabaseInsertion(tx, tx.txIdBE, remoteTxDAO, daos.dlcDAO)
  }
}
