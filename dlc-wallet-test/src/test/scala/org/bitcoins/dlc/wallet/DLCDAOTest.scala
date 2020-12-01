package org.bitcoins.dlc.wallet

import org.bitcoins.core.api.wallet.db.TransactionDbHelper
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.tlv.{EnumOutcome, UnsignedNumericOutcome}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.crypto.{ECAdaptorSignature, Sha256DigestBE}
import org.bitcoins.db.CRUD
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.testkit.fixtures.DLCDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, DLCWalletUtil}
import org.scalatest.Assertion

import scala.concurrent.Future

class DLCDAOTest extends BitcoinSWalletTest with DLCDAOFixture {

  behavior of "DLCDAO"

  val dlcDb: DLCDb = DLCWalletUtil.sampleDLCDb

  val paramHash: Sha256DigestBE = dlcDb.paramHash

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
    verifyDatabaseInsertion(dlcDb, paramHash, dlcDAO, dlcDAO)
  }

  it should "correctly insert a DLCOffer into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val offerDAO = daos.dlcOfferDAO

    val offerDb =
      DLCOfferDbHelper.fromDLCOffer(DLCWalletUtil.sampleDLCOffer)

    verifyDatabaseInsertion(offerDb, paramHash, offerDAO, dlcDAO)
  }

  it should "correctly insert a DLCAccept into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val acceptDAO = daos.dlcAcceptDAO

    val acceptDb =
      DLCAcceptDbHelper.fromDLCAccept(paramHash, DLCWalletUtil.sampleDLCAccept)

    verifyDatabaseInsertion(acceptDb, paramHash, acceptDAO, dlcDAO)
  }

  it should "correctly insert funding inputs into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val inputsDAO = daos.dlcInputsDAO

    val input = DLCFundingInputDb(
      paramHash = paramHash,
      isInitiator = true,
      outPoint = TransactionOutPoint(testBlockHash, UInt32.zero),
      output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
      redeemScriptOpt = None,
      witnessScriptOpt = Some(DLCWalletUtil.dummyScriptWitness)
    )

    verifyDatabaseInsertion(input, input.outPoint, inputsDAO, dlcDAO)
  }

  it should "correctly find funding inputs by eventId and isInitiator" in {
    daos =>
      val inputsDAO = daos.dlcInputsDAO
      val dlcDAO = daos.dlcDAO

      val inputs = Vector(
        DLCFundingInputDb(
          paramHash = paramHash,
          isInitiator = true,
          outPoint = TransactionOutPoint(testBlockHash, UInt32.zero),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          redeemScriptOpt = None,
          witnessScriptOpt = Some(DLCWalletUtil.dummyScriptWitness)
        ),
        DLCFundingInputDb(
          paramHash = paramHash,
          isInitiator = false,
          outPoint = TransactionOutPoint(testBlockHash, UInt32.one),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          redeemScriptOpt = None,
          witnessScriptOpt = Some(DLCWalletUtil.dummyScriptWitness)
        ),
        DLCFundingInputDb(
          paramHash = paramHash,
          isInitiator = true,
          outPoint = TransactionOutPoint(testBlockHash, UInt32(3)),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          redeemScriptOpt = None,
          witnessScriptOpt = Some(DLCWalletUtil.dummyScriptWitness)
        )
      )

      for {
        _ <- dlcDAO.create(dlcDb)
        _ <- inputsDAO.createAll(inputs)

        readInput <- inputsDAO.findByParamHash(paramHash, isInitiator = true)
      } yield assert(readInput.size == 2)
  }

  it should "correctly insert enum outcome CET signatures into the database" in {
    daos =>
      val dlcDAO = daos.dlcDAO
      val sigsDAO = daos.dlcSigsDAO

      val sig = DLCCETSignatureDb(
        paramHash = paramHash,
        isInitiator = true,
        outcome = EnumOutcome(DLCWalletUtil.winStr),
        signature = ECAdaptorSignature.dummy
      )

      verifyDatabaseInsertion(sig,
                              (sig.paramHash, sig.outcome),
                              sigsDAO,
                              dlcDAO)
  }

  it should "correctly insert unsigned numeric outcome CET signatures into the database" in {
    daos =>
      val dlcDAO = daos.dlcDAO
      val sigsDAO = daos.dlcSigsDAO

      val outcomes = 0.to(100).toVector

      val sig = DLCCETSignatureDb(
        paramHash = paramHash,
        isInitiator = true,
        outcome = UnsignedNumericOutcome(outcomes),
        signature = ECAdaptorSignature.dummy
      )

      verifyDatabaseInsertion(sig,
                              (sig.paramHash, sig.outcome),
                              sigsDAO,
                              dlcDAO)
  }

  it should "correctly find CET signatures by eventId" in { daos =>
    val dlcDAO = daos.dlcDAO
    val sigsDAO = daos.dlcSigsDAO

    val sigs = Vector(
      DLCCETSignatureDb(
        paramHash = paramHash,
        isInitiator = true,
        outcome = EnumOutcome(DLCWalletUtil.winStr),
        signature = ECAdaptorSignature.dummy
      ),
      DLCCETSignatureDb(
        paramHash = paramHash,
        isInitiator = false,
        outcome = EnumOutcome(DLCWalletUtil.loseStr),
        signature = ECAdaptorSignature.dummy
      )
    )

    for {
      _ <- dlcDAO.create(dlcDb)
      _ <- sigsDAO.createAll(sigs)

      readInput <- sigsDAO.findByParamHash(paramHash)
    } yield {
      assert(readInput.size == 2)
      // Do it this way so ordering doesn't matter
      assert(readInput.contains(sigs.head))
      assert(readInput.contains(sigs.last))
    }
  }

  it should "correctly insert txs into the database" in { daos =>
    val remoteTxDAO = daos.dlcRemoteTxDAO

    val tx = TransactionDbHelper.fromTransaction(DLCWalletUtil.dummyPrevTx)

    verifyDatabaseInsertion(tx, tx.txIdBE, remoteTxDAO, daos.dlcDAO)
  }
}
