package org.bitcoins.wallet.models

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.db.CRUD
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, DLCWalletTestUtil}
import org.scalatest.Assertion

import scala.concurrent.Future

class DLCDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  behavior of "DLCDAO"

  val dlcDb: DLCDb = DLCWalletTestUtil.sampleDLCDb

  val eventId: Sha256DigestBE = dlcDb.eventId

  def verifyDatabaseInsertion[ElementType](
      element: ElementType,
      dao: CRUD[ElementType, Sha256DigestBE],
      dlcDAO: DLCDAO): Future[Assertion] = {
    for {
      _ <- dlcDAO.create(dlcDb)
      _ <- dao.upsert(element) //upsert in case we are testing the dlcDAO

      read <- dao.read(eventId)
    } yield {
      assert(read.contains(element))
    }
  }

  it should "correctly insert a DLC into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    verifyDatabaseInsertion(dlcDb, dlcDAO, dlcDAO)
  }

  it should "correctly insert a DLCOffer into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val offerDAO = daos.dlcOfferDAO

    val offerDb =
      DLCOfferDb.fromDLCOffer(DLCWalletTestUtil.sampleDLCOffer, RegTest)

    verifyDatabaseInsertion(offerDb, offerDAO, dlcDAO)
  }

  it should "correctly insert a DLCAccept into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val acceptDAO = daos.dlcAcceptDAO

    val acceptDb = DLCAcceptDb.fromDLCAccept(DLCWalletTestUtil.sampleDLCAccept)

    verifyDatabaseInsertion(acceptDb, acceptDAO, dlcDAO)
  }

  it should "correctly insert funding inputs into the database" in { daos =>
    val inputsDAO = daos.dlcInputsDAO
    val dlcDAO = daos.dlcDAO

    val input = DLCFundingInputDb(
      eventId = eventId,
      isInitiator = true,
      outPoint = TransactionOutPoint(testBlockHash, UInt32.zero),
      output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
      sigs = Vector(DLCWalletTestUtil.dummyPartialSig)
    )

    for {
      _ <- dlcDAO.create(dlcDb)
      _ <- inputsDAO.create(input)

      readInput <- inputsDAO.findByEventId(eventId)
    } yield assert(readInput.head.toString == input.toString)
  }

  it should "correctly find funding inputs by eventId and isInitiator" in {
    daos =>
      val inputsDAO = daos.dlcInputsDAO
      val dlcDAO = daos.dlcDAO

      val inputs = Vector(
        DLCFundingInputDb(
          eventId = eventId,
          isInitiator = true,
          outPoint = TransactionOutPoint(testBlockHash, UInt32.zero),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          sigs = Vector(DLCWalletTestUtil.dummyPartialSig)
        ),
        DLCFundingInputDb(
          eventId = eventId,
          isInitiator = false,
          outPoint = TransactionOutPoint(testBlockHash, UInt32.one),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          sigs = Vector(DLCWalletTestUtil.dummyPartialSig)
        ),
        DLCFundingInputDb(
          eventId = eventId,
          isInitiator = true,
          outPoint = TransactionOutPoint(testBlockHash, UInt32(3)),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          sigs = Vector(DLCWalletTestUtil.dummyPartialSig)
        )
      )

      for {
        _ <- dlcDAO.create(dlcDb)
        _ <- inputsDAO.createAll(inputs)

        readInput <- inputsDAO.findByEventId(eventId, isInitiator = true)
      } yield assert(readInput.size == 2)
  }
}
