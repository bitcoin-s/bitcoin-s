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
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, DLCWalletUtil}
import org.scalatest.Assertion

import scala.concurrent.Future

class DLCDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  behavior of "DLCDAO"

  val dlcDb: DLCDb = DLCWalletUtil.sampleDLCDb

  val eventId: Sha256DigestBE = dlcDb.eventId

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
    verifyDatabaseInsertion(dlcDb, eventId, dlcDAO, dlcDAO)
  }

  it should "correctly insert a DLCOffer into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val offerDAO = daos.dlcOfferDAO

    val offerDb =
      DLCOfferDb.fromDLCOffer(DLCWalletUtil.sampleDLCOffer, RegTest)

    verifyDatabaseInsertion(offerDb, eventId, offerDAO, dlcDAO)
  }

  it should "correctly insert a DLCAccept into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val acceptDAO = daos.dlcAcceptDAO

    val acceptDb = DLCAcceptDb.fromDLCAccept(DLCWalletUtil.sampleDLCAccept)

    verifyDatabaseInsertion(acceptDb, eventId, acceptDAO, dlcDAO)
  }

  it should "correctly insert funding inputs into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val inputsDAO = daos.dlcInputsDAO

    val input = DLCFundingInputDb(
      eventId = eventId,
      isInitiator = true,
      outPoint = TransactionOutPoint(testBlockHash, UInt32.zero),
      output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
      sigs = Vector(DLCWalletUtil.dummyPartialSig)
    )

    verifyDatabaseInsertion(input, input.outPoint, inputsDAO, dlcDAO)
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
          sigs = Vector(DLCWalletUtil.dummyPartialSig)
        ),
        DLCFundingInputDb(
          eventId = eventId,
          isInitiator = false,
          outPoint = TransactionOutPoint(testBlockHash, UInt32.one),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          sigs = Vector(DLCWalletUtil.dummyPartialSig)
        ),
        DLCFundingInputDb(
          eventId = eventId,
          isInitiator = true,
          outPoint = TransactionOutPoint(testBlockHash, UInt32(3)),
          output = TransactionOutput(Satoshis.one, EmptyScriptPubKey),
          sigs = Vector(DLCWalletUtil.dummyPartialSig)
        )
      )

      for {
        _ <- dlcDAO.create(dlcDb)
        _ <- inputsDAO.createAll(inputs)

        readInput <- inputsDAO.findByEventId(eventId, isInitiator = true)
      } yield assert(readInput.size == 2)
  }
}
