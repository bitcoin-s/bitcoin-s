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
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, DLCWalletTestUtil}

class DLCDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  behavior of "DLCDAO"

  val dlcDb: DLCDb = DLCWalletTestUtil.sampleDLCDb

  val eventId: Sha256DigestBE = dlcDb.eventId

  it should "correctly insert a DLC into the database" in { daos =>
    val dlcDAO = daos.dlcDAO

    for {
      _ <- dlcDAO.create(dlcDb)
      readDLC <- dlcDAO.read(eventId)
    } yield {
      assert(readDLC.contains(dlcDb))
    }
  }

  it should "correctly insert a DLCOffer into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val offerDAO = daos.dlcOfferDAO

    val offer = DLCWalletTestUtil.sampleDLCOffer

    for {
      _ <- dlcDAO.create(dlcDb)
      offerDb = DLCOfferDb.fromDLCOffer(offer, RegTest)
      _ <- offerDAO.create(offerDb)

      readOffer <- offerDAO.read(eventId)
    } yield {
      assert(
        readOffer.get.toDLCOffer(offer.fundingInputs).toJson == offer.toJson)
    }
  }

  it should "correctly insert a DLCAccept into the database" in { daos =>
    val dlcDAO = daos.dlcDAO
    val acceptDAO = daos.dlcAcceptDAO

    val accept = DLCWalletTestUtil.sampleDLCAccept

    for {
      _ <- dlcDAO.create(dlcDb)
      acceptDb = DLCAcceptDb.fromDLCAccept(accept)
      _ <- acceptDAO.create(acceptDb)

      readAccept <- acceptDAO.read(eventId)
    } yield {
      assert(
        readAccept.get
          .toDLCAccept(accept.fundingInputs)
          .toJson == accept.toJson)
    }
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
