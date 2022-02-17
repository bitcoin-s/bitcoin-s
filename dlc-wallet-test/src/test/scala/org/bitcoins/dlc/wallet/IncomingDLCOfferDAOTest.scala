package org.bitcoins.dlc.wallet

import org.bitcoins.dlc.wallet.models.IncomingDLCOfferDbHelper
import org.bitcoins.testkit.fixtures.DLCDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, DLCWalletUtil}

import java.time.Instant

class IncomingDLCOfferDAOTest extends BitcoinSWalletTest with DLCDAOFixture {

  behavior of "IncomingDLCOfferDAO"

  it should "write and read incoming offers" in { daos =>
    val expected =
      IncomingDLCOfferDbHelper.fromTLV(offerTLV =
                                         DLCWalletUtil.sampleDLCOffer.toTLV,
                                       message = Some("msg"),
                                       peer = Some("peer"),
                                       receivedAt = Instant.ofEpochSecond(0))

    for {
      _ <- daos.incomingDLCOfferDAO.create(expected)
      actual <- daos.incomingDLCOfferDAO.read(expected.hash)
    } yield {
      assert(actual.nonEmpty)
      assert(actual.get == expected)
    }
  }

  it should "select incoming offers in reverse chronological order" in { daos =>
    val offer1 =
      IncomingDLCOfferDbHelper.fromTLV(offerTLV =
                                         DLCWalletUtil.sampleDLCOffer.toTLV,
                                       message = Some("msg"),
                                       peer = Some("peer"),
                                       receivedAt = Instant.ofEpochSecond(0))
    val offer2 = IncomingDLCOfferDbHelper.fromTLV(
      offerTLV = DLCWalletUtil.sampleMultiNonceDLCOffer.toTLV,
      message = Some("msg"),
      peer = Some("peer"),
      receivedAt = Instant.ofEpochSecond(1))

    val offers = Vector(offer1, offer2)
    for {
      _ <- daos.incomingDLCOfferDAO.createAll(offers)
      actual <- daos.incomingDLCOfferDAO.findAll()
    } yield {
      assert(actual.nonEmpty)
      val expected = offers.sortBy(_.receivedAt.getEpochSecond).reverse
      assert(actual == expected)
    }
  }

}
