package org.bitcoins.dlc.oracle.storage

import java.time.Instant
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.core.protocol.dlc.SigningVersion
import org.bitcoins.core.protocol.tlv.EventDescriptorTLV
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.TLVGen
import org.bitcoins.testkit.fixtures.DLCOracleDAOFixture

class EventDAOTest extends DLCOracleDAOFixture {

  behavior of "EventDAO"

  val ecKey: ECPublicKey = ECPublicKey.freshPublicKey
  val publicKey: SchnorrPublicKey = ecKey.schnorrPublicKey
  val nonce: SchnorrNonce = ecKey.schnorrNonce

  val eventName = "dummy"
  val sigVersion: SigningVersion = SigningVersion.latest
  val message = "dummy message"

  val time: Instant = {
    // Need to do this so it is comparable to the db representation
    val now = TimeUtil.now.getEpochSecond
    Instant.ofEpochSecond(now)
  }

  val dummyRValDb: RValueDb =
    RValueDb(nonce, eventName, HDPurpose(0), HDCoinType.Bitcoin, 0, 0, 0)

  val dummySig: SchnorrDigitalSignature =
    SchnorrDigitalSignature(nonce, FieldElement.one)

  def descriptor: EventDescriptorTLV = TLVGen.eventDescriptorTLV.sampleSome

  it must "create an EventDb and read it" in { daos =>
    val rValDAO = daos.rValueDAO
    val eventDAO = daos.eventDAO

    val eventDb =
      EventDb(nonce,
              publicKey,
              0,
              eventName,
              0,
              sigVersion,
              time,
              None,
              dummySig,
              descriptor)

    for {
      _ <- rValDAO.create(dummyRValDb)
      _ <- eventDAO.create(eventDb)
      read <- eventDAO.read(nonce)
    } yield assert(read.contains(eventDb))
  }

  it must "create an EventDb and find all" in { daos =>
    val rValDAO = daos.rValueDAO
    val eventDAO = daos.eventDAO

    val eventDb =
      EventDb(nonce,
              publicKey,
              0,
              eventName,
              0,
              sigVersion,
              time,
              None,
              dummySig,
              descriptor)

    for {
      _ <- rValDAO.create(dummyRValDb)
      _ <- eventDAO.create(eventDb)
      all <- eventDAO.findAll()
    } yield assert(all.contains(eventDb))
  }
}
