package org.bitcoins.dlc.oracle.storage

import java.time.Instant
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.core.protocol.dlc.SigningVersion
import org.bitcoins.core.protocol.tlv.EventDescriptorTLV
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.testkit.Implicits.GeneratorOps
import org.bitcoins.testkit.core.gen.TLVGen
import org.bitcoins.testkit.fixtures.DLCOracleDAOFixture
import scodec.bits.ByteVector

class EventOutcomeDAOTest extends DLCOracleDAOFixture {

  behavior of "EventOutcomeDAO"

  val ecKey: ECPublicKey = ECPublicKey.freshPublicKey
  val publicKey: SchnorrPublicKey = ecKey.schnorrPublicKey
  val nonce: SchnorrNonce = ecKey.schnorrNonce

  val eventName = "dummy"
  val sigVersion: SigningVersion = SigningVersion.latest
  val message = "dummy message"

  val hash: ByteVector = CryptoUtil.sha256(message).bytes

  val time: Instant = {
    // Need to do this so it is comparable to the db representation
    val now = TimeUtil.now.getEpochSecond
    Instant.ofEpochSecond(now)
  }

  val dummyRValDb: RValueDb =
    RValueDb(nonce, eventName, HDPurpose(0), HDCoinType.Bitcoin, 0, 0, 0)

  def descriptor: EventDescriptorTLV = TLVGen.eventDescriptorTLV.sampleSome

  val dummyEventDb: EventDb =
    EventDb(nonce,
            publicKey,
            0,
            eventName,
            1,
            sigVersion,
            time,
            None,
            None,
            SchnorrDigitalSignature(nonce, FieldElement.one),
            descriptor)

  it must "create an EventOutcomeDb and read it" in { daos =>
    val rValDAO = daos.rValueDAO
    val eventDAO = daos.eventDAO
    val outcomeDAO = daos.outcomeDAO

    val outcomeDb = EventOutcomeDb(nonce, message, hash)

    for {
      _ <- rValDAO.create(dummyRValDb)
      _ <- eventDAO.create(dummyEventDb)
      _ <- outcomeDAO.create(outcomeDb)
      read <- outcomeDAO.read((nonce, message))
    } yield assert(read.contains(outcomeDb))
  }

  it must "create an EventOutcomeDb and find all" in { daos =>
    val rValDAO = daos.rValueDAO
    val eventDAO = daos.eventDAO
    val outcomeDAO = daos.outcomeDAO

    val outcomeDb = EventOutcomeDb(nonce, message, hash)

    for {
      _ <- rValDAO.create(dummyRValDb)
      _ <- eventDAO.create(dummyEventDb)
      _ <- outcomeDAO.create(outcomeDb)
      all <- outcomeDAO.findAll()
    } yield assert(all.contains(outcomeDb))
  }

  it must "create EventOutcomeDbs and find by nonce" in { daos =>
    val rValDAO = daos.rValueDAO
    val eventDAO = daos.eventDAO
    val outcomeDAO = daos.outcomeDAO

    val outcomeDb = EventOutcomeDb(nonce, message, hash)
    val bytes = CryptoUtil.sha256("message").bytes
    val outcomeDb1 = EventOutcomeDb(nonce, "message", bytes)

    for {
      _ <- rValDAO.create(dummyRValDb)
      _ <- eventDAO.create(dummyEventDb)
      _ <- outcomeDAO.createAll(Vector(outcomeDb, outcomeDb1))
      all <- outcomeDAO.findByNonce(nonce)
    } yield {
      assert(all.size == 2)
      assert(all.contains(outcomeDb))
      assert(all.contains(outcomeDb1))
    }
  }
}
