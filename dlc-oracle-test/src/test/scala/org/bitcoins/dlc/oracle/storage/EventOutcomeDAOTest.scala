package org.bitcoins.dlc.oracle.storage

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.DLCOracleAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.DLCOracleDAOFixture

class EventOutcomeDAOTest extends DLCOracleDAOFixture {

  behavior of "EventOutcomeDAO"

  val ecKey: ECPublicKey = ECPublicKey.freshPublicKey
  val publicKey: SchnorrPublicKey = ecKey.schnorrPublicKey
  val nonce: SchnorrNonce = ecKey.schnorrNonce

  val eventName = "dummy"
  val sigVersion: SigningVersion = SigningVersion.latest
  val message = "dummy message"

  val hash: Sha256Digest =
    CryptoUtil.taggedSha256(message, sigVersion.outcomeTag)

  val time: Instant = {
    // Need to do this so it is comparable to the db representation
    val now = TimeUtil.now.getEpochSecond
    Instant.ofEpochSecond(now)
  }

  val dummyRValDb: RValueDb = RValueDb(
    nonce,
    eventName,
    HDPurpose(0),
    HDCoinType.Bitcoin,
    0,
    0,
    0,
    SchnorrDigitalSignature(nonce, FieldElement.one))

  val dummyEventDb: EventDb =
    EventDb(nonce, publicKey, eventName, 1, sigVersion, time, None)

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
    val outcomeDb1 =
      EventOutcomeDb(nonce,
                     "message",
                     CryptoUtil.taggedSha256("message", sigVersion.outcomeTag))

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
