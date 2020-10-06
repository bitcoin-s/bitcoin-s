package org.bitcoins.dlc.oracle

import java.sql.SQLException

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.testkit.core.gen.ChainParamsGenerator
import org.bitcoins.testkit.fixtures.DLCOracleFixture

class DLCOracleTest extends DLCOracleFixture {

  val testOutcomes: Vector[String] = (0 to 10).map(_.toString).toVector

  behavior of "DLCOracle"

  it must "correctly initialize" in { dlcOracle: DLCOracle =>
    assert(dlcOracle.conf.exists())
  }

  it must "start with no events" in { dlcOracle: DLCOracle =>
    dlcOracle.listEventDbs().map { events =>
      assert(events.isEmpty)
    }
  }

  it must "start with no pending events" in { dlcOracle: DLCOracle =>
    dlcOracle.listPendingEventDbs().map { events =>
      assert(events.isEmpty)
    }
  }

  it must "calculate the correct staking address" in { dlcOracle: DLCOracle =>
    forAllAsync(ChainParamsGenerator.bitcoinNetworkParams) { network =>
      val expected =
        Bech32Address(P2WPKHWitnessSPKV0(dlcOracle.publicKey.publicKey),
                      network)
      assert(dlcOracle.stakingAddress(network) == expected)
    }
  }

  it must "create a new event and list it with pending" in {
    dlcOracle: DLCOracle =>
      val time = TimeUtil.now
      for {
        testEventDb <- dlcOracle.createNewEvent("test", time, testOutcomes)
        pendingEvents <- dlcOracle.listPendingEventDbs()
      } yield {
        assert(pendingEvents.size == 1)
        // encoding of the time can make them unequal
        val comparable =
          pendingEvents.head.copy(maturationTime = testEventDb.maturationTime)
        assert(comparable == testEventDb)
      }
  }

  it must "create a new event and get its details" in { dlcOracle: DLCOracle =>
    val time = TimeUtil.now
    val eventName = "test"

    for {
      testEventDb <- dlcOracle.createNewEvent(eventName, time, testOutcomes)
      eventOpt <- dlcOracle.getEvent(testEventDb.nonce)
    } yield {
      assert(eventOpt.isDefined)
      val event = eventOpt.get

      assert(event.isInstanceOf[PendingEvent])
      assert(event.eventName == eventName)
      assert(event.outcomes == testOutcomes)
      assert(event.numOutcomes == testOutcomes.size)
      assert(event.signingVersion == SigningVersion.latest)
      assert(event.pubkey == dlcOracle.publicKey)
      assert(event.nonce == testEventDb.nonce)
      assert(event.maturationTime.getEpochSecond == time.getEpochSecond)
    }
  }

  it must "not get an event that doesn't exit" in { dlcOracle: DLCOracle =>
    val nonce = ECPublicKey.freshPublicKey.schnorrNonce
    dlcOracle.getEvent(nonce).map {
      case None    => succeed
      case Some(_) => fail()
    }
  }

  it must "create a new event with a valid commitment signature" in {
    dlcOracle: DLCOracle =>
      for {
        testEventDb <-
          dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
        rValDbOpt <- dlcOracle.rValueDAO.read(testEventDb.nonce)
      } yield {
        assert(rValDbOpt.isDefined)
        val rValDb = rValDbOpt.get
        val hash = CryptoUtil.taggedSha256(
          rValDb.nonce.bytes ++ CryptoUtil.serializeForHash(rValDb.eventName),
          "DLCv0/Commitment")
        assert(
          dlcOracle.publicKey.verify(hash.bytes, rValDb.commitmentSignature))
      }
  }

  it must "create multiple events with different names" in {
    dlcOracle: DLCOracle =>
      for {
        _ <- dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
        _ <- dlcOracle.createNewEvent("test1", TimeUtil.now, testOutcomes)
      } yield succeed
  }

  it must "fail to create multiple events with the same name" in {
    dlcOracle: DLCOracle =>
      recoverToSucceededIf[SQLException] {
        for {
          _ <- dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
          _ <- dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
        } yield ()
      }
  }

  it must "create and sign a event" in { dlcOracle: DLCOracle =>
    val outcome = testOutcomes.head
    for {
      eventDb <- dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
      signedEventDb <- dlcOracle.signEvent(eventDb.nonce, outcome)
      outcomeDbs <- dlcOracle.eventOutcomeDAO.findByNonce(eventDb.nonce)
      outcomeDb = outcomeDbs.find(_.message == outcome).get
      eventOpt <- dlcOracle.getEvent(eventDb.nonce)
    } yield {
      assert(eventOpt.isDefined)
      val event = eventOpt.get
      val sig = signedEventDb.sigOpt.get

      event match {
        case completedEvent: CompletedEvent =>
          assert(completedEvent.attestation == sig.sig)
          assert(dlcOracle.publicKey.verify(outcomeDb.hashedMessage.bytes, sig))
          assert(
            SchnorrDigitalSignature(completedEvent.nonce,
                                    completedEvent.attestation) == sig)
        case _: PendingEvent =>
          fail()
      }
    }
  }

  it must "correctly track pending events" in { dlcOracle: DLCOracle =>
    val outcome = testOutcomes.head
    for {
      eventDb <- dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
      beforePending <- dlcOracle.listPendingEventDbs()
      beforeEvents <- dlcOracle.listEvents()
      _ = assert(beforePending.size == 1)
      _ = assert(beforeEvents.size == 1)
      _ = assert(beforeEvents.head.isInstanceOf[PendingEvent])
      _ <- dlcOracle.signEvent(eventDb.nonce, outcome)
      afterPending <- dlcOracle.listPendingEventDbs()
      afterEvents <- dlcOracle.listEvents()
    } yield {
      assert(afterPending.isEmpty)
      assert(afterEvents.size == 1)
      assert(afterEvents.head.isInstanceOf[CompletedEvent])
    }
  }

  it must "fail to sign an event that doesn't exist" in {
    dlcOracle: DLCOracle =>
      val dummyNonce = SchnorrNonce(ECPublicKey.freshPublicKey.bytes.tail)
      recoverToSucceededIf[RuntimeException](
        dlcOracle.signEvent(dummyNonce, "testOutcomes"))
  }

  it must "fail to sign an outcome that doesn't exist" in {
    dlcOracle: DLCOracle =>
      recoverToSucceededIf[RuntimeException] {
        for {
          eventDb <-
            dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
          _ <- dlcOracle.signEvent(eventDb.nonce, "not a real outcome")
        } yield ()
      }
  }

  it must "fail to sign an event with an outside nonce" in {
    dlcOracle: DLCOracle =>
      val ecKey = ECPublicKey.freshPublicKey
      val publicKey = ecKey.schnorrPublicKey
      val nonce = ecKey.schnorrNonce

      val eventName = "dummy"
      val sigVersion = SigningVersion.latest
      val message = "dummy message"

      val rValDb = RValueDb(nonce,
                            eventName,
                            HDPurpose(0),
                            HDCoinType.Bitcoin,
                            0,
                            0,
                            0,
                            SchnorrDigitalSignature(nonce, FieldElement.one))

      val eventDb =
        EventDb(nonce, publicKey, eventName, 1, sigVersion, TimeUtil.now, None)

      val outcomeDb =
        EventOutcomeDb(nonce,
                       message,
                       CryptoUtil.taggedSha256(message, sigVersion.outcomeTag))

      val setupF = for {
        _ <- dlcOracle.rValueDAO.create(rValDb)
        _ <- dlcOracle.eventDAO.create(eventDb)
        _ <- dlcOracle.eventOutcomeDAO.create(outcomeDb)
      } yield ()

      recoverToSucceededIf[IllegalArgumentException] {
        for {
          _ <- setupF
          _ <- dlcOracle.signEvent(nonce, message)
        } yield ()
      }
  }

  it must "fail to create an event with no outcomes" in {
    dlcOracle: DLCOracle =>
      assertThrows[IllegalArgumentException] {
        dlcOracle.createNewEvent("test", TimeUtil.now, Vector.empty)
      }
  }

  it must "fail to create an event with duplicate outcomes" in {
    dlcOracle: DLCOracle =>
      val outcomes = testOutcomes :+ testOutcomes.head
      assertThrows[IllegalArgumentException] {
        dlcOracle.createNewEvent("test", TimeUtil.now, outcomes)
      }
  }
}
