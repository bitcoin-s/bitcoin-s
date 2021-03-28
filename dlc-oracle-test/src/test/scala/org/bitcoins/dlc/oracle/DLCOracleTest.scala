package org.bitcoins.dlc.oracle

import com.typesafe.config.ConfigFactory
import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.api.dlcoracle.db._
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.SigningVersion
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.testkit.fixtures.DLCOracleFixture
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.{ChainParamsGenerator, TLVGen}

import java.time.Instant

class DLCOracleTest extends DLCOracleFixture {

  val enumOutcomes: Vector[String] = Vector("sunny", "windy", "rainy", "cloudy")

  val futureTime: Instant = TimeUtil.now.plusSeconds(100000)

  val testDescriptor: EnumEventDescriptorV0TLV = EnumEventDescriptorV0TLV(
    enumOutcomes)

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

  it must "not find an event it doesn't have" in { dlcOracle: DLCOracle =>
    val dummyEvent = TLVGen.oracleEventV0TLV.sampleSome
    dlcOracle.findEvent(dummyEvent).map { eventOpt =>
      assert(eventOpt.isEmpty)
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

  it must "get the correctly sorted nonces in an announcement " in {
    dlcOracle: DLCOracle =>
      val eventName = "test"
      val descriptorTLV =
        DigitDecompositionEventDescriptorV0TLV(base = UInt16(2),
                                               isSigned = false,
                                               numDigits = 3,
                                               unit = "units",
                                               precision = Int32.zero)

      for {
        announcement <- dlcOracle.createNewEvent(eventName = eventName,
                                                 maturationTime = futureTime,
                                                 descriptorTLV)

        // To get around foreign key, won't be needed
        _ <- dlcOracle.eventOutcomeDAO.deleteAll()

        eventDbs <- dlcOracle.eventDAO.findByEventName(eventName)
        _ <- dlcOracle.eventDAO.deleteAll()

        unsortedDbs = eventDbs.reverse
        _ <- dlcOracle.eventDAO.createAll(unsortedDbs)

        eventOpt <- dlcOracle.findEvent(eventName)
      } yield {
        eventOpt match {
          case Some(event) =>
            assert(announcement == event.announcementTLV)
          case None => fail()
        }
      }
  }

  it must "have same keys with different network configs" in {
    oracleA: DLCOracle =>
      // set to mainnet and give separate db
      val newConf = oracleA.conf.newConfigOfType(
        Vector(ConfigFactory.parseString("bitcoin-s.network = mainnet"),
               ConfigFactory.parseString("bitcoin-s.oracle.db.name = oracle1")))

      newConf.initialize().flatMap { oracleB =>
        assert(oracleA.publicKey == oracleB.publicKey)

        val eventName = "test"
        val descriptorTLV =
          DigitDecompositionEventDescriptorV0TLV(base = UInt16(2),
                                                 isSigned = false,
                                                 numDigits = 3,
                                                 unit = "units",
                                                 precision = Int32.zero)

        for {
          announcementA <- oracleA.createNewEvent(eventName = eventName,
                                                  maturationTime = futureTime,
                                                  descriptorTLV)
          announcementB <- oracleB.createNewEvent(eventName = eventName,
                                                  maturationTime = futureTime,
                                                  descriptorTLV)

          // Can't compare announcementTLV because different nonces might be used for signature
          _ = assert(announcementA.publicKey == announcementB.publicKey)
          _ = assert(announcementA.eventTLV == announcementB.eventTLV)

          eventA <- oracleA.signDigits(eventName, 1)
          eventB <- oracleB.signDigits(eventName, 1)
        } yield {
          (eventA, eventB) match {
            case (completedA: CompletedDigitDecompositionV0OracleEvent,
                  completedB: CompletedDigitDecompositionV0OracleEvent) =>
              assert(
                completedA.oracleAttestmentV0TLV == completedB.oracleAttestmentV0TLV)
            case (_, _) =>
              fail("Unexpected outcome")
          }
        }
      }
  }

  it must "create a new event and list it with pending" in {
    dlcOracle: DLCOracle =>
      val time = futureTime

      for {
        _ <- dlcOracle.createNewEvent("test", time, testDescriptor)
        pendingEvents <- dlcOracle.listPendingEventDbs()
      } yield {
        assert(pendingEvents.size == 1)
        assert(pendingEvents.head.eventDescriptorTLV == testDescriptor)
      }
  }

  it must "create the same event twice and list them" in {
    dlcOracle: DLCOracle =>
      val time = futureTime

      for {
        _ <- dlcOracle.createNewEvent("test", time, testDescriptor)
        _ <- dlcOracle.createNewEvent("test2", time, testDescriptor)
        events <- dlcOracle.listEvents()
      } yield {
        assert(events.size == 2)
        assert(events.forall(_.eventDescriptorTLV == testDescriptor))
      }
  }

  it must "fail to create an event with the same name" in {
    dlcOracle: DLCOracle =>
      for {
        _ <- dlcOracle.createNewEvent("test", futureTime, testDescriptor)
        res <- recoverToSucceededIf[IllegalArgumentException](
          dlcOracle.createNewEvent("test", futureTime, testDescriptor))
      } yield res
  }

  it must "create an enum new event and get its details" in {
    dlcOracle: DLCOracle =>
      val time = futureTime
      val eventName = "test"

      for {
        announcement <-
          dlcOracle.createNewEnumEvent(eventName, time, enumOutcomes)

        eventOpt <- dlcOracle.findEvent(announcement.eventTLV)
      } yield {
        assert(announcement.validateSignature)
        assert(eventOpt.isDefined)
        val event = eventOpt.get

        assert(event.isInstanceOf[PendingEnumV0OracleEvent])
        assert(event.eventName == eventName)
        assert(event.eventDescriptorTLV == testDescriptor)
        assert(event.signingVersion == SigningVersion.latest)
        assert(event.pubkey == dlcOracle.publicKey)
        assert(event.maturationTime.getEpochSecond == time.getEpochSecond)

        val expectedEventTLV =
          OracleEventV0TLV(Vector(event.nonces.head),
                           UInt32(event.maturationTime.getEpochSecond),
                           testDescriptor,
                           eventName)

        assert(event.eventTLV == expectedEventTLV)

        val expectedAnnouncementTLV =
          OracleAnnouncementV0TLV(event.announcementSignature,
                                  event.pubkey,
                                  expectedEventTLV)

        assert(event.announcementTLV == expectedAnnouncementTLV)

        val announceBytes =
          SigningVersion.latest.calcAnnouncementHash(event.eventTLV)

        assert(
          dlcOracle.publicKey.verify(announceBytes,
                                     event.announcementSignature))
      }
  }

  it must "create and sign an enum event" in { dlcOracle: DLCOracle =>
    val descriptor = TLVGen.enumEventDescriptorV0TLV.sampleSome
    val outcome = descriptor.outcomes.head

    val descriptorV0TLV =
      EnumEventDescriptorV0TLV(descriptor.outcomes)

    for {
      announcement <-
        dlcOracle.createNewEvent("test", futureTime, descriptorV0TLV)

      signedEventDb <-
        dlcOracle.signEnumEvent(announcement.eventTLV, EnumAttestation(outcome))
      eventOpt <- dlcOracle.findEvent(announcement.eventTLV)
    } yield {
      assert(eventOpt.isDefined)
      val event = eventOpt.get
      val sig = signedEventDb.sigOpt.get

      event match {
        case completedEvent: CompletedEnumV0OracleEvent =>
          assert(completedEvent.attestation == sig.sig)
          assert(completedEvent.outcomes == Vector(EnumAttestation(outcome)))

          val descriptor = completedEvent.eventDescriptorTLV
          val hash = SigningVersion.latest.calcOutcomeHash(descriptor, outcome)

          assert(dlcOracle.publicKey.verify(hash, sig))
          assert(
            SchnorrDigitalSignature(completedEvent.nonces.head,
                                    completedEvent.attestation) == sig)
          assert(
            OracleEvent.verifyAttestations(announcement,
                                           completedEvent.oracleAttestmentV0TLV,
                                           signingVersion =
                                             SigningVersion.latest))
        case _: PendingOracleEvent | _: CompletedOracleEvent =>
          fail()
      }
    }
  }

  it must "create and sign a large range event" in { dlcOracle: DLCOracle =>
    val outcome = -321L

    for {
      announcement <-
        dlcOracle.createNewDigitDecompEvent(eventName = "test",
                                            maturationTime = futureTime,
                                            base = UInt16(10),
                                            isSigned = true,
                                            numDigits = 3,
                                            unit = "units",
                                            precision = Int32.zero)

      _ = assert(announcement.validateSignature)

      eventTLV = announcement.eventTLV

      event <- dlcOracle.signDigits(eventTLV, outcome)
    } yield {
      event match {
        case completedEvent: CompletedDigitDecompositionV0OracleEvent =>
          val signOutcome = DigitDecompositionSignAttestation(outcome >= 0)
          val digitOutcomes = Vector(DigitDecompositionAttestation(3),
                                     DigitDecompositionAttestation(2),
                                     DigitDecompositionAttestation(1))
          assert(completedEvent.outcomes == signOutcome +: digitOutcomes)

          val descriptor = completedEvent.eventDescriptorTLV

          // Sign Signature Check
          val signHash = SigningVersion.latest.calcOutcomeHash(descriptor, "-")
          val signSig = completedEvent.signatures.head
          assert(dlcOracle.publicKey.verify(signHash, signSig))
          assert(
            SchnorrDigitalSignature(
              completedEvent.nonces.head,
              completedEvent.attestations.head) == signSig)

          // 100s Place signature Check
          val hash100 =
            SigningVersion.latest.calcOutcomeHash(
              descriptor,
              DigitDecompositionAttestation(3).bytes)
          val sig100 = completedEvent.signatures(1)
          assert(dlcOracle.publicKey.verify(hash100, sig100))
          assert(
            SchnorrDigitalSignature(completedEvent.nonces(1),
                                    completedEvent.attestations(1)) == sig100)

          // 10s Place signature Check
          val hash10 =
            SigningVersion.latest.calcOutcomeHash(
              descriptor,
              DigitDecompositionAttestation(2).bytes)
          val sig10 = completedEvent.signatures(2)
          assert(dlcOracle.publicKey.verify(hash10, sig10))
          assert(
            SchnorrDigitalSignature(completedEvent.nonces(2),
                                    completedEvent.attestations(2)) == sig10)

          // 1s Place signature Check
          val hash1 =
            SigningVersion.latest.calcOutcomeHash(
              descriptor,
              DigitDecompositionAttestation(1).bytes)
          val sig1 = completedEvent.signatures(3)
          assert(dlcOracle.publicKey.verify(hash1, sig1))
          assert(
            SchnorrDigitalSignature(completedEvent.nonces(3),
                                    completedEvent.attestations(3)) == sig1)
        case _: PendingOracleEvent | _: CompletedOracleEvent =>
          fail()
      }
    }
  }

  it must "create and sign a non-base 10 large range event" in {
    dlcOracle: DLCOracle =>
      val outcome = -1931L

      for {
        announcement <-
          dlcOracle.createNewDigitDecompEvent(eventName = "test",
                                              maturationTime = futureTime,
                                              base = UInt16(16),
                                              isSigned = true,
                                              numDigits = 3,
                                              unit = "units",
                                              precision = Int32.zero)

        _ = assert(announcement.validateSignature)

        eventTLV = announcement.eventTLV

        event <- dlcOracle.signDigits(eventTLV, outcome)
      } yield {
        event match {
          case completedEvent: CompletedDigitDecompositionV0OracleEvent =>
            val signOutcome = DigitDecompositionSignAttestation(outcome >= 0)
            val digitOutcomes = Vector(DigitDecompositionAttestation(7),
                                       DigitDecompositionAttestation(8),
                                       DigitDecompositionAttestation(11))
            assert(completedEvent.outcomes == signOutcome +: digitOutcomes)

            val descriptor = completedEvent.eventDescriptorTLV

            // Sign Signature Check
            val signHash =
              SigningVersion.latest.calcOutcomeHash(descriptor, "-")
            val signSig = completedEvent.signatures.head
            assert(dlcOracle.publicKey.verify(signHash, signSig))
            assert(
              SchnorrDigitalSignature(
                completedEvent.nonces.head,
                completedEvent.attestations.head) == signSig)

            // 100s Place signature Check
            val hash100 =
              SigningVersion.latest.calcOutcomeHash(
                descriptor,
                DigitDecompositionAttestation(7).bytes)
            val sig100 = completedEvent.signatures(1)
            assert(dlcOracle.publicKey.verify(hash100, sig100))
            assert(
              SchnorrDigitalSignature(completedEvent.nonces(1),
                                      completedEvent.attestations(1)) == sig100)

            // 10s Place signature Check
            val hash10 =
              SigningVersion.latest.calcOutcomeHash(
                descriptor,
                DigitDecompositionAttestation(8).bytes)
            val sig10 = completedEvent.signatures(2)
            assert(dlcOracle.publicKey.verify(hash10, sig10))
            assert(
              SchnorrDigitalSignature(completedEvent.nonces(2),
                                      completedEvent.attestations(2)) == sig10)

            // 1s Place signature Check
            val hash1 =
              SigningVersion.latest.calcOutcomeHash(
                descriptor,
                DigitDecompositionAttestation(11).bytes)
            val sig1 = completedEvent.signatures(3)
            assert(dlcOracle.publicKey.verify(hash1, sig1))
            assert(
              SchnorrDigitalSignature(completedEvent.nonces(3),
                                      completedEvent.attestations(3)) == sig1)
          case _: PendingOracleEvent | _: CompletedOracleEvent =>
            fail()
        }
      }
  }

  it must "create and sign a large range event with digits of 0" in {
    dlcOracle: DLCOracle =>
      val outcome = 2

      for {
        announcement <-
          dlcOracle.createNewDigitDecompEvent(eventName = "test",
                                              maturationTime = futureTime,
                                              base = UInt16(2),
                                              isSigned = false,
                                              numDigits = 3,
                                              unit = "units",
                                              precision = Int32.zero)

        _ = assert(announcement.validateSignature)

        eventTLV = announcement.eventTLV

        event <- dlcOracle.signDigits(eventTLV, outcome)
      } yield {
        event match {
          case completedEvent: CompletedDigitDecompositionV0OracleEvent =>
            val digitOutcomes = Vector(DigitDecompositionAttestation(0),
                                       DigitDecompositionAttestation(1),
                                       DigitDecompositionAttestation(0))
            assert(completedEvent.outcomes == digitOutcomes)

            val descriptor = completedEvent.eventDescriptorTLV

            // 100s Place signature Check
            val hash100 =
              SigningVersion.latest.calcOutcomeHash(
                descriptor,
                DigitDecompositionAttestation(0).bytes)
            val sig100 = completedEvent.signatures.head
            assert(dlcOracle.publicKey.verify(hash100, sig100))
            assert(
              SchnorrDigitalSignature(
                completedEvent.nonces.head,
                completedEvent.attestations.head) == sig100)

            // 10s Place signature Check
            val hash10 =
              SigningVersion.latest.calcOutcomeHash(
                descriptor,
                DigitDecompositionAttestation(1).bytes)
            val sig10 = completedEvent.signatures(1)
            assert(dlcOracle.publicKey.verify(hash10, sig10))
            assert(
              SchnorrDigitalSignature(completedEvent.nonces(1),
                                      completedEvent.attestations(1)) == sig10)

            // 1s Place signature Check
            val hash1 =
              SigningVersion.latest.calcOutcomeHash(
                descriptor,
                DigitDecompositionAttestation(0).bytes)
            val sig1 = completedEvent.signatures(2)
            assert(dlcOracle.publicKey.verify(hash1, sig1))
            assert(
              SchnorrDigitalSignature(completedEvent.nonces(2),
                                      completedEvent.attestations(2)) == sig1)
          case _: PendingOracleEvent | _: CompletedOracleEvent =>
            fail()
        }
      }
  }

  it must "correctly track pending events" in { dlcOracle: DLCOracle =>
    val outcome = enumOutcomes.head
    for {
      announcement <-
        dlcOracle.createNewEnumEvent("test", futureTime, enumOutcomes)
      beforePending <- dlcOracle.listPendingEventDbs()
      beforeEvents <- dlcOracle.listEvents()
      _ = assert(beforePending.size == 1)
      _ = assert(beforeEvents.size == 1)
      _ = assert(beforeEvents.head.isInstanceOf[PendingOracleEvent])

      nonce = announcement.eventTLV.nonces.head

      _ <- dlcOracle.signEvent(nonce, EnumAttestation(outcome))
      afterPending <- dlcOracle.listPendingEventDbs()
      afterEvents <- dlcOracle.listEvents()
    } yield {
      assert(afterPending.isEmpty)
      assert(afterEvents.size == 1)
      assert(afterEvents.head.isInstanceOf[CompletedOracleEvent])
    }
  }

  it must "fail to sign an event that doesn't exist" in {
    dlcOracle: DLCOracle =>
      val dummyNonce = SchnorrNonce(ECPublicKey.freshPublicKey.bytes.tail)
      recoverToSucceededIf[RuntimeException](
        dlcOracle.signEvent(dummyNonce, EnumAttestation("testOutcomes")))
  }

  it must "fail to sign an enum outcome that doesn't exist" in {
    dlcOracle: DLCOracle =>
      recoverToSucceededIf[RuntimeException] {
        for {
          announcement <-
            dlcOracle.createNewEnumEvent("test", futureTime, enumOutcomes)

          nonce = announcement.eventTLV.nonces.head

          _ <- dlcOracle.signEvent(nonce, EnumAttestation("not a real outcome"))
        } yield ()
      }
  }

  it must "fail to sign a negative number for a unsigned digit decomp event" in {
    dlcOracle: DLCOracle =>
      for {
        announcement <-
          dlcOracle.createNewDigitDecompEvent(eventName = "test",
                                              maturationTime = futureTime,
                                              base = UInt16(2),
                                              isSigned = false,
                                              numDigits = 3,
                                              unit = "units",
                                              precision = Int32.zero)

        res <- recoverToSucceededIf[IllegalArgumentException] {
          dlcOracle.signDigits(announcement.eventTLV, -2)
        }
      } yield res
  }

  it must "fail to sign an event with an outside nonce" in {
    dlcOracle: DLCOracle =>
      val ecKey = ECPublicKey.freshPublicKey
      val publicKey = ecKey.schnorrPublicKey
      val nonce = ecKey.schnorrNonce

      val eventName = "dummy"
      val sigVersion = SigningVersion.latest
      val message = "dummy message"

      val rValDb =
        RValueDb(nonce, eventName, HDPurpose(0), HDCoinType.Bitcoin, 0, 0, 0)
      val eventDb =
        EventDb(nonce,
                publicKey,
                0,
                eventName,
                0,
                sigVersion,
                futureTime,
                None,
                None,
                SchnorrDigitalSignature(nonce, FieldElement.one),
                testDescriptor)

      val setupF = for {
        _ <- dlcOracle.rValueDAO.create(rValDb)
        _ <- dlcOracle.eventDAO.create(eventDb)
      } yield ()

      recoverToSucceededIf[IllegalArgumentException] {
        for {
          _ <- setupF
          _ <- dlcOracle.signEvent(nonce, EnumAttestation(message))
        } yield ()
      }
  }

  it must "fail to sign an event with a nonce not in the event db" in {
    dlcOracle: DLCOracle =>
      val ecKey = ECPublicKey.freshPublicKey
      val nonce = ecKey.schnorrNonce

      val eventName = "dummy"
      val message = "dummy message"

      val rValDb =
        RValueDb(nonce, eventName, HDPurpose(0), HDCoinType.Bitcoin, 0, 0, 0)

      recoverToSucceededIf[RuntimeException] {
        for {
          _ <- dlcOracle.rValueDAO.create(rValDb)
          _ <- dlcOracle.signEvent(nonce, EnumAttestation(message))
        } yield ()
      }
  }

  it must "fail to create an enum event with no outcomes" in {
    dlcOracle: DLCOracle =>
      assertThrows[IllegalArgumentException] {
        dlcOracle.createNewEnumEvent("test", futureTime, Vector.empty)
      }
  }

  it must "fail to create an event with duplicate outcomes" in {
    dlcOracle: DLCOracle =>
      val outcomes = enumOutcomes :+ enumOutcomes.head
      assertThrows[IllegalArgumentException] {
        dlcOracle.createNewEnumEvent("test", futureTime, outcomes)
      }
  }

  it must "fail to create an event in the past" in { dlcOracle: DLCOracle =>
    assertThrows[IllegalArgumentException] {
      dlcOracle.createNewEvent("test", Instant.EPOCH, testDescriptor)
    }
  }

  it must "create and sign a signed digit decomposition event" in {
    dlcOracle: DLCOracle =>
      val eventName = "signed"
      val maturationTime = futureTime
      val descriptor =
        SignedDigitDecompositionEventDescriptor(UInt16(2),
                                                UInt16(3),
                                                "unit",
                                                Int32(0))

      for {
        announcement: OracleAnnouncementTLV <-
          dlcOracle.createNewEvent(eventName, maturationTime, descriptor)
        event <-
          dlcOracle
            .signDigits(announcement.eventTLV, -2)
      } yield {
        assert(event.isInstanceOf[CompletedDigitDecompositionV0OracleEvent])
        val attestations = event
          .asInstanceOf[CompletedDigitDecompositionV0OracleEvent]
          .oracleAttestmentV0TLV
        assert(
          OracleEvent.verifyAttestations(announcement,
                                         attestations,
                                         signingVersion =
                                           SigningVersion.latest))
      }
  }

  it must "create and sign a unsigned digit decomposition event" in {
    dlcOracle: DLCOracle =>
      val eventName = "unsigned"
      val maturationTime = futureTime
      val descriptor =
        UnsignedDigitDecompositionEventDescriptor(UInt16(2),
                                                  UInt16(3),
                                                  "unit",
                                                  Int32(0))

      for {
        announcement: OracleAnnouncementTLV <-
          dlcOracle.createNewEvent(eventName, maturationTime, descriptor)
        event <-
          dlcOracle
            .signDigits(announcement.eventTLV, 2)
      } yield {
        assert(event.isInstanceOf[CompletedDigitDecompositionV0OracleEvent])
        val attestations = event
          .asInstanceOf[CompletedDigitDecompositionV0OracleEvent]
          .oracleAttestmentV0TLV
        assert(
          OracleEvent.verifyAttestations(announcement,
                                         attestations,
                                         signingVersion =
                                           SigningVersion.latest))
      }
  }

  it must "fail to verify a unsigned digit decomposition event " in {
    dlcOracle: DLCOracle =>
      val eventName1 = "unsigned1"
      val eventName2 = "unsigned2"
      val maturationTime = futureTime
      val descriptor =
        UnsignedDigitDecompositionEventDescriptor(UInt16(2),
                                                  UInt16(3),
                                                  "unit",
                                                  Int32(0))

      for {
        announcement1: OracleAnnouncementTLV <-
          dlcOracle.createNewEvent(eventName1, maturationTime, descriptor)
        announcement2: OracleAnnouncementTLV <-
          dlcOracle.createNewEvent(eventName2, maturationTime, descriptor)
        event1 <-
          dlcOracle
            .signDigits(announcement1.eventTLV, 2)
        event2 <-
          dlcOracle
            .signDigits(announcement2.eventTLV, 1)
      } yield {
        assert(event1.isInstanceOf[CompletedDigitDecompositionV0OracleEvent])
        val attestations1 = event1
          .asInstanceOf[CompletedDigitDecompositionV0OracleEvent]
          .oracleAttestmentV0TLV
        assert(event2.isInstanceOf[CompletedDigitDecompositionV0OracleEvent])
        val attestations2 = event2
          .asInstanceOf[CompletedDigitDecompositionV0OracleEvent]
          .oracleAttestmentV0TLV

        assert(
          !OracleEvent.verifyAttestations(announcement1,
                                          attestations2,
                                          signingVersion =
                                            SigningVersion.latest))
        assert(
          !OracleEvent.verifyAttestations(announcement2,
                                          attestations1,
                                          signingVersion =
                                            SigningVersion.latest))
      }
  }
}
