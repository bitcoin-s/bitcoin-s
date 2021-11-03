package org.bitcoins.dlc.oracle

import com.typesafe.config.ConfigFactory
import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.api.dlcoracle.db._
import org.bitcoins.core.crypto.ECPrivateKeyUtil
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
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

  val testDescriptor: EnumEventDescriptorDLCSubType =
    EnumEventDescriptorDLCSubType(enumOutcomes)

  behavior of "DLCOracle"

  it must "correctly initialize" in { dlcOracle: DLCOracle =>
    dlcOracle.conf.exists().map(assert(_))
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
    val dummyEvent = TLVGen.oracleAnnouncementV1TLV(None).sampleSome
    dlcOracle.findEvent(dummyEvent).map { eventOpt =>
      assert(eventOpt.isEmpty)
    }
  }

  it must "calculate the correct staking address" in { dlcOracle: DLCOracle =>
    forAllAsync(ChainParamsGenerator.bitcoinNetworkParams) { network =>
      val expected =
        Bech32Address(
          P2WPKHWitnessSPKV0(dlcOracle.announcementPublicKey.publicKey),
          network)
      assert(dlcOracle.stakingAddress(network) == expected)
    }
  }

  it must "correctly sign a message" in { dlcOracle: DLCOracle =>
    val message = "hello world"
    val signature = dlcOracle.signMessage(message)
    assert(
      dlcOracle.announcementPublicKey.verify(CryptoUtil.sha256(message).bytes,
                                             signature))
  }

  it must "get the correctly sorted nonces in an announcement" in {
    dlcOracle: DLCOracle =>
      val eventName = "test"
      val descriptorTLV =
        DigitDecompositionEventDescriptorDLCType(base = UInt8.two,
                                                 isSigned = false,
                                                 numDigits = 3,
                                                 unit = "units",
                                                 precision = Int32.zero)

      for {
        announcement <- dlcOracle.createNewAnnouncement(eventName = eventName,
                                                        maturationTime =
                                                          futureTime,
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

      newConf.start().flatMap { _ =>
        val oracleB = new DLCOracle()(newConf)
        assert(oracleA.announcementPublicKey == oracleB.announcementPublicKey)

        val eventName = "test"
        val descriptorTLV =
          DigitDecompositionEventDescriptorDLCType(base = UInt8.two,
                                                   isSigned = false,
                                                   numDigits = 3,
                                                   unit = "units",
                                                   precision = Int32.zero)

        for {
          announcementA <- oracleA.createNewAnnouncement(eventName = eventName,
                                                         maturationTime =
                                                           futureTime,
                                                         descriptorTLV)
          announcementB <- oracleB.createNewAnnouncement(eventName = eventName,
                                                         maturationTime =
                                                           futureTime,
                                                         descriptorTLV)

          // Can't compare announcementTLV because different nonces might be used for signature
          _ = assert(
            announcementA.announcementPublicKey == announcementB.announcementPublicKey)
          _ = assert(announcementA.eventTLV == announcementB.eventTLV)

          eventA <- oracleA.signDigits(eventName, 1)
          eventB <- oracleB.signDigits(eventName, 1)
        } yield {
          assert(eventA.oracleAttestmentV0TLV == eventB.oracleAttestmentV0TLV)
        }
      }
  }

  it must "create a new event and list it with pending" in {
    dlcOracle: DLCOracle =>
      val time = futureTime

      for {
        _ <- dlcOracle.createNewAnnouncement("test", time, testDescriptor)
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
        _ <- dlcOracle.createNewAnnouncement("test", time, testDescriptor)
        _ <- dlcOracle.createNewAnnouncement("test2", time, testDescriptor)
        events <- dlcOracle.listEvents()
      } yield {
        assert(events.size == 2)
        assert(events.forall(_.eventDescriptorTLV == testDescriptor))
      }
  }

  it must "create two events and use incrementing key indexes" in {
    dlcOracle: DLCOracle =>
      val create1F =
        dlcOracle.createNewDigitDecompAnnouncement(eventName = "test1",
                                                   maturationTime = futureTime,
                                                   base = UInt8(10),
                                                   isSigned = false,
                                                   numDigits = 3,
                                                   unit = "units",
                                                   precision = Int32.zero)

      val create2F =
        dlcOracle.createNewDigitDecompAnnouncement(eventName = "test2",
                                                   maturationTime = futureTime,
                                                   base = UInt8(10),
                                                   isSigned = false,
                                                   numDigits = 3,
                                                   unit = "units",
                                                   precision = Int32.zero)

      for {
        _ <- create1F
        _ <- create2F

        rValDbs <- dlcOracle.rValueDAO.findAll()
      } yield {
        val indexes = rValDbs.map(_.keyIndex).sorted
        assert(indexes == Vector(0, 1, 2, 3, 4, 5))
      }
  }

  it must "fail to create an event with the same name" in {
    dlcOracle: DLCOracle =>
      for {
        _ <- dlcOracle.createNewAnnouncement("test", futureTime, testDescriptor)
        res <- recoverToSucceededIf[IllegalArgumentException](
          dlcOracle.createNewAnnouncement("test", futureTime, testDescriptor))
      } yield res
  }

  it must "create an enum new event and get its details" in {
    dlcOracle: DLCOracle =>
      val time = futureTime
      val eventName = "test"
      val oracleDataManagement = dlcOracle.oracleDataManagement
      for {
        announcement <-
          dlcOracle.createNewEnumAnnouncement(eventName, time, enumOutcomes)

        eventOpt <- dlcOracle.findEvent(announcement)
        metadata <- oracleDataManagement
          .findMetadataByAttestationPubKey(
            eventOpt.get.metadataOpt.get.attestationPublicKey)
          .map(_.get)
      } yield {
        assert(announcement.validateSignature)
        assert(eventOpt.isDefined)
        val event = eventOpt.get

        val announcementV1 = announcement.asInstanceOf[OracleAnnouncementV1TLV]

        assert(event.isInstanceOf[PendingEnumV0OracleEvent])
        assert(event.eventName == eventName)
        assert(event.eventDescriptorTLV == testDescriptor)
        assert(event.signingVersion == SigningVersion.latest)
        assert(event.pubkey == dlcOracle.attestationPublicKey)
        assert(event.maturationTime.getEpochSecond == time.getEpochSecond)

        val timestamp =
          FixedOracleEventTimestamp(UInt32(event.maturationTime.getEpochSecond))
        val expectedEventTLV =
          OracleEventV1TLV(testDescriptor, eventName, timestamp)

        assert(event.eventTLV == expectedEventTLV)

        val expectedAnnouncementTLV =
          OracleAnnouncementV1TLV(announcementSignature =
                                    event.announcementSignature,
                                  eventTLV = expectedEventTLV,
                                  metadata = metadata)

        assert(event.eventTLV == expectedEventTLV)
        assert(event.metadataOpt.get == metadata)
        assert(event.announcementTLV == expectedAnnouncementTLV)

        val announceBytes =
          SigningVersion.latest.calcAnnouncementHash(announcement =
                                                       event.eventTLV,
                                                     metadata =
                                                       announcementV1.metadata)

        assert(
          dlcOracle.announcementPublicKey.verify(announceBytes,
                                                 event.announcementSignature))
      }
  }

  it must "create and sign an enum event" in { dlcOracle: DLCOracle =>
    val descriptor = TLVGen.enumEventDescriptorV0TLV.sampleSome
    val outcome = descriptor.outcomes.head

    val descriptorV0TLV =
      EnumEventDescriptorDLCSubType(descriptor.outcomes)

    for {
      announcement <-
        dlcOracle.createNewAnnouncement("test", futureTime, descriptorV0TLV)

      completedEvent <-
        dlcOracle.signEnum(announcement.eventTLV, EnumAttestation(outcome))
    } yield {
      val sig = completedEvent.signature
      assert(completedEvent.attestation == sig.sig)
      assert(completedEvent.outcomes == Vector(EnumAttestation(outcome)))

      val descriptor = completedEvent.eventDescriptorTLV
      val hash = SigningVersion.latest.calcOutcomeHash(outcome)

      assert(dlcOracle.attestationPublicKey.verify(hash, sig))
      assert(
        SchnorrDigitalSignature(completedEvent.nonces.head,
                                completedEvent.attestation) == sig)
      assert(
        OracleEvent.verifyAttestations(announcement,
                                       completedEvent.oracleAttestmentV0TLV,
                                       signingVersion = SigningVersion.latest))
    }
  }

  it must "create and sign a large range event" in { dlcOracle: DLCOracle =>
    val outcome = -321L

    for {
      announcement <-
        dlcOracle.createNewDigitDecompAnnouncement(eventName = "test",
                                                   maturationTime = futureTime,
                                                   base = UInt8(10),
                                                   isSigned = true,
                                                   numDigits = 3,
                                                   unit = "units",
                                                   precision = Int32.zero)

      _ = assert(announcement.validateSignature)

      eventTLV = announcement.eventTLV

      completedEvent <- dlcOracle.signDigits(announcement, outcome)
    } yield {
      val signOutcome = DigitDecompositionSignAttestation(outcome >= 0)
      val digitOutcomes = Vector(DigitDecompositionAttestation(3),
                                 DigitDecompositionAttestation(2),
                                 DigitDecompositionAttestation(1))
      assert(completedEvent.outcomes == signOutcome +: digitOutcomes)

      val descriptor = completedEvent.eventDescriptorTLV

      // Sign Signature Check
      val signHash = SigningVersion.latest.calcOutcomeHash("-")
      val signSig = completedEvent.signatures.head
      assert(dlcOracle.attestationPublicKey.verify(signHash, signSig))
      assert(
        SchnorrDigitalSignature(completedEvent.nonces.head,
                                completedEvent.attestations.head) == signSig)

      // 100s Place signature Check
      val hash100 =
        SigningVersion.latest.calcOutcomeHash(
          DigitDecompositionAttestation(3).bytes)
      val sig100 = completedEvent.signatures(1)
      assert(dlcOracle.attestationPublicKey.verify(hash100, sig100))
      assert(
        SchnorrDigitalSignature(completedEvent.nonces(1),
                                completedEvent.attestations(1)) == sig100)

      // 10s Place signature Check
      val hash10 =
        SigningVersion.latest.calcOutcomeHash(
          DigitDecompositionAttestation(2).bytes)
      val sig10 = completedEvent.signatures(2)
      assert(dlcOracle.attestationPublicKey.verify(hash10, sig10))
      assert(
        SchnorrDigitalSignature(completedEvent.nonces(2),
                                completedEvent.attestations(2)) == sig10)

      // 1s Place signature Check
      val hash1 =
        SigningVersion.latest.calcOutcomeHash(
          DigitDecompositionAttestation(1).bytes)
      val sig1 = completedEvent.signatures(3)
      assert(dlcOracle.attestationPublicKey.verify(hash1, sig1))
      assert(
        SchnorrDigitalSignature(completedEvent.nonces(3),
                                completedEvent.attestations(3)) == sig1)
    }
  }

  it must "create and sign a non-base 10 large range event" in {
    dlcOracle: DLCOracle =>
      val outcome = -1931L

      for {
        announcement <-
          dlcOracle.createNewDigitDecompAnnouncement(eventName = "test",
                                                     maturationTime =
                                                       futureTime,
                                                     base = UInt8(16),
                                                     isSigned = true,
                                                     numDigits = 3,
                                                     unit = "units",
                                                     precision = Int32.zero)

        _ = assert(announcement.validateSignature)

        eventTLV = announcement.eventTLV

        completedEvent <- dlcOracle.signDigits(announcement, outcome)
      } yield {
        val signOutcome = DigitDecompositionSignAttestation(outcome >= 0)
        val digitOutcomes = Vector(DigitDecompositionAttestation(7),
                                   DigitDecompositionAttestation(8),
                                   DigitDecompositionAttestation(11))
        assert(completedEvent.outcomes == signOutcome +: digitOutcomes)

        val descriptor = completedEvent.eventDescriptorTLV

        // Sign Signature Check
        val signHash =
          SigningVersion.latest.calcOutcomeHash("-")
        val signSig = completedEvent.signatures.head
        assert(dlcOracle.attestationPublicKey.verify(signHash, signSig))
        assert(
          SchnorrDigitalSignature(completedEvent.nonces.head,
                                  completedEvent.attestations.head) == signSig)

        // 100s Place signature Check
        val hash100 =
          SigningVersion.latest.calcOutcomeHash(
            DigitDecompositionAttestation(7).bytes)
        val sig100 = completedEvent.signatures(1)
        assert(dlcOracle.attestationPublicKey.verify(hash100, sig100))
        assert(
          SchnorrDigitalSignature(completedEvent.nonces(1),
                                  completedEvent.attestations(1)) == sig100)

        // 10s Place signature Check
        val hash10 =
          SigningVersion.latest.calcOutcomeHash(
            DigitDecompositionAttestation(8).bytes)
        val sig10 = completedEvent.signatures(2)
        assert(dlcOracle.attestationPublicKey.verify(hash10, sig10))
        assert(
          SchnorrDigitalSignature(completedEvent.nonces(2),
                                  completedEvent.attestations(2)) == sig10)

        // 1s Place signature Check
        val hash1 =
          SigningVersion.latest.calcOutcomeHash(
            DigitDecompositionAttestation(11).bytes)
        val sig1 = completedEvent.signatures(3)
        assert(dlcOracle.attestationPublicKey.verify(hash1, sig1))
        assert(
          SchnorrDigitalSignature(completedEvent.nonces(3),
                                  completedEvent.attestations(3)) == sig1)
      }
  }

  it must "create and sign a large range event with digits of 0" in {
    dlcOracle: DLCOracle =>
      val outcome = 2

      for {
        announcement <-
          dlcOracle.createNewDigitDecompAnnouncement(eventName = "test",
                                                     maturationTime =
                                                       futureTime,
                                                     base = UInt8.two,
                                                     isSigned = false,
                                                     numDigits = 3,
                                                     unit = "units",
                                                     precision = Int32.zero)

        _ = assert(announcement.validateSignature)

        eventTLV = announcement.eventTLV

        completedEvent <- dlcOracle.signDigits(announcement, outcome)
      } yield {
        val digitOutcomes = Vector(DigitDecompositionAttestation(0),
                                   DigitDecompositionAttestation(1),
                                   DigitDecompositionAttestation(0))
        assert(completedEvent.outcomes == digitOutcomes)

        val descriptor = completedEvent.eventDescriptorTLV

        // 100s Place signature Check
        val hash100 =
          SigningVersion.latest.calcOutcomeHash(
            DigitDecompositionAttestation(0).bytes)
        val sig100 = completedEvent.signatures.head
        assert(dlcOracle.attestationPublicKey.verify(hash100, sig100))
        assert(
          SchnorrDigitalSignature(completedEvent.nonces.head,
                                  completedEvent.attestations.head) == sig100)

        // 10s Place signature Check
        val hash10 =
          SigningVersion.latest.calcOutcomeHash(
            DigitDecompositionAttestation(1).bytes)
        val sig10 = completedEvent.signatures(1)
        assert(dlcOracle.attestationPublicKey.verify(hash10, sig10))
        assert(
          SchnorrDigitalSignature(completedEvent.nonces(1),
                                  completedEvent.attestations(1)) == sig10)

        // 1s Place signature Check
        val hash1 =
          SigningVersion.latest.calcOutcomeHash(
            DigitDecompositionAttestation(0).bytes)
        val sig1 = completedEvent.signatures(2)
        assert(dlcOracle.attestationPublicKey.verify(hash1, sig1))
        assert(
          SchnorrDigitalSignature(completedEvent.nonces(2),
                                  completedEvent.attestations(2)) == sig1)
      }
  }

  it must "create and sign a decomp event with a large num digits" in {
    dlcOracle: DLCOracle =>
      //trying make sure we don't regress on
      //https://github.com/bitcoin-s/bitcoin-s/issues/3431

      val outcome = 30816
      val numDigits = 18
      val eventName = "test"
      for {
        announcement <-
          dlcOracle.createNewDigitDecompAnnouncement(eventName = eventName,
                                                     maturationTime =
                                                       futureTime,
                                                     base = UInt8.two,
                                                     isSigned = false,
                                                     numDigits = numDigits,
                                                     unit = "units",
                                                     precision = Int32.zero)

        _ = assert(announcement.validateSignature)

        event <- dlcOracle.signDigits(eventName, outcome)
      } yield {
        assert(event.outcomeBase10 == outcome)
      }
  }

  it must "correctly track pending events" in { dlcOracle: DLCOracle =>
    val outcome = enumOutcomes.head
    for {
      announcement <-
        dlcOracle.createNewEnumAnnouncement("test", futureTime, enumOutcomes)
      beforePending <- dlcOracle.listPendingEventDbs()
      beforeEvents <- dlcOracle.listEvents()
      _ = assert(beforePending.size == 1)
      _ = assert(beforeEvents.size == 1)
      _ = assert(beforeEvents.head.isInstanceOf[PendingOracleEvent])

      nonce = announcement.nonces.head.head

      _ <- dlcOracle.createAttestation(nonce, EnumAttestation(outcome))
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
        dlcOracle.createAttestation(dummyNonce,
                                    EnumAttestation("testOutcomes")))
  }

  it must "fail to sign an enum outcome that doesn't exist" in {
    dlcOracle: DLCOracle =>
      recoverToSucceededIf[RuntimeException] {
        for {
          announcement <-
            dlcOracle.createNewEnumAnnouncement("test",
                                                futureTime,
                                                enumOutcomes)

          nonce = announcement.nonces.head.head

          _ <- dlcOracle.createAttestation(
            nonce,
            EnumAttestation("not a real outcome"))
        } yield ()
      }
  }

  it must "sign a negative number for a unsigned digit decomp event that results in 0" in {
    dlcOracle: DLCOracle =>
      for {
        announcement <-
          dlcOracle.createNewDigitDecompAnnouncement(eventName = "test",
                                                     maturationTime =
                                                       futureTime,
                                                     base = UInt8.two,
                                                     isSigned = false,
                                                     numDigits = 3,
                                                     unit = "units",
                                                     precision = Int32.zero)

        res <- dlcOracle.signDigits(announcement, -2)
      } yield {
        assert(res.outcomeBase10 == 0)
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
          _ <- dlcOracle.createAttestation(nonce, EnumAttestation(message))
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
          _ <- dlcOracle.createAttestation(nonce, EnumAttestation(message))
        } yield ()
      }
  }

  it must "fail to create an enum event with no outcomes" in {
    dlcOracle: DLCOracle =>
      assertThrows[IllegalArgumentException] {
        dlcOracle.createNewEnumAnnouncement("test", futureTime, Vector.empty)
      }
  }

  it must "fail to create an event with duplicate outcomes" in {
    dlcOracle: DLCOracle =>
      val outcomes = enumOutcomes :+ enumOutcomes.head
      assertThrows[IllegalArgumentException] {
        dlcOracle.createNewEnumAnnouncement("test", futureTime, outcomes)
      }
  }

  it must "fail to create an event in the past" in { dlcOracle: DLCOracle =>
    assertThrows[IllegalArgumentException] {
      dlcOracle.createNewAnnouncement("test", Instant.EPOCH, testDescriptor)
    }
  }

  it must "create and sign a signed digit decomposition event" in {
    dlcOracle: DLCOracle =>
      val eventName = "signed"
      val maturationTime = futureTime
      val descriptor =
        SignedDigitDecompositionEventDescriptorDLCType(UInt8.two,
                                                       UInt16(3),
                                                       "unit",
                                                       Int32(0))

      for {
        announcement <-
          dlcOracle.createNewAnnouncement(eventName, maturationTime, descriptor)
        event <-
          dlcOracle
            .signDigits(announcement, -2)
      } yield {
        val attestations = event.oracleAttestmentV0TLV
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
        UnsignedDigitDecompositionEventDescriptorDLCType(UInt8.two,
                                                         UInt16(3),
                                                         "unit",
                                                         Int32(0))

      for {
        announcement <-
          dlcOracle.createNewAnnouncement(eventName, maturationTime, descriptor)
        event <-
          dlcOracle
            .signDigits(announcement, 2)
      } yield {
        val attestations = event.oracleAttestmentV0TLV
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
        UnsignedDigitDecompositionEventDescriptorDLCType(UInt8.two,
                                                         UInt16(3),
                                                         "unit",
                                                         Int32(0))

      for {
        announcement1 <-
          dlcOracle.createNewAnnouncement(eventName1,
                                          maturationTime,
                                          descriptor)
        announcement2 <-
          dlcOracle.createNewAnnouncement(eventName2,
                                          maturationTime,
                                          descriptor)
        event1 <-
          dlcOracle
            .signDigits(announcement1, 2)
        event2 <-
          dlcOracle
            .signDigits(announcement2, 1)
      } yield {
        val attestations1 = event1.oracleAttestmentV0TLV
        val attestations2 = event2.oracleAttestmentV0TLV

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

  it must "create and delete signatures for an enum event" in {
    dlcOracle: DLCOracle =>
      val descriptor = TLVGen.enumEventDescriptorV0TLV.sampleSome
      val outcome = descriptor.outcomes.head

      val descriptorV0TLV =
        EnumEventDescriptorDLCSubType(descriptor.outcomes)

      for {
        announcement <-
          dlcOracle.createNewAnnouncement("test", futureTime, descriptorV0TLV)

        _ <-
          dlcOracle.signEnum(announcement.eventTLV, EnumAttestation(outcome))

        signedEvent <- dlcOracle.findEvent("test").map(_.get)
        _ = {
          signedEvent match {
            case c: CompletedEnumV0OracleEvent =>
              assert(c.attestations.nonEmpty)
              assert(c.outcomes.nonEmpty)
            case _: PendingOracleEvent | _: CompletedOracleEvent =>
              fail()
          }
        }

        _ <- dlcOracle.deleteAttestation("test")
        event <- dlcOracle.findEvent("test").map(_.get)
      } yield {
        event match {
          case _: PendingEnumV0OracleEvent => succeed
          case _: PendingOracleEvent | _: CompletedOracleEvent =>
            fail()
        }
      }
  }

  it must "create and delete signatures for a decomp event" in {
    dlcOracle: DLCOracle =>
      val descriptor =
        UnsignedDigitDecompositionEventDescriptorDLCType(UInt8.two,
                                                         UInt16(3),
                                                         "unit",
                                                         Int32(0))

      for {
        _ <- dlcOracle.createNewAnnouncement("test", futureTime, descriptor)

        _ <- dlcOracle.signDigits("test", 0)

        signedEvent <- dlcOracle.findEvent("test").map(_.get)
        _ = {
          signedEvent match {
            case c: CompletedDigitDecompositionV0OracleEvent =>
              assert(c.attestations.nonEmpty)
              assert(c.outcomes.nonEmpty)
            case _: PendingOracleEvent | _: CompletedOracleEvent =>
              fail()
          }
        }

        _ <- dlcOracle.deleteAttestation("test")
        event <- dlcOracle.findEvent("test").map(_.get)
      } yield {
        event match {
          case _: PendingDigitDecompositionV0OracleEvent => succeed
          case _: PendingOracleEvent | _: CompletedOracleEvent =>
            fail()
        }
      }
  }

  it must "fail to delete signatures for an unsigned enum event" in {
    dlcOracle: DLCOracle =>
      val descriptor =
        UnsignedDigitDecompositionEventDescriptorDLCType(UInt8.two,
                                                         UInt16(3),
                                                         "unit",
                                                         Int32(0))

      for {
        _ <- dlcOracle.createNewAnnouncement("test", futureTime, descriptor)

        signedEvent <- dlcOracle.findEvent("test").map(_.get)
        _ = assert(
          signedEvent.isInstanceOf[PendingDigitDecompositionV0OracleEvent])

        res <- recoverToSucceededIf[IllegalArgumentException](
          dlcOracle.deleteAttestation("test"))
      } yield res
  }

  it must "fail to delete signatures for an unsigned decomp event" in {
    dlcOracle: DLCOracle =>
      val descriptor = TLVGen.enumEventDescriptorV0TLV.sampleSome

      for {
        _ <- dlcOracle.createNewAnnouncement("test", futureTime, descriptor)

        signedEvent <- dlcOracle.findEvent("test").map(_.get)
        _ = assert(signedEvent.isInstanceOf[PendingEnumV0OracleEvent])

        res <- recoverToSucceededIf[IllegalArgumentException](
          dlcOracle.deleteAttestation("test"))
      } yield res
  }

  it must "delete enum announcements" in { dlcOracle =>
    val eventName = "test"
    val createdF =
      dlcOracle.createNewEnumAnnouncement(eventName,
                                          futureTime,
                                          Vector("0", "1", "2"))
    for {
      c <- createdF
      _ <- dlcOracle.deleteAnnouncement(c)
      //make sure we can't find it
      annOpt <- dlcOracle.findEvent(eventName)
    } yield {
      assert(annOpt.isEmpty)
    }
  }

  it must "delete numeric announcements" in { dlcOracle =>
    val eventName = "test"
    val createdF =
      dlcOracle.createNewDigitDecompAnnouncement(eventName = eventName,
                                                 maturationTime = futureTime,
                                                 base = UInt8.two,
                                                 isSigned = false,
                                                 numDigits = 2,
                                                 unit = "UNIT",
                                                 precision = Int32.zero)
    for {
      c <- createdF
      _ <- dlcOracle.deleteAnnouncement(c)
      //make sure we can't find it
      annOpt <- dlcOracle.findEvent(eventName)
    } yield {
      assert(annOpt.isEmpty)
    }
  }

  it must "fail to delete an announcement if there are attesations associated with it" in {
    dlcOracle =>
      val eventName = "test"
      val createdF =
        dlcOracle.createNewDigitDecompAnnouncement(eventName = eventName,
                                                   maturationTime = futureTime,
                                                   base = UInt8.two,
                                                   isSigned = false,
                                                   numDigits = 2,
                                                   unit = "UNIT",
                                                   precision = Int32.zero)

      val resultF = for {
        _ <- createdF
        _ <- dlcOracle.signDigits(eventName, 1)
        _ <- dlcOracle.deleteAnnouncement(eventName)
      } yield ()

      recoverToSucceededIf[RuntimeException] {
        resultF
      }

  }

  it must "delete enum attestation" in { dlcOracle: DLCOracle =>
    val eventName = "test"
    val createdF =
      dlcOracle.createNewAnnouncement(eventName, futureTime, testDescriptor)
    for {
      _ <- createdF
      _ <- dlcOracle.signEnum(eventName, EnumAttestation("cloudy"))
      _ <- dlcOracle.deleteAttestation(eventName)
      eventOpt <- dlcOracle.findEvent(eventName)
    } yield {
      assert(eventOpt.isDefined)
      assert(eventOpt.get.isInstanceOf[PendingEnumV0OracleEvent])
    }
  }

  it must "delete numeric attestations" in { dlcOracle: DLCOracle =>
    val eventName = "test"
    val createdF =
      dlcOracle.createNewDigitDecompAnnouncement(eventName = eventName,
                                                 maturationTime = futureTime,
                                                 base = UInt8.two,
                                                 isSigned = false,
                                                 numDigits = 2,
                                                 unit = "UNIT",
                                                 precision = Int32.zero)

    for {
      _ <- createdF
      _ <- dlcOracle.signDigits(eventName, 1)
      _ <- dlcOracle.deleteAttestation(eventName)
      eventOpt <- dlcOracle.findEvent(eventName)
    } yield {
      assert(eventOpt.isDefined)
      assert(eventOpt.get.isInstanceOf[PendingDigitDecompositionV0OracleEvent])
    }
  }

  it must "delete attestations, and then delete the announcement" in {
    dlcOracle: DLCOracle =>
      val eventName = "test"
      val createdF =
        dlcOracle.createNewDigitDecompAnnouncement(eventName = eventName,
                                                   maturationTime = futureTime,
                                                   base = UInt8.two,
                                                   isSigned = false,
                                                   numDigits = 2,
                                                   unit = "UNIT",
                                                   precision = Int32.zero)

      for {
        _ <- createdF
        _ <- dlcOracle.signDigits(eventName, 1)
        _ <- dlcOracle.deleteAttestation(eventName)
        _ <- dlcOracle.deleteAnnouncement(eventName)
        eventOpt <- dlcOracle.findEvent(eventName)
      } yield {
        assert(eventOpt.isEmpty)
      }
  }

  it must "set and retrieve oracle name" in { dlcOracle: DLCOracle =>
    for {
      emptyNameOpt <- dlcOracle.oracleName()
      _ <- dlcOracle.setOracleName("test name")
      testNameOpt <- dlcOracle.oracleName()
    } yield {
      assert(emptyNameOpt.isEmpty)
      assert(testNameOpt.contains("test name"))
    }
  }

  it must "export staking address wif" in { dlcOracle: DLCOracle =>
    val pubKey = dlcOracle.announcementPublicKey
    val wif = dlcOracle.exportSigningKeyWIF
    val privKey = ECPrivateKeyUtil.fromWIFToPrivateKey(wif)
    assert(privKey.toPrivateKey.schnorrPublicKey == pubKey)
  }
}
