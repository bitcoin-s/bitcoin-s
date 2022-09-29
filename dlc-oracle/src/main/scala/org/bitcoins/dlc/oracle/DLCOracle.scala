package org.bitcoins.dlc.oracle

import com.typesafe.config.Config
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.api.dlcoracle.db._
import org.bitcoins.core.config.{BitcoinNetwork, MainNet}
import org.bitcoins.core.crypto.ExtKeyVersion.SegWitTestNet3Priv
import org.bitcoins.core.crypto.{
  ECPrivateKeyUtil,
  ExtPrivateKeyHardened,
  ExtPublicKey
}
import org.bitcoins.core.dlc.oracle.{
  NonceSignaturePairDb,
  OracleAnnouncementWithId
}
import org.bitcoins.core.hd._
import org.bitcoins.core.number.{UInt32, _}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.sorted.OrderedNonces
import org.bitcoins.core.util.{FutureUtil, NumberUtil, TimeUtil}
import org.bitcoins.crypto._
import org.bitcoins.db.SafeDatabase
import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.db.util.MasterXPubUtil
import org.bitcoins.dlc.commons.oracle.{
  EventOutcomeDAO,
  OracleAnnouncementDataDAO,
  OracleMetadataDAO,
  OracleSchnorrNonceDAO
}
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.keymanager.WalletStorage
import scodec.bits.ByteVector

import java.nio.file.Path
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{ExecutionContext, Future}

case class DLCOracle()(implicit val conf: DLCOracleAppConfig)
    extends DLCOracleApi
    with Logging {

  implicit private val ec: ExecutionContext = conf.ec

  // We have to use Testnet here because before this was dictated by
  // by the network config. The default network config was Regtest,
  // which uses a Testnet HDCoinType, so that was chosen as the hard coded value.
  // It was a mistake to originally have this dictated by the config
  // as the oracle should be network agnostic.
  // see https://github.com/bitcoin-s/bitcoin-s/issues/2748
  private val coinType = HDCoinType.Testnet

  private val rValAccount: HDAccount = {
    val purpose = conf.kmParams.purpose

    val coin = HDCoin(purpose, coinType)
    HDAccount(coin, 0)
  }

  /** The chain index used in the bip32 paths for generating R values */
  private val rValueChainIndex = 0

  /** The root private key for this oracle */
  private[this] val extPrivateKey: ExtPrivateKeyHardened = {
    WalletStorage.getPrivateKeyFromDisk(conf.kmConf.seedPath,
                                        SegWitTestNet3Priv,
                                        conf.aesPasswordOpt,
                                        conf.bip39PasswordOpt)
  }

  def getRootXpub: ExtPublicKey = extPrivateKey.extPublicKey

  private def announcementPrivKey: ECPrivateKey = {
    val coin = HDCoin(HDPurposes.SegWit, coinType)
    val account = HDAccount(coin, 0)
    val purpose = coin.purpose
    val chain = HDChainType.External
    val index = 0

    val path = BIP32Path.fromHardenedString(
      s"m/${purpose.constant}'/${coin.coinType.toInt}'/${account.index}'/${chain.index}'/$index'")
    extPrivateKey.deriveChildPrivKey(path).key
  }

  private def attestationPrivKey: ECPrivateKey = {
    val coin = HDCoin(HDPurposes.SegWit, coinType)
    val account = HDAccount(coin, 0)
    val purpose = coin.purpose
    val chain = HDChainType.External
    val index = 1 //increment the index by one

    val path = BIP32Path.fromHardenedString(
      s"m/${purpose.constant}'/${coin.coinType.toInt}'/${account.index}'/${chain.index}'/$index'")
    extPrivateKey.deriveChildPrivKey(path).key
  }

  override val announcementPublicKey: SchnorrPublicKey =
    announcementPrivKey.schnorrPublicKey

  override val attestationPublicKey: SchnorrPublicKey =
    attestationPrivKey.schnorrPublicKey

  override def stakingAddress(network: BitcoinNetwork): Bech32Address =
    Bech32Address(P2WPKHWitnessSPKV0(announcementPublicKey.publicKey), network)

  private val daos: DLCOracleDAOs = DLCOracleDAOs(
    RValueDAO(),
    EventDAO(),
    EventOutcomeDAO(),
    OracleAnnouncementDataDAO(),
    OracleMetadataDAO(),
    OracleSchnorrNonceDAO()
  )

  protected[bitcoins] val rValueDAO: RValueDAO = daos.rValueDAO
  protected[bitcoins] val eventDAO: EventDAO = daos.eventDAO
  protected[bitcoins] val eventOutcomeDAO: EventOutcomeDAO = daos.outcomeDAO

  protected[bitcoins] val masterXpubDAO: MasterXPubDAO =
    MasterXPubDAO()(ec, conf)

  private lazy val safeDatabase: SafeDatabase = rValueDAO.safeDatabase
  import rValueDAO.profile.api._

  private lazy val nextKeyIndexF: Future[AtomicInteger] =
    rValueDAO.maxKeyIndex.map {
      case Some(idx) => new AtomicInteger(idx + 1)
      case None      => new AtomicInteger(0)
    }

  private def getPath(keyIndex: Int): BIP32Path = {
    val accountIndex = rValAccount.index
    val coin = rValAccount.coin
    val purpose = coin.purpose

    BIP32Path.fromString(
      s"m/${purpose.constant}'/${coin.coinType.toInt}'/$accountIndex'/$rValueChainIndex'/$keyIndex'")
  }

  private def getKValue(
      rValDb: RValueDb,
      signingVersion: SigningVersion): ECPrivateKey =
    getKValue(rValDb.eventName, rValDb.path, signingVersion)

  private def getKValue(
      label: String,
      path: BIP32Path,
      signingVersion: SigningVersion): ECPrivateKey = {
    require(path.forall(_.hardened),
            s"Cannot use a BIP32Path with unhardened nodes, got $path")
    val priv = extPrivateKey.deriveChildPrivKey(path).key
    val tweakBytes = signingVersion.calcNonceTweak(priv.schnorrNonce, label)
    val tweak = ECPrivateKey(tweakBytes)

    priv.add(tweak)
  }

  override def listEvents(): Future[Vector[OracleEvent]] = {
    val pendingF = listPendingEvents()
    val completedF = listCompletedEvents()
    for {
      pending <- pendingF
      completed <- completedF
    } yield pending ++ completed
  }

  private def getFullOracleEvents(
      eventDbs: Vector[EventDb]): Future[Option[OracleEvent]] = {
    require(
      eventDbs.map(_.eventName).distinct.size == 1,
      s"Event names must all be the same, first eventName=${eventDbs.head.eventName}")
    require(
      eventDbs.map(_.pubkey).distinct.size == 1,
      s"Event pubkeys must all be the same, first event pubKey=${eventDbs.head.pubkey}")

    val eventDb = eventDbs.head
    eventDb.attestationOpt match {
      case Some(_) => getCompleteOracleEvent(eventDbs)
      case None    => getPendingOracleEvent(eventDbs)
    }
  }

  private def getPendingOracleEvent(
      eventDbs: Vector[EventDb]): Future[Option[PendingOracleEvent]] = {
    require(eventDbs.nonEmpty, s"Cannot have no eventDbs!")
    require(
      eventDbs.map(_.eventName).distinct.size == 1,
      s"Event names must all be the same, first eventName=${eventDbs.head.eventName}")
    require(
      eventDbs.map(_.pubkey).distinct.size == 1,
      s"Event pubkeys must all be the same, first event pubKey=${eventDbs.head.pubkey}")

    val events = eventDbs.groupBy(_.announcementSignature)
    val resultNested = events.values.map { dbs =>
      require(
        dbs.map(_.pubkey).distinct.length == 1,
        s"Attestation pubkey must be the same for all events with the same announcement signature")

      //bug here, we don't guarantee that attestation public keys are unique!
      val metadataOptF =
        oracleDataManagement.findMetadataByNonce(dbs.head.nonce)
      metadataOptF.map { metadataDbOpt =>
        val metadataOpt = metadataDbOpt.map(_.metadata)
        val e = OracleEvent.fromPendingEventDbs(dbs, metadataOpt)
        Some(e)
      }
    }

    Future
      .sequence(resultNested)
      .map(_.headOption.flatten)
  }

  private def getCompleteOracleEvent(
      eventDbs: Vector[EventDb]): Future[Option[CompletedOracleEvent]] = {
    require(eventDbs.nonEmpty, s"Cannot have no eventDbs!")
    require(
      eventDbs.forall(_.attestationOpt.isDefined),
      s"To make completed events all attestations need to be defined, got=${eventDbs
        .map(_.eventName)}")

    require(
      eventDbs.map(_.eventName).distinct.size == 1,
      s"Event names must all be the same, first eventName=${eventDbs.head.eventName}")
    require(
      eventDbs.map(_.pubkey).distinct.size == 1,
      s"Event pubkeys must all be the same, first event pubKey=${eventDbs.head.pubkey}")

    val events = eventDbs.groupBy(_.announcementSignature)
    val resultNested = events.values.map { dbs =>
      require(
        dbs.map(_.pubkey).distinct.length == 1,
        s"Attestation pubkey must be the same for all events with the same announcement signature")
      val metadataOptF =
        oracleDataManagement.findMetadataByNonce(eventDbs.head.nonce)
      metadataOptF.map { metadataDbOpt =>
        val metadataOpt = metadataDbOpt.map(_.metadata)
        val e = OracleEvent.fromCompletedEventDbs(dbs, metadataOpt)
        Some(e)
      }
    }

    Future
      .sequence(resultNested)
      .map(_.headOption.flatten)
  }

  override def listPendingEvents(): Future[Vector[PendingOracleEvent]] = {

    val oldPendingEventsF: Future[Vector[PendingOracleEvent]] =
      eventDAO.getPendingEvents.flatMap { eventDbs =>
        val events = eventDbs.groupBy(_.announcementSignature)
        val resultF = Future.traverse(events.values) { eventDbs =>
          getPendingOracleEvent(eventDbs)
        }

        resultF.map(_.flatten.toVector)
      }

    val newPendingAnnouncementsF = oracleDataManagement.getPendingAnnouncements

    val newPendingEventsF: Future[Vector[PendingOracleEvent]] = for {
      newPendingAnnouncements <- newPendingAnnouncementsF
      pendingEvent = newPendingAnnouncements.map { a =>
        a.announcement match {
          case v1: OracleAnnouncementV1TLV =>
            OracleEvent.fromAnnouncement(v1)
        }
      }
    } yield pendingEvent

    for {
      newPendingEvents <- newPendingEventsF
      oldPendingEvents <- oldPendingEventsF
    } yield newPendingEvents ++ oldPendingEvents
  }

  override def listCompletedEvents(): Future[Vector[CompletedOracleEvent]] = {
    val oldCompletedEventsF = eventDAO.getCompletedEvents.flatMap { eventDbs =>
      val events = eventDbs.groupBy(_.announcementSignature)
      val resultF = Future.traverse(events.values) { eventDbs =>
        getCompleteOracleEvent(eventDbs)
      }

      resultF.map(_.flatten.toVector)
    }

    val completedAnnouncementsF = oracleDataManagement.getCompletedAnnouncements
    val completedEventsF = for {
      completedAnnouncements <- completedAnnouncementsF
      completedEvents = completedAnnouncements.map {
        announcementAttestationPair =>
          OracleEvent.fromAnnouncementAttestationPair(
            announcementAttestationPair.announcement,
            announcementAttestationPair.attestation)
      }
    } yield completedEvents

    for {
      oldCompletedEvents <- oldCompletedEventsF
      completedEvents <- completedEventsF
    } yield completedEvents ++ oldCompletedEvents
  }

  override def findEvent(
      announcement: BaseOracleAnnouncement): Future[Option[OracleEvent]] = {
    findEvent(announcement.eventTLV.eventId)
  }

  override def findEvent(eventName: String): Future[Option[OracleEvent]] = {
    val newFormatEventsF: Future[Vector[OracleAnnouncementWithId]] =
      oracleDataManagement.getByEventName(eventName)

    newFormatEventsF.flatMap { newFormatEvents =>
      //if we don't have a new format check to see if we have old format
      if (newFormatEvents.isEmpty) {
        eventDAO.findByEventName(eventName).flatMap { dbs =>
          if (dbs.isEmpty) {
            Future.successful(None)
          } else {
            getFullOracleEvents(dbs)
          }
        }
      } else {

        //get attestments
        val oracleEventOptF: Future[Vector[OracleEvent]] = {
          Future.traverse(newFormatEvents) { n =>
            val announcement = n.announcement.asInstanceOf[
              OracleAnnouncementV1TLV
            ] //safe because only v1 works for getattestments
            oracleDataManagement.getAttestments(n.id).map { attestmentOpt =>
              OracleEvent.fromAnnouncementAndAttestations(announcement,
                                                          attestmentOpt)
            }
          }
        }

        //fine to only grab 1 as we enforce database
        //invariants that say announcements are unique by event name
        oracleEventOptF.map(_.headOption)
      }
    }
  }

  override def createNewDigitDecompAnnouncement(
      eventName: String,
      maturationTime: Instant,
      base: UInt8,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int32): Future[OracleAnnouncementWithId] = {
    require(base > UInt8.zero, s"base cannot be less than 1, got ${base.toInt}")
    require(numDigits > 0, s"numDigits cannot be less than 1, got $numDigits")

    logger.info(
      s"Create new digit decomp event with eventId=$eventName base=$base numDigits=$numDigits unit=$unit")

    val descriptorTLV = DigitDecompositionEventDescriptorDLCType(base,
                                                                 isSigned,
                                                                 numDigits,
                                                                 unit,
                                                                 precision)

    createNewAnnouncement(eventName, maturationTime, descriptorTLV)
  }

  override def createNewEnumAnnouncement(
      eventName: String,
      maturationTime: Instant,
      outcomes: Vector[String]): Future[OracleAnnouncementWithId] = {
    require(outcomes.nonEmpty, "Cannot make an event with no outcomes")
    require(outcomes.distinct.size == outcomes.size,
            s"Cannot have duplicate outcomes, got $outcomes")

    logger.info(
      s"Creating new enum announcement eventName=$eventName outcomes=$outcomes")
    val descriptorTLV = EnumEventDescriptorDLCSubType(outcomes)

    createNewAnnouncement(eventName, maturationTime, descriptorTLV)
  }

  val oracleDataManagement: OracleDataManagement =
    OracleDataManagement(daos = daos)

  override def createNewAnnouncement(
      eventName: String,
      maturationTime: Instant,
      descriptor: EventDescriptorDLCType,
      signingVersion: SigningVersion = SigningVersion.latest): Future[
    OracleAnnouncementWithId] = {
    require(maturationTime.isAfter(TimeUtil.now),
            s"Event cannot mature in the past, got $maturationTime")

    val rValueDbsF = for {
      index <- nextKeyIndexF
      firstIndex = index.getAndAdd(descriptor.noncesNeeded)
      rValueDbs =
        0.until(descriptor.noncesNeeded)
          .map { num =>
            val index = firstIndex + num
            val path = getPath(index)
            val nonce = getKValue(eventName, path, signingVersion).schnorrNonce

            RValueDbHelper(nonce = nonce,
                           eventName = eventName,
                           account = rValAccount,
                           chainType = rValueChainIndex,
                           keyIndex = index)
          }
          .toVector
    } yield rValueDbs

    val metadataF: Future[OracleMetadata] = for {
      rValues <- rValueDbsF
      nonces = OrderedNonces.fromUnsorted(rValues.map(_.nonce))
      attestations = SchnorrAttestation.build(
        announcementPrivKey,
        attestationPrivKey.schnorrPublicKey,
        nonces)
      oracleName = NormalizedString("Oracle_name") //how do i get this?
      oracleDescription = NormalizedString("oracle description")
      creationTime = UInt32(Instant.now().getEpochSecond)
      metadataSignature = OracleMetadataSignature.buildSignature(
        announcementPrivKey = announcementPrivKey,
        oracleName = oracleName,
        oracleDescription = oracleDescription,
        creationTime = creationTime,
        schnorrAttestation = attestations
      )
    } yield {
      OracleMetadata(
        announcementPublicKey = announcementPublicKey,
        oracleName = oracleName,
        oracleDescription = oracleDescription,
        creationTime = creationTime,
        attestations = attestations,
        metadataSignature = metadataSignature
      )
    }

    for {
      rValues <- rValueDbsF
      metadata <- metadataF
      created <- oracleDataManagement.createNewAnnouncement(
        eventName = eventName,
        maturationTime = maturationTime,
        descriptor = descriptor,
        announcementPrivKey = announcementPrivKey,
        metadata = metadata,
        rValueDbs = rValues
      )
      _ = require(created.announcement.validateSignature,
                  s"Valid signature must be generated for $eventName")
    } yield created

  }

  override def signEnum(
      eventName: String,
      outcome: EnumAttestation): Future[CompletedEnumV0OracleEvent] = {
    logger.info(s"Signing enum eventName=$eventName outcome=$outcome")
    val announcementV1F = oracleDataManagement.getByEventName(eventName)

    val nonceF = announcementV1F.flatMap { case announcements =>
      require(
        announcements.isEmpty || announcements.length == 1,
        s"Can only have 1 announcement for signing enum events, got=$announcements")
      val nonceF = if (announcements.nonEmpty) {
        //sign v1 announcement
        val nonces = announcements.head.announcement
          .asInstanceOf[OracleAnnouncementV1TLV]
          .metadata
          .attestations
          .nonces
        require(nonces.size == 1,
                s"Can only have 1 nonce for an enum event, got=$nonces")
        Future.successful(nonces.head)
      } else {
        eventDAO
          .findByEventName(eventName)
          .map { eventDbs =>
            require(
              eventDbs.size == 1,
              s"Enum events can only have 1 nonce, got eventDbs.size=${eventDbs.size}")
            eventDbs.head.nonce
          }
      }
      nonceF
    }
    for {
      nonce <- nonceF
      _ <- createAttestation(nonce, outcome)
      oracleEventOpt <- findEvent(eventName)
    } yield {
      oracleEventOpt match {
        case Some(oracleEvent) =>
          oracleEvent match {
            case enum: CompletedEnumV0OracleEvent => enum
            case enumPending: PendingEnumV0OracleEvent =>
              sys.error(
                s"Cannot have a pending enum oracle event after creating attestations, got=$enumPending")
            case numeric @ (_: CompletedDigitDecompositionV0OracleEvent |
                _: PendingDigitDecompositionV0OracleEvent) =>
              sys.error(
                s"Cannot have a numeric event when signing enum event, got=$numeric")
          }
        case None =>
          sys.error(s"Could not find oracleEvent with eventName=$eventName")
      }
    }
  }

  override def signEnum(
      oracleEventTLV: BaseOracleEvent,
      outcome: EnumAttestation): Future[CompletedEnumV0OracleEvent] = {
    signEnum(oracleEventTLV.eventId, outcome)
  }

  private def createAttestationV1NonceAction(
      nonceSignatureDb: NonceSignaturePairDb,
      rValDb: RValueDb,
      outcome: DLCAttestationType,
      signingVersion: SigningVersion): DBIOAction[
    NonceSignaturePairDb,
    NoStream,
    Effect.Write with Effect.Read] = {
    require(nonceSignatureDb.attestationOpt.isEmpty,
            s"Cannot sign nonce twice, got=$nonceSignatureDb")
    val hash = signingVersion.calcOutcomeHash(outcome.bytes)

    val kVal = getKValue(rValDb, signingVersion)
    require(
      kVal.schnorrNonce == rValDb.nonce,
      s"The nonce from derived seed did not match database, db=${rValDb.nonce.hex} derived=${kVal.schnorrNonce.hex}, rValDb=$rValDb"
    )

    val outcomeExistsA =
      oracleDataManagement.getOutcomesForNonceAction(nonceSignatureDb.nonce)
    outcomeExistsA.flatMap { outcomeExists =>
      //verify we have a hash in the database for the thing that was signed
      val hashExists = outcomeExists.exists(_.hashedMessage == hash)
      if (hashExists) {
        val sig = attestationPrivKey
          .schnorrSignWithNonce(dataToSign = hash, nonce = kVal)

        val updated = nonceSignatureDb.copy(attestationOpt = Some(sig.sig),
                                            outcomeOpt =
                                              Some(outcome.outcomeString))
        oracleDataManagement.updateNonceSignatureDbAction(updated)
      } else {
        sys.error(
          s"Unknown outcome in database for what oracle attestted to, outcome=$outcome rValDb=$rValDb nonceSignatureDb=$nonceSignatureDb")
      }
    }
  }

  private def createAttestationV0Action(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType,
      rValDb: RValueDb): Future[DBIOAction[EventDb, NoStream, Effect.Write]] = {
    for {
      eventOpt <- eventDAO.read(nonce)
      eventDb <- eventOpt match {
        case Some(value) =>
          require(
            value.attestationOpt.isEmpty,
            s"Event already has been signed, attestation: ${value.sigOpt.get.hex}")
          Future.successful(value)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No event saved with nonce ${nonce.hex} $outcome"))
      }
      sigVersion = eventDb.signingVersion

      hash = sigVersion.calcOutcomeHash(outcome.outcomeString)
      kVal = getKValue(rValDb, sigVersion)
      _ = require(
        kVal.schnorrNonce == rValDb.nonce,
        s"The nonce from derived seed did not match database, db=${rValDb.nonce.hex} derived=${kVal.schnorrNonce.hex}, rValDb=$rValDb"
      )
      sig = {
        val privKey = eventDb.eventDescriptorTLV match {
          case _: EventDescriptorTLV | _: NumericEventDescriptorTLV =>
            //old format doesn't use attestation key
            announcementPrivKey
          case _: EventDescriptorDLCType | _: NumericEventDescriptorDLCType =>
            attestationPrivKey
        }
        privKey.schnorrSignWithNonce(dataToSign = hash, nonce = kVal)
      }

      updated = eventDb.copy(attestationOpt = Some(sig.sig),
                             outcomeOpt = Some(outcome.outcomeString))
      eventUpdateA = eventDAO.updateAction(updated)
    } yield eventUpdateA
  }

  private def createAttestationActionF(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[
    DBIOAction[FieldElement, NoStream, Effect.Write with Effect.Read]] = {
    val announcementNoncePairOptF =
      oracleDataManagement.getAnnouncmementByNonce(nonce)

    //try to sign the old version of the announcement'
    for {
      rValDbOpt <- rValueDAO.read(nonce)
      rValDb <- rValDbOpt match {
        case Some(value) => Future.successful(value)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"Nonce not found from this oracle ${nonce.hex}"))
      }
      announcementNoncePairOpt <- announcementNoncePairOptF
      action <- {
        announcementNoncePairOpt match {
          case Some((_, nonceSignatureDb)) =>
            val action = createAttestationV1NonceAction(
              nonceSignatureDb = nonceSignatureDb,
              rValDb = rValDb,
              outcome = outcome,
              signingVersion = SigningVersion.latest)
            Future.successful(action.map(_.attestationOpt.get))
          case None =>
            createAttestationV0Action(nonce = nonce,
                                      outcome = outcome,
                                      rValDb = rValDb)
              .map(_.map(_.attestationOpt.get))
        }
      }

    } yield action
  }

  /** Signs the event for the single nonce
    * This will be called multiple times by signDigits for each nonce
    */
  override def createAttestation(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[FieldElement] = {
    val actionF = createAttestationActionF(nonce, outcome)
    actionF.flatMap(action => safeDatabase.run(action))
  }

  override def signDigits(
      eventName: String,
      num: Long): Future[CompletedDigitDecompositionV0OracleEvent] = {
    for {
      eventOpt <- findEvent(eventName)
      _ = require(eventOpt.isDefined,
                  s"No event found by event name $eventName")
      res <- signDigits(eventOpt.get.announcementTLV, num)
    } yield res
  }

  override def signDigits(
      oracleAnnouncement: BaseOracleAnnouncement,
      num: Long): Future[CompletedDigitDecompositionV0OracleEvent] = {

    oracleAnnouncement match {
      case v1: OracleAnnouncementV1TLV =>
        logger.info(
          s"Signing announcement v1 digits for eventName=${oracleAnnouncement.eventTLV.eventId} num=$num")
        signDigitsV1Announcement(announcementV1 = v1, num = num)
      case v0: OracleAnnouncementV0TLV =>
        logger.info(
          s"Signing announcement v0 digits for eventName=${oracleAnnouncement.eventTLV.eventId} num=$num")
        signDigitsV0Announcement(announcementV0 = v0, num = num)
    }
  }

  private def signDigitsV0Announcement(
      announcementV0: OracleAnnouncementV0TLV,
      num: Long): Future[CompletedDigitDecompositionV0OracleEvent] = {
    signNumericAnnouncement(announcement = announcementV0,
                            num = num,
                            nonces = announcementV0.eventTLV.nonces)
  }

  private def signDigitsV1Announcement(
      announcementV1: OracleAnnouncementV1TLV,
      num: Long): Future[CompletedDigitDecompositionV0OracleEvent] = {
    signNumericAnnouncement(
      announcement = announcementV1,
      num = num,
      nonces = announcementV1.metadata.attestations.nonces.toVector)
  }

  private def signNumericAnnouncement(
      announcement: BaseOracleAnnouncement,
      num: Long,
      nonces: Vector[SchnorrNonce]): Future[
    CompletedDigitDecompositionV0OracleEvent] = {
    val oracleEventV0TLV = announcement.eventTLV

    val eventDescriptorTLV: BaseNumericEventDescriptorTLV = {
      oracleEventV0TLV.eventDescriptor match {
        case _: EnumEventDescriptorDLCSubType | _: EnumEventDescriptorV0TLV =>
          throw new IllegalArgumentException(
            "Must have a DigitDecomposition event descriptor use signEvent instead")
        case decomp: BaseNumericEventDescriptorTLV =>
          decomp
      }
    }

    // Make this a vec so it is easier to add on
    val signSigF =
      eventDescriptorTLV match {
        case _: SignedDigitDecompositionEventDescriptorDLCType |
            _: SignedDigitDecompositionEventDescriptor =>
          val signOutcome = DigitDecompositionSignAttestation(num >= 0)
          createAttestation(nonces.head, signOutcome)
            .map(db => Vector(db))
        case _: UnsignedDigitDecompositionEventDescriptorDLCType |
            _: UnsignedDigitDecompositionEventDescriptor =>
          FutureUtil.emptyVec[EventDb]
      }

    val boundedNum = if (num < eventDescriptorTLV.minNum) {
      logger.info(
        s"Number given $num is less than the minimum, signing minimum instead")
      eventDescriptorTLV.minNum.toLong
    } else if (num > eventDescriptorTLV.maxNum) {
      logger.info(
        s"Number given $num is greater than the maximum, signing maximum instead")
      eventDescriptorTLV.maxNum.toLong
    } else num

    val decomposed = NumberUtil.decompose(Math.abs(boundedNum),
                                          eventDescriptorTLV.base.toInt,
                                          eventDescriptorTLV.numDigits.toInt)

    val digitNonces: Vector[SchnorrNonce] = eventDescriptorTLV match {
      case _: UnsignedDigitDecompositionEventDescriptor |
          _: UnsignedDigitDecompositionEventDescriptorDLCType =>
        nonces
      case _: SignedDigitDecompositionEventDescriptor |
          _: SignedDigitDecompositionEventDescriptorDLCType =>
        nonces.tail
    }

    val digitSigAVecF: Future[Vector[
      DBIOAction[FieldElement, NoStream, Effect.Write with Effect.Read]]] =
      Future.sequence {
        digitNonces.zipWithIndex.map { case (nonce, index) =>
          val digit = decomposed(index)
          createAttestationActionF(nonce, DigitDecompositionAttestation(digit))
        }
      }
    val digitSigAF: Future[DBIOAction[
      Vector[FieldElement],
      NoStream,
      Effect.Write with Effect.Read]] = {
      digitSigAVecF.map(digitSigs => DBIO.sequence(digitSigs))
    }

    for {
      _ <- signSigF
      digitSigA <- digitSigAF
      _ <- safeDatabase.run(digitSigA)
      //read event again after writing all attestations
      _ = logger.info(
        s"Retreiving ${announcement.eventTLV.eventId} now that we have signed")
      eventOpt <- findEvent(announcement.eventTLV.eventId)
    } yield {
      eventOpt match {
        case Some(event) =>
          event match {
            case enum @ (_: CompletedEnumV0OracleEvent |
                _: PendingEnumV0OracleEvent) =>
              sys.error(
                s"Cannot have an enum oracle event when signing a numeric announcement, got=$enum")
            case pending: PendingDigitDecompositionV0OracleEvent =>
              sys.error(
                s"Cannot have a pending numeric oracle event after signing it! got=$pending")
            case numeric: CompletedDigitDecompositionV0OracleEvent =>
              numeric
          }
        case None =>
          sys.error(
            s"Should be able to read event after creating attations! eventName=${announcement.eventTLV.eventId}")
      }
    }
  }

  /** @inheritdoc */
  def signMessage(message: ByteVector): SchnorrDigitalSignature = {
    val hash = CryptoUtil.sha256(message)
    announcementPrivKey.schnorrSign(hash.bytes)
  }

  /** @inheritdoc */
  override def deleteAnnouncement(
      eventName: String): Future[BaseOracleAnnouncement] = {
    logger.warn(s"Deleting announcement with name=$eventName")

    for {
      eventOpt <- findEvent(eventName)
      _ = require(eventOpt.isDefined,
                  s"No announcement found by event name $eventName")
      event = eventOpt.get
      deletedAnnouncement <- {
        event.announcementTLV match {
          case v0: OracleAnnouncementV0TLV =>
            deleteV0Announcement(v0)
          case v1: OracleAnnouncementV1TLV =>
            deleteV1Announcement(v1)
        }
      }
    } yield deletedAnnouncement
  }

  private def deleteV0Announcement(
      announcementV0TLV: OracleAnnouncementV0TLV): Future[
    OracleAnnouncementV0TLV] = {
    for {
      eventDbs <- eventDAO.findByAnnouncement(announcementV0TLV)
      _ = require(
        eventDbs.forall(_.attestationOpt.isEmpty),
        s"Cannot have attesations defined when deleting an announcement, name=${announcementV0TLV.eventTLV.eventId}"
      )
      nonces = eventDbs.map(_.nonce)
      rVals <- rValueDAO.findByNonces(nonces)
      outcomeDbs <- eventOutcomeDAO.findByNonces(nonces)
      _ <- eventOutcomeDAO.deleteAll(outcomeDbs)
      _ <- rValueDAO.deleteAll(rVals)
      _ <- eventDAO.deleteAll(eventDbs)
    } yield announcementV0TLV
  }

  private def deleteV1Announcement(
      announcementV1TLV: OracleAnnouncementV1TLV): Future[
    OracleAnnouncementV1TLV] = {
    val announcementsA = oracleDataManagement
      .getByEventNameAction(announcementV1TLV.eventTLV.eventId)
    val action = for {
      announcements <- announcementsA
      nested = announcements.map { annWithId =>
        val attestationOptA =
          oracleDataManagement.getAttestmentsAction(annWithId.id)
        attestationOptA.flatMap {
          case Some(attestation) =>
            val eventId = annWithId.announcement.eventTLV.eventId
            sys.error(
              s"Cannot delete announcement=${eventId} that has attestations, attestation=$attestation")
          case None =>
            oracleDataManagement.deleteAnnouncementAction(
              annWithId.announcement)
        }
      }
      _ <- DBIO.sequence(nested)
    } yield {
      announcementV1TLV
    }

    safeDatabase.run(action)
  }

  /** @inheritdoc */
  override def deleteAnnouncement(
      announcementTLV: BaseOracleAnnouncement): Future[
    BaseOracleAnnouncement] = {
    deleteAnnouncement(announcementTLV.eventTLV.eventId.toString)
  }

  /** Deletes attestations for the given event
    *
    * WARNING: if previous signatures have been made public
    * the oracle private key will be revealed.
    */
  override def deleteAttestation(eventName: String): Future[OracleEvent] = {
    for {
      eventOpt <- findEvent(eventName)
      _ = require(eventOpt.isDefined,
                  s"No event found by event name $eventName")
      res <- deleteAttestation(eventOpt.get.announcementTLV)
    } yield res
  }

  /** Deletes attestations for the given event
    *
    * WARNING: if previous signatures have been made public
    * the oracle private key will be revealed.
    */
  override def deleteAttestation(
      announcement: BaseOracleAnnouncement): Future[OracleEvent] = {
    val eventId = announcement.eventTLV.eventId
    logger.warn(s"Attempting to delete attestations for $eventId")
    announcement match {
      case _: OracleAnnouncementV0TLV =>
        for {
          eventDbs <- eventDAO.findByAnnouncement(announcement)
          _ = require(eventDbs.exists(_.attestationOpt.isDefined),
                      s"Event given is unsigned, eventId=$eventId")

          updated = eventDbs.map(
            _.copy(outcomeOpt = None, attestationOpt = None))
          _ <- eventDAO.updateAll(updated)
          oracleEvents <- getFullOracleEvents(eventDbs)
        } yield oracleEvents.head
      case v1: OracleAnnouncementV1TLV =>
        val deleteAttestationsAction = for {
          announcementVec <- oracleDataManagement.getByEventNameAction(
            v1.eventTLV.eventId)
          nonces <- {
            announcementVec.headOption match {
              case Some(announcement) =>
                oracleDataManagement.getNonceSchnorrDbAction(announcement.id)
              case None =>
                DBIO.successful(Vector.empty)
            }
          }
          _ = require(nonces.exists(_.attestationOpt.isDefined),
                      s"Event given is unsigned, eventId=$eventId")
          //remove attestation/outcome
          noAttestations = nonces.map(n =>
            n.copy(attestationOpt = None, outcomeOpt = None))
          deleteActionNested = noAttestations.map(a =>
            oracleDataManagement.updateNonceSignatureDbAction(a))
          deleteAction <- DBIO.sequence(deleteActionNested)
        } yield deleteAction
        val deletedF = safeDatabase.run(deleteAttestationsAction)

        for {
          _ <- deletedF
          event <- findEvent(announcement)
        } yield event.get
    }
  }

  override def oracleName(): Future[Option[String]] = {
    masterXpubDAO.findXPub().map(_.name)
  }

  override def setOracleName(name: String): Future[Unit] = {
    masterXpubDAO.updateName(name)
  }

  override def exportSigningKeyWIF: String = {
    ECPrivateKeyUtil.toWIF(privKey =
                             announcementPrivKey.toPrivateKeyBytes(false),
                           network = MainNet)
  }
}

object DLCOracle {

  // 585 is a random one I picked, unclaimed in https://github.com/satoshilabs/slips/blob/master/slip-0044.md
  val R_VALUE_PURPOSE = 585

  /** Gets the DLC oracle from the given datadir */
  def fromDatadir(path: Path, configs: Vector[Config])(implicit
      ec: ExecutionContext): Future[DLCOracle] = {
    implicit val appConfig =
      DLCOracleAppConfig.fromDatadir(datadir = path, configs)
    val masterXpubDAO: MasterXPubDAO = MasterXPubDAO()(ec, appConfig)
    val oracle = DLCOracle()

    for {
      _ <- appConfig.start()
      _ <- MasterXPubUtil.checkMasterXPub(oracle.getRootXpub, masterXpubDAO)
      differentKeyDbs <- oracle.eventDAO.findDifferentPublicKey(
        oracle.announcementPublicKey)
      fixedDbs = differentKeyDbs.map(
        _.copy(pubkey = oracle.announcementPublicKey))
      _ <- oracle.eventDAO.updateAll(fixedDbs)
    } yield oracle
  }
}
