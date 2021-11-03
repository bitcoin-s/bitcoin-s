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

  override def listEventDbs(): Future[Vector[EventDb]] = eventDAO.findAll()

  override def listPendingEventDbs(): Future[Vector[EventDb]] =
    eventDAO.getPendingEvents

  override def listCompletedEventDbs(): Future[Vector[EventDb]] =
    eventDAO.getCompletedEvents

  override def listEvents(): Future[Vector[OracleEvent]] = {
    eventDAO.findAll().flatMap { eventDbs =>
      getFullOracleEvents(eventDbs)
    }
  }

  private def getFullOracleEvents(
      eventDbs: Vector[EventDb]): Future[Vector[OracleEvent]] = {
    val eventDb = eventDbs.head
    eventDb.attestationOpt match {
      case Some(_) => getCompleteOracleEvent(eventDbs)
      case None    => getPendingOracleEvent(eventDbs)
    }
  }

  private def getPendingOracleEvent(
      eventDbs: Vector[EventDb]): Future[Vector[PendingOracleEvent]] = {
    val events = eventDbs.groupBy(_.announcementSignature)
    val resultNested = events.values.map { dbs =>
      require(
        dbs.map(_.pubkey).distinct.length == 1,
        s"Attestation pubkey must be the same for all events with the same announcement signature")
      val metadataOptF =
        oracleDataManagement.findMetadataByAttestationPubKey(dbs.head.pubkey)
      metadataOptF.map { metadataOpt =>
        val e = OracleEvent.fromPendingEventDbs(dbs, metadataOpt)
        Some(e)
      }
    }

    Future
      .sequence(resultNested)
      .map(_.flatten.toVector)
  }

  private def getCompleteOracleEvent(
      eventDbs: Vector[EventDb]): Future[Vector[CompletedOracleEvent]] = {
    val events = eventDbs.groupBy(_.announcementSignature)
    val resultNested = events.values.map { dbs =>
      require(
        dbs.map(_.pubkey).distinct.length == 1,
        s"Attestation pubkey must be the same for all events with the same announcement signature")
      val metadataOptF =
        oracleDataManagement.findMetadataByAttestationPubKey(dbs.head.pubkey)
      metadataOptF.map { metadataOpt =>
        val e = OracleEvent.fromCompletedEventDbs(dbs, metadataOpt)
        Some(e)
      }
    }

    Future
      .sequence(resultNested)
      .map(_.flatten.toVector)
  }

  override def listPendingEvents(): Future[Vector[PendingOracleEvent]] = {
    listPendingEventDbs().flatMap { eventDbs =>
      getPendingOracleEvent(eventDbs)
    }
  }

  override def listCompletedEvents(): Future[Vector[CompletedOracleEvent]] = {
    listCompletedEventDbs().flatMap { eventDbs =>
      getCompleteOracleEvent(eventDbs)
    }
  }

  override def findEvent(
      announcement: BaseOracleAnnouncement): Future[Option[OracleEvent]] = {
    eventDAO.findByAnnouncement(announcement).flatMap { dbs =>
      if (dbs.isEmpty) {
        Future.successful(None)
      } else {
        getFullOracleEvents(dbs)
          .map(_.headOption)
      }
    }
  }

  override def findEvent(eventName: String): Future[Option[OracleEvent]] = {
    eventDAO.findByEventName(eventName).flatMap { dbs =>
      if (dbs.isEmpty) {
        Future.successful(None)
      } else {
        getFullOracleEvents(dbs)
          .map(_.headOption)
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
      precision: Int32): Future[BaseOracleAnnouncement] = {
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
      outcomes: Vector[String]): Future[BaseOracleAnnouncement] = {
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
    BaseOracleAnnouncement] = {
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
    } yield created

  }

  override def signEnum(
      eventName: String,
      outcome: EnumAttestation): Future[CompletedEnumV0OracleEvent] = {
    logger.info(s"Signing enum eventName=$eventName outcome=$outcome")
    for {
      eventDbs <- eventDAO.findByEventName(eventName)
      _ = require(eventDbs.size == 1,
                  "Use signLargeRange for signing multi nonce outcomes")

      sign <- createAttestation(eventDbs.head.nonce, outcome)
      oracleEvent <- getCompleteOracleEvent(Vector(sign))
        .map(_.head)
    } yield {
      oracleEvent match {
        case enum: CompletedEnumV0OracleEvent => enum
        case numeric: CompletedDigitDecompositionV0OracleEvent =>
          sys.error(
            s"Cannot have a numeric event when signing enum event, got=$numeric")
      }
    }
  }

  override def signEnum(
      oracleEventTLV: BaseOracleEvent,
      outcome: EnumAttestation): Future[CompletedEnumV0OracleEvent] = {
    logger.info(
      s"Signing enum eventName=${oracleEventTLV.eventId} outcome=$outcome")
    signEnum(oracleEventTLV.eventId, outcome)
  }

  private def createAttestationActionF(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[
    DBIOAction[EventDb, NoStream, Effect.Write]] = {
    for {
      rValDbOpt <- rValueDAO.read(nonce)
      rValDb <- rValDbOpt match {
        case Some(value) => Future.successful(value)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"Nonce not found from this oracle ${nonce.hex}"))
      }

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

      hash = eventDb.signingVersion
        .calcOutcomeHash(outcome.outcomeString)
      eventOutcomeOpt <- eventOutcomeDAO.find(nonce, hash)
      eventOutcomeDb <- eventOutcomeOpt match {
        case Some(value) => Future.successful(value)
        case None =>
          Future.failed(new IllegalArgumentException(
            s"No event outcome saved with nonce and message ${nonce.hex} ${outcome.outcomeString}"))
      }

      sigVersion = eventDb.signingVersion

      kVal = getKValue(rValDb, sigVersion)
      _ = require(
        kVal.schnorrNonce == rValDb.nonce,
        s"The nonce from derived seed did not match database, db=${rValDb.nonce.hex} derived=${kVal.schnorrNonce.hex}, rValDb=$rValDb"
      )

      hashBytes = eventOutcomeDb.hashedMessage
      sig = attestationPrivKey.schnorrSignWithNonce(hashBytes, kVal)

      updated = eventDb.copy(attestationOpt = Some(sig.sig),
                             outcomeOpt = Some(outcome.outcomeString))
      eventUpdateA = eventDAO.updateAction(updated)
    } yield eventUpdateA
  }

  /** Signs the event for the single nonce
    * This will be called multiple times by signDigits for each nonce
    */
  override def createAttestation(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[EventDb] = {
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
    logger.info(
      s"Signing digits for eventName=${oracleAnnouncement.eventTLV.eventId} num=$num")
    oracleAnnouncement match {
      case v1: OracleAnnouncementV1TLV =>
        signDigitsV1Announcement(announcementV1 = v1, num = num)
      case v0: OracleAnnouncementV0TLV =>
        signDigitsV0Announcement(announcementV0 = v0, num = num)
    }
  }

  private def signDigitsV0Announcement(
      announcementV0: OracleAnnouncementV0TLV,
      num: Long): Future[CompletedDigitDecompositionV0OracleEvent] = {
    val oracleEventV0TLV = announcementV0.eventTLV
    val eventDescriptorTLV: DigitDecompositionEventDescriptorV0TLV = {
      oracleEventV0TLV.eventDescriptor match {
        case _: EnumEventDescriptorV0TLV =>
          throw new IllegalArgumentException(
            "Must have a DigitDecomposition event descriptor use signEvent instead")
        case decomp: DigitDecompositionEventDescriptorV0TLV =>
          decomp
      }
    }

    // Make this a vec so it is easier to add on
    val signSigF =
      eventDescriptorTLV match {
        case _: SignedDigitDecompositionEventDescriptor =>
          val signOutcome = DigitDecompositionSignAttestation(num >= 0)
          val signNonce = oracleEventV0TLV match {
            case v0: OracleEventV0TLV => v0.nonces.head
            case v1: OracleEventV1TLV =>
              sys.error(s"Cannot have a v1 event when creating a v0 attestation, got=$v1")
          }
          createAttestation(signNonce, signOutcome).map(db => Vector(db))

        case _: UnsignedDigitDecompositionEventDescriptor =>
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

    val oracleEventNonces = oracleEventV0TLV match {
      case v0: OracleEventV0TLV =>
        v0.nonces
      case v1: OracleEventV1TLV =>
        sys.error(s"Cannot have a v1 event when creating a v0 attestation, got=$v1")
    }
    val nonces = eventDescriptorTLV match {
      case _: UnsignedDigitDecompositionEventDescriptor =>
        oracleEventNonces
      case _: SignedDigitDecompositionEventDescriptor =>
        oracleEventNonces.tail
    }

    val digitSigAVecF: Future[
      Vector[DBIOAction[EventDb, NoStream, Effect.Write]]] = Future.sequence {
      nonces.zipWithIndex.map { case (nonce, index) =>
        val digit = decomposed(index)
        createAttestationActionF(nonce, DigitDecompositionAttestation(digit))
      }.toVector
    }
    val digitSigAF: Future[
      DBIOAction[Vector[EventDb], NoStream, Effect.Write]] = {
      digitSigAVecF.map(digitSigs => DBIO.sequence(digitSigs))
    }

    for {
      signSig <- signSigF
      digitSigA <- digitSigAF
      digitSigs <- safeDatabase.run(digitSigA)
    } yield {
      val event = OracleEvent.fromCompletedEventDbs(
        eventDbs = signSig ++ digitSigs,
        metadataOpt = None
      )
      event match {
        case enum: CompletedEnumV0OracleEvent =>
          sys.error(
            s"Cannot have an enum oracle event when signing a numeric announcement, got=$enum")
        case numeric: CompletedDigitDecompositionV0OracleEvent =>
          numeric
      }
    }
  }

  private def signDigitsV1Announcement(
      announcementV1: OracleAnnouncementV1TLV,
      num: Long): Future[CompletedDigitDecompositionV0OracleEvent] = {
    val oracleEventV0TLV = announcementV1.eventTLV
    val metadata = announcementV1.metadata
    val metadataNonces = metadata.attestations.nonces

    val eventDescriptorTLV: DigitDecompositionEventDescriptorDLCType = {
      oracleEventV0TLV.eventDescriptor match {
        case _: EnumEventDescriptorDLCSubType =>
          throw new IllegalArgumentException(
            "Must have a DigitDecomposition event descriptor use signEvent instead")
        case decomp: DigitDecompositionEventDescriptorDLCType =>
          decomp
      }
    }

    // Make this a vec so it is easier to add on
    val signSigF =
      eventDescriptorTLV match {
        case _: SignedDigitDecompositionEventDescriptorDLCType =>
          val signOutcome = DigitDecompositionSignAttestation(num >= 0)
          createAttestation(metadataNonces.head, signOutcome)
            .map(db => Vector(db))
        case _: UnsignedDigitDecompositionEventDescriptorDLCType =>
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

    val nonces: OrderedNonces = eventDescriptorTLV match {
      case _: UnsignedDigitDecompositionEventDescriptor |
          _: UnsignedDigitDecompositionEventDescriptorDLCType =>
        metadataNonces
      case _: SignedDigitDecompositionEventDescriptor |
          _: SignedDigitDecompositionEventDescriptorDLCType =>
        OrderedNonces(metadataNonces.tail.toVector)
    }

    val digitSigAVecF: Future[
      Vector[DBIOAction[EventDb, NoStream, Effect.Write]]] = Future.sequence {
      nonces.zipWithIndex.map { case (nonce, index) =>
        val digit = decomposed(index)
        createAttestationActionF(nonce, DigitDecompositionAttestation(digit))
      }.toVector
    }
    val digitSigAF: Future[
      DBIOAction[Vector[EventDb], NoStream, Effect.Write]] = {
      digitSigAVecF.map(digitSigs => DBIO.sequence(digitSigs))
    }

    for {
      signSig <- signSigF
      digitSigA <- digitSigAF
      digitSigs <- safeDatabase.run(digitSigA)
    } yield {
      val event =
        OracleEvent.fromCompletedEventDbs(signSig ++ digitSigs, Some(metadata))
      event match {
        case enum: CompletedEnumV0OracleEvent =>
          sys.error(
            s"Cannot have an enum oracle event when signing a numeric announcement, got=$enum")
        case numeric: CompletedDigitDecompositionV0OracleEvent =>
          numeric
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
      eventDbs <- eventDAO.findByAnnouncement(event.announcementTLV)
      _ = require(
        eventDbs.forall(_.attestationOpt.isEmpty),
        s"Cannot have attesations defined when deleting an announcement, name=$eventName")
      nonces = eventDbs.map(_.nonce)
      rVals <- rValueDAO.findByNonces(nonces)
      outcomeDbs <- eventOutcomeDAO.findByNonces(nonces)
      _ <- eventOutcomeDAO.deleteAll(outcomeDbs)
      _ <- rValueDAO.deleteAll(rVals)
      _ <- eventDAO.deleteAll(eventDbs)
    } yield eventOpt.get.announcementTLV
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
    for {
      eventDbs <- eventDAO.findByAnnouncement(announcement)
      _ = require(eventDbs.exists(_.attestationOpt.isDefined),
                  s"Event given is unsigned")

      updated = eventDbs.map(_.copy(outcomeOpt = None, attestationOpt = None))
      _ <- eventDAO.updateAll(updated)
      oracleEvents <- getFullOracleEvents(eventDbs)
    } yield oracleEvents.head
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
