package org.bitcoins.dlc.oracle

import com.typesafe.config.Config
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.api.dlcoracle.db._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.ExtKeyVersion.SegWitTestNet3Priv
import org.bitcoins.core.crypto.{ExtPrivateKeyHardened, ExtPublicKey}
import org.bitcoins.core.hd._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.sorted.OrderedNonces
import org.bitcoins.core.util.{FutureUtil, NumberUtil, TimeUtil}
import org.bitcoins.crypto._
import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.db.util.MasterXPubUtil
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.dlc.oracle.util.EventDbUtil
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

  private def signingKey: ECPrivateKey = {
    val coin = HDCoin(HDPurposes.SegWit, coinType)
    val account = HDAccount(coin, 0)
    val purpose = coin.purpose
    val chain = HDChainType.External
    val index = 0

    val path = BIP32Path.fromHardenedString(
      s"m/${purpose.constant}'/${coin.coinType.toInt}'/${account.index}'/${chain.index}'/$index'")

    extPrivateKey.deriveChildPrivKey(path).key
  }

  override val publicKey: SchnorrPublicKey = signingKey.schnorrPublicKey

  override def stakingAddress(network: BitcoinNetwork): Bech32Address =
    Bech32Address(P2WPKHWitnessSPKV0(publicKey.publicKey), network)

  protected[bitcoins] val rValueDAO: RValueDAO = RValueDAO()
  protected[bitcoins] val eventDAO: EventDAO = EventDAO()
  protected[bitcoins] val eventOutcomeDAO: EventOutcomeDAO = EventOutcomeDAO()

  protected[bitcoins] val masterXpubDAO: MasterXPubDAO =
    MasterXPubDAO()(ec, conf)

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
    eventDAO.findAll().map { eventDbs =>
      val events = eventDbs.groupBy(_.announcementSignature)
      events.values.map(dbs => OracleEvent.fromEventDbs(dbs)).toVector
    }
  }

  override def listPendingEvents(): Future[Vector[OracleEvent]] = {
    listPendingEventDbs().map { eventDbs =>
      val events = eventDbs.groupBy(_.announcementSignature)
      events.values.map(dbs => OracleEvent.fromEventDbs(dbs)).toVector
    }
  }

  override def listCompletedEvents(): Future[Vector[OracleEvent]] = {
    listCompletedEventDbs().map { eventDbs =>
      val events = eventDbs.groupBy(_.announcementSignature)
      events.values.map(dbs => OracleEvent.fromEventDbs(dbs)).toVector
    }
  }

  override def findEvent(
      oracleEventTLV: OracleEventTLV): Future[Option[OracleEvent]] = {
    eventDAO.findByOracleEventTLV(oracleEventTLV).map { dbs =>
      if (dbs.isEmpty) {
        None
      } else Some(OracleEvent.fromEventDbs(dbs))
    }
  }

  override def findEvent(eventName: String): Future[Option[OracleEvent]] = {
    eventDAO.findByEventName(eventName).map { dbs =>
      if (dbs.isEmpty) {
        None
      } else Some(OracleEvent.fromEventDbs(dbs))
    }
  }

  override def createNewDigitDecompAnnouncement(
      eventName: String,
      maturationTime: Instant,
      base: UInt16,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int32): Future[OracleAnnouncementTLV] = {
    require(base > UInt16.zero,
            s"base cannot be less than 1, got ${base.toInt}")
    require(numDigits > 0, s"numDigits cannot be less than 1, got $numDigits")

    val descriptorTLV = DigitDecompositionEventDescriptorV0TLV(base,
                                                               isSigned,
                                                               numDigits,
                                                               unit,
                                                               precision)

    createNewAnnouncement(eventName, maturationTime, descriptorTLV)
  }

  override def createNewEnumAnnouncement(
      eventName: String,
      maturationTime: Instant,
      outcomes: Vector[String]): Future[OracleAnnouncementTLV] = {
    require(outcomes.nonEmpty, "Cannot make an event with no outcomes")
    require(outcomes.distinct.size == outcomes.size,
            s"Cannot have duplicate outcomes, got $outcomes")

    val descriptorTLV = EnumEventDescriptorV0TLV(outcomes)

    createNewAnnouncement(eventName, maturationTime, descriptorTLV)
  }

  override def createNewAnnouncement(
      eventName: String,
      maturationTime: Instant,
      descriptor: EventDescriptorTLV,
      signingVersion: SigningVersion = SigningVersion.latest): Future[
    OracleAnnouncementTLV] = {
    require(maturationTime.isAfter(TimeUtil.now),
            s"Event cannot mature in the past, got $maturationTime")

    for {
      dbs <- eventDAO.findByEventName(eventName)
      _ = require(dbs.isEmpty, s"Event name ($eventName) is already being used")

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

      epoch = UInt32(maturationTime.getEpochSecond)

      nonces = rValueDbs.map(_.nonce)

      eventTLV = OracleEventV0TLV(OrderedNonces(nonces),
                                  epoch,
                                  descriptor,
                                  eventName)

      announcementBytes = signingVersion.calcAnnouncementHash(eventTLV)
      announcementSignature = signingKey.schnorrSign(announcementBytes)

      oracleAnnouncement = OracleAnnouncementV0TLV(announcementSignature =
                                                     announcementSignature,
                                                   publicKey = publicKey,
                                                   eventTLV = eventTLV)

      eventOutcomeDbs = EventDbUtil.toEventOutcomeDbs(
        oracleAnnouncementV0TLV = oracleAnnouncement,
        signingVersion = signingVersion)

      eventDbs = EventDbUtil.toEventDbs(oracleAnnouncementV0TLV =
                                          oracleAnnouncement,
                                        eventName = eventName,
                                        signingVersion = signingVersion)

      _ <- rValueDAO.createAll(rValueDbs)
      _ <- eventDAO.createAll(eventDbs)
      _ <- eventOutcomeDAO.createAll(eventOutcomeDbs)
    } yield {
      OracleEvent.fromEventDbs(eventDbs).announcementTLV
    }
  }

  override def signEnum(
      eventName: String,
      outcome: EnumAttestation): Future[EventDb] = {
    for {
      eventDbs <- eventDAO.findByEventName(eventName)
      _ = require(eventDbs.size == 1,
                  "Use signLargeRange for signing multi nonce outcomes")

      sign <- createAttestation(eventDbs.head.nonce, outcome)
    } yield sign
  }

  override def signEnum(
      oracleEventTLV: OracleEventTLV,
      outcome: EnumAttestation): Future[EventDb] = {
    for {
      eventDbs <- eventDAO.findByOracleEventTLV(oracleEventTLV)
      _ = require(eventDbs.size == 1,
                  "Use signLargeRange for signing multi nonce outcomes")

      sign <- createAttestation(eventDbs.head.nonce, outcome)
    } yield sign
  }

  /** Signs the event for the single nonce
    * This will be called multiple times by signDigits for each nonce
    */
  override def createAttestation(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[EventDb] = {
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

      hash = eventDb.signingVersion.calcOutcomeHash(eventDb.eventDescriptorTLV,
                                                    outcome.outcomeString)
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
      sig = signingKey.schnorrSignWithNonce(hashBytes, kVal)

      updated = eventDb.copy(attestationOpt = Some(sig.sig),
                             outcomeOpt = Some(outcome.outcomeString))
      _ <- eventDAO.update(updated)
    } yield updated
  }

  override def signDigits(eventName: String, num: Long): Future[OracleEvent] = {
    for {
      eventOpt <- findEvent(eventName)
      _ = require(eventOpt.isDefined,
                  s"No event found by event name $eventName")
      res <- signDigits(eventOpt.get.announcementTLV.eventTLV, num)
    } yield res
  }

  override def signDigits(
      oracleEventTLV: OracleEventTLV,
      num: Long): Future[OracleEvent] = {

    val eventDescriptorTLV: DigitDecompositionEventDescriptorV0TLV = {
      oracleEventTLV.eventDescriptor match {
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
          createAttestation(oracleEventTLV.nonces.head, signOutcome).map(db =>
            Vector(db))
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

    val nonces = eventDescriptorTLV match {
      case _: UnsignedDigitDecompositionEventDescriptor =>
        oracleEventTLV.nonces
      case _: SignedDigitDecompositionEventDescriptor =>
        oracleEventTLV.nonces.tail
    }

    val digitSigFs = nonces.zipWithIndex.map { case (nonce, index) =>
      val digit = decomposed(index)
      createAttestation(nonce, DigitDecompositionAttestation(digit))
    }

    for {
      signSig <- signSigF
      digitSigs <- Future.sequence(digitSigFs)
    } yield OracleEvent.fromEventDbs(signSig ++ digitSigs)
  }

  /** @inheritdoc */
  def signMessage(message: ByteVector): SchnorrDigitalSignature = {
    val hash = CryptoUtil.sha256(message)
    signingKey.schnorrSign(hash.bytes)
  }

  /** @inheritdoc */
  override def deleteAnnouncement(
      eventName: String): Future[OracleAnnouncementTLV] = {
    logger.warn(s"Deleting announcement with name=$eventName")
    for {
      eventOpt <- findEvent(eventName)
      _ = require(eventOpt.isDefined,
                  s"No announcement found by event name $eventName")
      event = eventOpt.get
      eventDbs <- eventDAO.findByOracleEventTLV(event.eventTLV)
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
      announcementTLV: OracleAnnouncementTLV): Future[OracleAnnouncementTLV] = {
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
      res <- deleteAttestation(eventOpt.get.eventTLV)
    } yield res
  }

  /** Deletes attestations for the given event
    *
    * WARNING: if previous signatures have been made public
    * the oracle private key will be revealed.
    */
  override def deleteAttestation(
      oracleEventTLV: OracleEventTLV): Future[OracleEvent] = {
    for {
      eventDbs <- eventDAO.findByOracleEventTLV(oracleEventTLV)
      _ = require(eventDbs.exists(_.attestationOpt.isDefined),
                  s"Event given is unsigned")

      updated = eventDbs.map(_.copy(outcomeOpt = None, attestationOpt = None))
      _ <- eventDAO.updateAll(updated)
    } yield OracleEvent.fromEventDbs(eventDbs)
  }

  override def oracleName(): Future[Option[String]] = {
    masterXpubDAO.findXPub().map(_.name)
  }

  override def setOracleName(name: String): Future[Unit] = {
    masterXpubDAO.updateName(name)
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
        oracle.publicKey)
      fixedDbs = differentKeyDbs.map(_.copy(pubkey = oracle.publicKey))
      _ <- oracle.eventDAO.updateAll(fixedDbs)
    } yield oracle
  }
}
