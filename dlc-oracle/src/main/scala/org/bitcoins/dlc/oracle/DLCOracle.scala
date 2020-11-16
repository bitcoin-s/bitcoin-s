package org.bitcoins.dlc.oracle

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.ExtKeyVersion.SegWitMainNetPriv
import org.bitcoins.core.crypto.{ExtPrivateKeyHardened, MnemonicCode}
import org.bitcoins.core.hd._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil, NumberUtil, TimeUtil}
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.keymanager.{DecryptedMnemonic, WalletStorage}

import scala.concurrent.{ExecutionContext, Future}

case class DLCOracle(private val extPrivateKey: ExtPrivateKeyHardened)(implicit
    val conf: DLCOracleAppConfig)
    extends BitcoinSLogger {

  implicit val ec: ExecutionContext = conf.ec

  private val rValAccount: HDAccount = {
    val purpose = conf.kmParams.purpose
    val coin =
      HDCoin(purpose, HDCoinType.fromNetwork(conf.network))
    HDAccount(coin, 0)
  }

  /** The chain index used in the bip32 paths for generating R values */
  private val rValueChainIndex = 0

  private def signingKey: ECPrivateKey = {
    val coin = HDCoin(HDPurposes.SegWit, HDCoinType.fromNetwork(conf.network))
    val account = HDAccount(coin, 0)
    val purpose = coin.purpose
    val chain = HDChainType.External
    val index = 0

    val path = BIP32Path.fromString(
      s"m/${purpose.constant}'/${coin.coinType.toInt}'/${account.index}'/${chain.index}'/$index'")

    extPrivateKey.deriveChildPrivKey(path).key
  }

  val publicKey: SchnorrPublicKey = signingKey.schnorrPublicKey

  def stakingAddress(network: BitcoinNetwork): Bech32Address =
    Bech32Address(P2WPKHWitnessSPKV0(publicKey.publicKey), network)

  protected[bitcoins] val rValueDAO: RValueDAO = RValueDAO()
  protected[bitcoins] val eventDAO: EventDAO = EventDAO()
  protected[bitcoins] val eventOutcomeDAO: EventOutcomeDAO = EventOutcomeDAO()

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

  def listEventDbs(): Future[Vector[EventDb]] = eventDAO.findAll()

  def listPendingEventDbs(): Future[Vector[EventDb]] = eventDAO.getPendingEvents

  def listEvents(): Future[Vector[OracleEvent]] = {
    eventDAO.findAll().map { eventDbs =>
      val events = eventDbs.groupBy(_.announcementSignature)
      events.values.map(dbs => OracleEvent.fromEventDbs(dbs)).toVector
    }
  }

  def findEvent(oracleEventTLV: OracleEventTLV): Future[Option[OracleEvent]] = {
    eventDAO.findByOracleEventTLV(oracleEventTLV).map { dbs =>
      if (dbs.isEmpty) {
        None
      } else Some(OracleEvent.fromEventDbs(dbs))
    }
  }

  def createNewLargeRangedEvent(
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

    createNewEvent(eventName, maturationTime, descriptorTLV)
  }

  def createNewRangedEvent(
      eventName: String,
      maturationTime: Instant,
      start: Int,
      count: Int,
      step: Int,
      unit: String,
      precision: Int): Future[OracleAnnouncementTLV] =
    createNewRangedEvent(eventName,
                         maturationTime,
                         Int32(start),
                         UInt32(count),
                         UInt16(step),
                         unit,
                         Int32(precision))

  def createNewRangedEvent(
      eventName: String,
      maturationTime: Instant,
      start: Int32,
      count: UInt32,
      step: UInt16,
      unit: String,
      precision: Int32): Future[OracleAnnouncementTLV] = {
    require(count > UInt32.zero,
            s"Count cannot be less than 1, got ${count.toInt}")
    require(step > UInt16.zero,
            s"Step cannot be less than 1, got ${step.toInt}")

    val descriptorTLV =
      RangeEventDescriptorV0TLV(start, count, step, unit, precision)

    createNewEvent(eventName, maturationTime, descriptorTLV)
  }

  def createNewEnumEvent(
      eventName: String,
      maturationTime: Instant,
      outcomes: Vector[String]): Future[OracleAnnouncementTLV] = {
    require(outcomes.nonEmpty, "Cannot make an event with no outcomes")
    require(outcomes.distinct.size == outcomes.size,
            s"Cannot have duplicate outcomes, got $outcomes")

    val descriptorTLV = EnumEventDescriptorV0TLV(outcomes)

    createNewEvent(eventName, maturationTime, descriptorTLV)
  }

  def createNewEvent(
      eventName: String,
      maturationTime: Instant,
      descriptor: EventDescriptorTLV,
      signingVersion: SigningVersion = SigningVersion.latest): Future[
    OracleAnnouncementTLV] = {
    require(maturationTime.isAfter(TimeUtil.now),
            s"Event cannot mature in the past, got $maturationTime")

    for {
      indexOpt <- rValueDAO.maxKeyIndex
      firstIndex = indexOpt match {
        case Some(value) => value + 1
        case None        => 0
      }

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

      eventTLV = OracleEventV0TLV(nonces, epoch, descriptor, eventName)

      announcementBytes = signingVersion.calcAnnouncementHash(eventTLV)
      announcementSignature = signingKey.schnorrSign(announcementBytes)

      eventOutcomeDbs = descriptor match {
        case enum: EnumEventDescriptorV0TLV =>
          require(rValueDbs.size == 1,
                  "Enum events should only have one R value")
          val nonce = rValueDbs.head.nonce
          enum.outcomes.map { outcome =>
            val attestationType = EnumAttestation(outcome)
            val hash =
              signingVersion.calcOutcomeHash(enum, attestationType.bytes)
            EventOutcomeDb(nonce, outcome, hash)
          }
        case range: RangeEventDescriptorV0TLV =>
          require(rValueDbs.size == 1,
                  "Range events should only have one R value")
          val nonce = rValueDbs.head.nonce

          val outcomes: Vector[Long] = {
            val startL = range.start.toLong
            val stepL = range.step.toLong

            val outcomeRange =
              0L.until(range.count.toLong).map(num => startL + (num * stepL))

            outcomeRange.toVector
          }

          outcomes.map { outcome =>
            val attestationType = RangeAttestation(outcome)
            val hash =
              signingVersion.calcOutcomeHash(range, attestationType.bytes)
            EventOutcomeDb(nonce, outcome.toString, hash)
          }
        case decomp: DigitDecompositionEventDescriptorV0TLV =>
          val signDbs = decomp match {
            case _: SignedDigitDecompositionEventDescriptor =>
              val plusHash = signingVersion.calcOutcomeHash(decomp, "+")
              val minusHash = signingVersion.calcOutcomeHash(decomp, "-")
              Vector(EventOutcomeDb(nonces.head, "+", plusHash),
                     EventOutcomeDb(nonces.head, "-", minusHash))
            case _: UnsignedDigitDecompositionEventDescriptor =>
              Vector.empty
          }

          val digitNonces = if (decomp.isSigned) nonces.tail else nonces

          val digitDbs = digitNonces.flatMap { nonce =>
            0.until(decomp.base.toInt).map { num =>
              val attestationType = DigitDecompositionAttestation(num)
              val hash =
                signingVersion.calcOutcomeHash(decomp, attestationType.bytes)
              EventOutcomeDb(nonce, num.toString, hash)
            }
          }

          signDbs ++ digitDbs
      }

      eventDbs = rValueDbs.zipWithIndex.map {
        case (db, index) =>
          EventDb(db.nonce,
                  publicKey,
                  index,
                  eventName,
                  eventOutcomeDbs.size,
                  signingVersion,
                  maturationTime,
                  None,
                  announcementSignature,
                  descriptor)
      }

      _ <- rValueDAO.createAll(rValueDbs)
      _ <- eventDAO.createAll(eventDbs)
      _ <- eventOutcomeDAO.createAll(eventOutcomeDbs)
    } yield {
      OracleEvent.fromEventDbs(eventDbs).announcementTLV
    }
  }

  def signEvent(
      oracleEventTLV: OracleEventTLV,
      outcome: DLCAttestationType): Future[EventDb] = {
    for {
      eventDbs <- eventDAO.findByOracleEventTLV(oracleEventTLV)
      _ = require(eventDbs.size == 1,
                  "Use signLargeRange for signing multi nonce outcomes")

      sign <- signEvent(eventDbs.head.nonce, outcome)
    } yield sign
  }

  def signEvent(
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

      eventOutcomeOpt <- eventOutcomeDAO.read((nonce, outcome.outcomeString))
      eventOutcomeDb <- eventOutcomeOpt match {
        case Some(value) => Future.successful(value)
        case None =>
          Future.failed(new IllegalArgumentException(
            s"No event outcome saved with nonce and message ${nonce.hex} ${outcome.outcomeString}"))
      }

      sigVersion = eventDb.signingVersion

      kVal = getKValue(rValDb, sigVersion)
      _ = require(kVal.schnorrNonce == rValDb.nonce,
                  "The nonce from derived seed did not match database")

      hashBytes = eventOutcomeDb.hashedMessage
      sig = signingKey.schnorrSignWithNonce(hashBytes, kVal)

      updated = eventDb.copy(attestationOpt = Some(sig.sig))
      _ <- eventDAO.update(updated)
    } yield updated
  }

  def signDigits(
      oracleEventTLV: OracleEventTLV,
      num: Long): Future[OracleEvent] = {

    val eventDescriptorTLV = oracleEventTLV.eventDescriptor match {
      case _: EnumEventDescriptorV0TLV | _: RangeEventDescriptorV0TLV =>
        throw new IllegalArgumentException(
          "Must have a DigitDecomposition event descriptor use signEvent instead")
      case decomp: DigitDecompositionEventDescriptorV0TLV =>
        decomp
    }

    // Make this a vec so it is easier to add on
    val signSigF =
      eventDescriptorTLV match {
        case _: SignedDigitDecompositionEventDescriptor =>
          val signOutcome = DigitDecompositionSignAttestation(num >= 0)
          signEvent(oracleEventTLV.nonces.head, signOutcome).map(db =>
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

    val nonces =
      if (eventDescriptorTLV.isSigned) oracleEventTLV.nonces.tail
      else oracleEventTLV.nonces

    val digitSigFs = nonces.zipWithIndex.map {
      case (nonce, index) =>
        val digit = decomposed(index)
        signEvent(nonce, DigitDecompositionAttestation(digit))
    }

    for {
      signSig <- signSigF
      digitSigs <- Future.sequence(digitSigFs)
    } yield OracleEvent.fromEventDbs(signSig ++ digitSigs)
  }
}

object DLCOracle {

  // 585 is a random one I picked, unclaimed in https://github.com/satoshilabs/slips/blob/master/slip-0044.md
  val R_VALUE_PURPOSE = 585

  def apply(
      mnemonicCode: MnemonicCode,
      passwordOpt: Option[AesPassword],
      bip39PasswordOpt: Option[String] = None)(implicit
      conf: DLCOracleAppConfig): DLCOracle = {
    val decryptedMnemonic = DecryptedMnemonic(mnemonicCode, TimeUtil.now)
    val toWrite = passwordOpt match {
      case Some(password) => decryptedMnemonic.encrypt(password)
      case None           => decryptedMnemonic
    }
    if (!conf.seedExists()) {
      WalletStorage.writeMnemonicToDisk(conf.seedPath, toWrite)
    }

    val key =
      WalletStorage.getPrivateKeyFromDisk(conf.seedPath,
                                          SegWitMainNetPriv,
                                          passwordOpt,
                                          bip39PasswordOpt)
    DLCOracle(key)
  }
}
