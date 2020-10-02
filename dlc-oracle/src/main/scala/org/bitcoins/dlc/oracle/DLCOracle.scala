package org.bitcoins.dlc.oracle

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.commons.jsonmodels.dlc.SigningVersion._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.ExtKeyVersion.SegWitMainNetPriv
import org.bitcoins.core.crypto.{ExtPrivateKeyHardened, MnemonicCode}
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.keymanager.{DecryptedMnemonic, WalletStorage}

import scala.concurrent.{ExecutionContext, Future}

case class DLCOracle(private val extPrivateKey: ExtPrivateKeyHardened)(implicit
    val conf: DLCOracleAppConfig) {

  implicit val ec: ExecutionContext = conf.ec

  // what was previously used so we other wallets could find funds
  //  private val signingKeyHDAddress = {
//    val coin = HDCoin(HDPurposes.SegWit, HDCoinType.fromNetwork(conf.network))
//    val account = HDAccount(coin, 0)
//    val chain = HDChain(HDChainType.External, account)
//    HDAddress(chain, 0)
//  }

  // 585 is a random one I picked, unclaimed in https://github.com/satoshilabs/slips/blob/master/slip-0044.md
  private val PURPOSE = 585

  private val account: HDAccount = {
    val coin = HDCoin(HDPurpose(PURPOSE), HDCoinType.fromNetwork(conf.network))
    HDAccount(coin, 0)
  }

  /** The chain index used in the bip32 paths for generating R values */
  private val rValueChainIndex = 0

  private def signingKey: ECPrivateKey = {
    val accountIndex = account.index
    val coin = account.coin
    val purpose = coin.purpose
    val chain = 1
    require(chain != rValueChainIndex,
            "Cannot use same chain index as R values")

    val path = BIP32Path.fromString(
      s"m/${purpose.constant}'/${coin.coinType.toInt}'/$accountIndex'/$chain'/0'")

    // what was previously used so we other wallets could find funds
//    extPrivateKey
//      .deriveChildPrivKey(SegWitHDPath(signingKeyHDAddress))
//      .key

    extPrivateKey.deriveChildPrivKey(path).key
  }

  val publicKey: SchnorrPublicKey = signingKey.schnorrPublicKey

  def stakingAddress(network: BitcoinNetwork): Bech32Address =
    Bech32Address(P2WPKHWitnessSPKV0(signingKey.publicKey), network)

  protected[bitcoins] val rValueDAO: RValueDAO = RValueDAO()
  protected[bitcoins] val eventDAO: EventDAO = EventDAO()
  protected[bitcoins] val eventOutcomeDAO: EventOutcomeDAO = EventOutcomeDAO()

  private def getPath(keyIndex: Int): BIP32Path = {
    val accountIndex = account.index
    val coin = account.coin
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
    val hash =
      CryptoUtil.taggedSha256(
        priv.schnorrNonce.bytes ++ CryptoUtil.serializeForHash(label),
        signingVersion.nonceTag)
    val tweak = ECPrivateKey(hash.bytes)

    priv.add(tweak)
  }

  def listEventDbs(): Future[Vector[EventDb]] = eventDAO.findAll()

  def listPendingEventDbs(): Future[Vector[EventDb]] = eventDAO.getPendingEvents

  def listEvents(): Future[Vector[Event]] = {
    for {
      rValDbs <- rValueDAO.findAll()
      eventDbs <- eventDAO.findAll()
      outcomes <- eventOutcomeDAO.findAll()
    } yield {
      val rValDbsByNonce = rValDbs.groupBy(_.nonce)
      val outcomesByNonce = outcomes.groupBy(_.nonce)
      eventDbs.map(db =>
        Event(rValDbsByNonce(db.nonce).head, db, outcomesByNonce(db.nonce)))
    }
  }

  def getEvent(nonce: SchnorrNonce): Future[Option[Event]] = {
    for {
      rValDbOpt <- rValueDAO.read(nonce)
      eventDbOpt <- eventDAO.read(nonce)
      outcomes <- eventOutcomeDAO.findByNonce(nonce)
    } yield {
      (rValDbOpt, eventDbOpt) match {
        case (Some(rValDb), Some(eventDb)) =>
          Some(Event(rValDb, eventDb, outcomes))
        case (None, None) | (Some(_), None) | (None, Some(_)) =>
          None
      }
    }
  }

  def createNewEvent(
      eventName: String,
      maturationTime: Instant,
      outcomes: Vector[String]): Future[EventDb] = {
    for {
      indexOpt <- rValueDAO.maxKeyIndex
      index = indexOpt match {
        case Some(value) => value + 1
        case None        => 0
      }

      signingVersion = SigningVersion.latest

      path = getPath(index)
      nonce = getKValue(eventName, path, signingVersion).schnorrNonce

      hash = CryptoUtil.taggedSha256(
        nonce.bytes ++ CryptoUtil.serializeForHash(eventName),
        signingVersion.commitmentTag)
      commitmentSig = signingKey.schnorrSign(hash.bytes)

      rValueDb = RValueDbHelper(nonce = nonce,
                                eventName = eventName,
                                account = account,
                                chainType = rValueChainIndex,
                                keyIndex = index,
                                commitmentSignature = commitmentSig)

      eventDb = EventDb(nonce,
                        eventName,
                        outcomes.size,
                        signingVersion,
                        maturationTime,
                        None)
      eventOutcomeDbs = outcomes.map { outcome =>
        val hash = CryptoUtil.taggedSha256(outcome, signingVersion.outcomeTag)
        EventOutcomeDb(nonce, outcome, hash)
      }

      _ <- rValueDAO.create(rValueDb)
      eventDb <- eventDAO.create(eventDb)
      _ <- eventOutcomeDAO.createAll(eventOutcomeDbs)
    } yield eventDb
  }

  def signEvent(nonce: SchnorrNonce, outcome: String): Future[EventDb] = {
    for {
      rValDbOpt <- rValueDAO.read(nonce)
      rValDb <- rValDbOpt match {
        case Some(value) => Future.successful(value)
        case None =>
          Future.failed(
            new RuntimeException(
              s"Nonce not found from this oracle ${nonce.hex}"))
      }

      eventOpt <- eventDAO.read(nonce)
      eventDb <- eventOpt match {
        case Some(value) =>
          require(
            value.attestationOpt.isEmpty,
            s"Event already has been signed, attestation: ${value.attestationOpt.get}")
          Future.successful(value)
        case None =>
          Future.failed(
            new RuntimeException(
              s"No event saved with nonce ${nonce.hex} $outcome"))
      }

      eventOutcomeOpt <- eventOutcomeDAO.read((nonce, outcome))
      eventOutcomeDb <- eventOutcomeOpt match {
        case Some(value) => Future.successful(value)
        case None =>
          Future.failed(new RuntimeException(
            s"No event outcome saved with nonce and message ${nonce.hex} $outcome"))
      }

      sig = eventDb.signingVersion match {
        case Mock =>
          val kVal = getKValue(rValDb, Mock)
          require(kVal.schnorrNonce == rValDb.nonce,
                  "The nonce from derived seed did not match database")
          signingKey.schnorrSignWithNonce(eventOutcomeDb.hashedMessage.bytes,
                                          kVal)
      }

      updated = eventDb.copy(attestationOpt = Some(sig.sig))
      _ <- eventDAO.update(updated)
    } yield updated
  }
}

object DLCOracle {

  def apply(
      mnemonicCode: MnemonicCode,
      password: AesPassword,
      bip39PasswordOpt: Option[String] = None)(implicit
      conf: DLCOracleAppConfig): DLCOracle = {
    val decryptedMnemonic = DecryptedMnemonic(mnemonicCode, TimeUtil.now)
    val encrypted = decryptedMnemonic.encrypt(password)
    if (!conf.seedExists()) {
      WalletStorage.writeMnemonicToDisk(conf.seedPath, encrypted)
    }

    val key =
      WalletStorage.getPrivateKeyFromDisk(conf.seedPath,
                                          SegWitMainNetPriv,
                                          password,
                                          bip39PasswordOpt)
    DLCOracle(key)
  }
}
