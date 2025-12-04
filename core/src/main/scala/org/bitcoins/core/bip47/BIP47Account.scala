package org.bitcoins.core.bip47

import org.bitcoins.core.config.{MainNet, NetworkParameters}
import org.bitcoins.core.crypto._
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyMainNetPriv
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto.ExtKeyPubVersion.LegacyMainNetPub
import org.bitcoins.core.crypto.ExtKeyPubVersion.LegacyTestNet3Pub
import org.bitcoins.core.number.{UInt32, UInt8}
import org.bitcoins.core.protocol.{Bech32Address, P2PKHAddress}
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey, ECPublicKey}
import scodec.bits.ByteVector

import scala.util.Try

/** Represents a BIP47 account at derivation path m/47'/coin_type'/account'.
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0047.mediawiki BIP47]]
  */
sealed abstract class BIP47Account {

  def extPrivKey: ExtPrivateKey

  def network: NetworkParameters

  def accountIndex: Int

  def paymentCode: PaymentCode = {
    val accountKey = extPrivKey
    PaymentCode(accountKey.key.publicKey, accountKey.chainCode)
  }

  def notificationKey: ExtPrivateKey = deriveKey(0)

  def notificationAddress: P2PKHAddress = {
    val pubKey = notificationKey.key.publicKey
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHAddress(hash, network)
  }

  def notificationSegwitAddress: Bech32Address = {
    val pubKey = notificationKey.key.publicKey
    val spk = P2WPKHWitnessSPKV0(pubKey)
    Bech32Address(spk, network)
  }

  def deriveKey(index: Int): ExtPrivateKey = {
    extPrivKey.deriveChildPrivKey(UInt32(index))
  }

  def privateKeyAt(index: Int): ECPrivateKey = deriveKey(index).key

  def publicKeyAt(index: Int): ECPublicKey = privateKeyAt(index).publicKey

  def addressAt(index: Int): Bech32Address = {
    val pubKey = publicKeyAt(index)
    val spk = P2WPKHWitnessSPKV0(pubKey)
    Bech32Address(spk, network)
  }

  /** Creates a PaymentAddress for sending to a remote party. */
  def paymentAddressForSending(
      remotePaymentCode: PaymentCode,
      index: Int): PaymentAddress = {
    PaymentAddress.forSending(
      localPrivKey = notificationKey.key,
      remotePaymentCode = remotePaymentCode,
      index = index,
      network = network
    )
  }

  /** Creates a PaymentAddress for receiving from a remote party. */
  def paymentAddressForReceiving(
      remotePaymentCode: PaymentCode,
      index: Int): PaymentAddress = {
    PaymentAddress.forReceiving(
      localPaymentCode = paymentCode,
      localPrivKeyAtIndex = privateKeyAt(index),
      remotePaymentCode = remotePaymentCode,
      index = index,
      network = network
    )
  }
}

object BIP47Account {
  val Purpose: Int = 47
  val HardenedOffset: Long = 0x80000000L

  private case class BIP47AccountImpl(
      extPrivKey: ExtPrivateKey,
      network: NetworkParameters,
      accountIndex: Int)
      extends BIP47Account

  def apply(
      extPrivKey: ExtPrivateKey,
      network: NetworkParameters,
      accountIndex: Int): BIP47Account = {
    BIP47AccountImpl(extPrivKey, network, accountIndex)
  }

  /** Creates a BIP47Account from a seed at the standard derivation path. */
  def fromSeed(
      seed: ByteVector,
      network: NetworkParameters,
      accountIndex: Int = 0): BIP47Account = {
    val version = network match {
      case _: MainNet => LegacyMainNetPriv
      case _          => LegacyTestNet3Priv
    }

    val master = ExtPrivateKey(version, Some(seed))

    val purposeKey =
      master.deriveChildPrivKey(UInt32(Purpose + HardenedOffset))
    val coinTypeKey =
      purposeKey.deriveChildPrivKey(UInt32(coinType(network) + HardenedOffset))
    val accountKey =
      coinTypeKey.deriveChildPrivKey(UInt32(accountIndex + HardenedOffset))

    BIP47AccountImpl(accountKey, network, accountIndex)
  }

  def fromExtendedKey(
      accountKey: ExtPrivateKey,
      network: NetworkParameters,
      accountIndex: Int): BIP47Account = {
    BIP47AccountImpl(accountKey, network, accountIndex)
  }

  def fromPaymentCode(
      paymentCode: PaymentCode,
      network: NetworkParameters): WatchOnlyBIP47Account = {
    WatchOnlyBIP47Account(paymentCode, network)
  }

  private def coinType(network: NetworkParameters): Long = network match {
    case _: MainNet => 0L
    case _          => 1L
  }
}

/** Watch-only BIP47 account created from a payment code (no private keys). */
sealed abstract class WatchOnlyBIP47Account {

  def paymentCode: PaymentCode

  def network: NetworkParameters

  def publicKeyAt(index: Int): ECPublicKey = {
    val masterKey = ExtPublicKey(
      extKeyVersion(network),
      UInt8.zero,
      ByteVector.fill(4)(0),
      UInt32.zero,
      paymentCode.chainCode,
      paymentCode.pubKey
    )
    masterKey.deriveChildPubKey(index).get.key
  }

  def addressAt(index: Int): Bech32Address = {
    val pubKey = publicKeyAt(index)
    val spk = P2WPKHWitnessSPKV0(pubKey)
    Bech32Address(spk, network)
  }

  def notificationAddress: P2PKHAddress = {
    val pubKey = publicKeyAt(0)
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHAddress(hash, network)
  }

  private def extKeyVersion(network: NetworkParameters): ExtKeyPubVersion =
    network match {
      case _: MainNet => LegacyMainNetPub
      case _          => LegacyTestNet3Pub
    }
}

object WatchOnlyBIP47Account {

  private case class WatchOnlyBIP47AccountImpl(
      paymentCode: PaymentCode,
      network: NetworkParameters)
      extends WatchOnlyBIP47Account

  def apply(
      paymentCode: PaymentCode,
      network: NetworkParameters): WatchOnlyBIP47Account = {
    WatchOnlyBIP47AccountImpl(paymentCode, network)
  }

  def fromBase58(
      base58: String,
      network: NetworkParameters): Try[WatchOnlyBIP47Account] = {
    PaymentCode.fromBase58(base58).map { pc =>
      WatchOnlyBIP47AccountImpl(pc, network)
    }
  }
}
