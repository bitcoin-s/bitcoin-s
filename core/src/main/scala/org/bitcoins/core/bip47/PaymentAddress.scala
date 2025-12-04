package org.bitcoins.core.bip47

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.crypto.ExtKeyPubVersion.LegacyMainNetPub
import org.bitcoins.core.number.{UInt32, UInt8}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.crypto.{
  CryptoParams,
  CryptoUtil,
  ECPrivateKey,
  ECPublicKey,
  FieldElement
}
import scodec.bits.ByteVector

import java.math.BigInteger

/** Represents a derived payment address for BIP47 transactions. Used for both
  * sending and receiving payments.
  */
sealed abstract class PaymentAddress {

  def paymentCode: PaymentCode

  def index: Int

  def network: NetworkParameters

  protected def localPrivKey: ECPrivateKey

  protected def remotePublicKey: ECPublicKey

  def secretPoint: SecretPoint = SecretPoint(localPrivKey, remotePublicKey)

  def sharedSecretHash: ByteVector =
    CryptoUtil.sha256(secretPoint.ecdhSecret).bytes

  def tweakFieldElement: FieldElement = {
    val s = new BigInteger(1, sharedSecretHash.toArray)
    val n = CryptoParams.getN
    if (s.compareTo(BigInteger.ONE) <= 0 || s.compareTo(n) >= 0) {
      throw new IllegalStateException("Secret point not on secp256k1 curve")
    }
    FieldElement(sharedSecretHash)
  }

  /** Derives the public key for sending to the remote party. */
  def sendPublicKey: ECPublicKey = {
    val tweak = tweakFieldElement
    CryptoUtil.pubKeyTweakAdd(remotePublicKey, tweak.toPrivateKey)
  }

  /** Derives the private key for receiving from the remote party. */
  def receivePrivateKey: ECPrivateKey = {
    val tweak = tweakFieldElement
    val result = localPrivKey.fieldElement.add(tweak)
    result.toPrivateKey
  }

  /** Generates a SegWit address for sending to the remote party. */
  def segwitAddressForSend: Bech32Address = {
    val spk = P2WPKHWitnessSPKV0(sendPublicKey)
    Bech32Address(spk, network)
  }

  /** Generates a SegWit address for receiving from the remote party. */
  def segwitAddressForReceive: Bech32Address = {
    val spk = P2WPKHWitnessSPKV0(receivePrivateKey.publicKey)
    Bech32Address(spk, network)
  }
}

object PaymentAddress {

  private case class PaymentAddressImpl(
      paymentCode: PaymentCode,
      index: Int,
      localPrivKey: ECPrivateKey,
      remotePublicKey: ECPublicKey,
      network: NetworkParameters)
      extends PaymentAddress

  /** Creates a PaymentAddress for sending to a remote party.
    * @param localPrivKey
    *   your private key at index 0
    * @param remotePaymentCode
    *   the recipient's payment code
    * @param index
    *   the address index
    * @param network
    *   the network parameters
    */
  def forSending(
      localPrivKey: ECPrivateKey,
      remotePaymentCode: PaymentCode,
      index: Int,
      network: NetworkParameters): PaymentAddress = {
    val remoteKey = derivePublicKeyAtIndex(remotePaymentCode, index)
    PaymentAddressImpl(remotePaymentCode,
                       index,
                       localPrivKey,
                       remoteKey,
                       network)
  }

  /** Creates a PaymentAddress for receiving from a remote party.
    * @param localPaymentCode
    *   your payment code
    * @param localPrivKeyAtIndex
    *   your private key at the specified index
    * @param remotePaymentCode
    *   the sender's payment code
    * @param index
    *   the address index
    * @param network
    *   the network parameters
    */
  def forReceiving(
      localPaymentCode: PaymentCode,
      localPrivKeyAtIndex: ECPrivateKey,
      remotePaymentCode: PaymentCode,
      index: Int,
      network: NetworkParameters): PaymentAddress = {
    val remoteKey = derivePublicKeyAtIndex(remotePaymentCode, index)
    PaymentAddressImpl(localPaymentCode,
                       index,
                       localPrivKeyAtIndex,
                       remoteKey,
                       network)
  }

  private def derivePublicKeyAtIndex(
      paymentCode: PaymentCode,
      index: Int): ECPublicKey = {
    val masterKey = ExtPublicKey(
      LegacyMainNetPub,
      UInt8.zero,
      ByteVector.fill(4)(0),
      UInt32.zero,
      paymentCode.chainCode,
      paymentCode.pubKey
    )
    masterKey.deriveChildPubKey(index).get.key
  }
}
