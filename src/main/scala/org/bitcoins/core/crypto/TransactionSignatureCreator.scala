package org.bitcoins.core.crypto

import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger

import scala.concurrent.{ ExecutionContext, Future }

/**
 * Created by chris on 7/21/16.
 */
sealed abstract class TransactionSignatureCreator {
  private val logger = BitcoinSLogger.logger
  /**
   * Creates a signature from a tx signature component
   *
   * @param txSignatureComponent contains the tx, inputIndex which specify which input we are creating a sig for
   * @param privateKey the private key which we are signing the hash with
   * @param hashType the procedure to use for hashing to transaction
   * @return
   */
  def createSig(txSignatureComponent: TxSigComponent, privateKey: ECPrivateKey, hashType: HashType): ECDigitalSignature = {
    val sign: Seq[Byte] => ECDigitalSignature = privateKey.sign(_: Seq[Byte])
    createSig(txSignatureComponent, sign, hashType)
  }

  /**
   * This is intended to be a low level hardware wallet API.
   * At a fundamental level, a hardware wallet expects a Seq[Byte] as input, and returns an [[ECDigitalSignature]]
   * if it is able to sign the Seq[Byte]'s correctly.
   * @param component - the information needed to sign the transaction
   * @param sign - the implementation of the hardware wallet protocol to sign the Seq[Byte] w/ the given public key
   * @param hashType - the hash type to be appended on the digital signature when the hardware wallet is done being signed
   * @return the digital signature returned by the hardware wallet
   */
  def createSig(component: TxSigComponent, sign: Seq[Byte] => ECDigitalSignature, hashType: HashType): ECDigitalSignature = {
    val hash = TransactionSignatureSerializer.hashForSignature(component, hashType)
    val signature = sign(hash.bytes)
    //append 1 byte hash type onto the end
    val sig = ECDigitalSignature(signature.bytes ++ Seq(hashType.byte))
    require(sig.isStrictEncoded, "We did not create a signature that is strictly encoded, got: " + sig)
    require(DERSignatureUtil.isLowS(sig), "Sig does not have a low s value")
    sig
  }

  /** This is the same as createSig above, except the 'sign' function returns a Future[ECDigitalSignature] */
  def createSig(component: TxSigComponent, sign: Seq[Byte] => Future[ECDigitalSignature],
                hashType: HashType)(implicit ec: ExecutionContext): Future[ECDigitalSignature] = {
    val hash = TransactionSignatureSerializer.hashForSignature(component, hashType)
    val signature = sign(hash.bytes)
    //append 1 byte hash type onto the end
    val sig = signature.map(s => ECDigitalSignature(s.bytes ++ Seq(hashType.byte)))
    sig.map { s =>
      require(s.isStrictEncoded, "We did not create a signature that is strictly encoded, got: " + sig)
      require(DERSignatureUtil.isLowS(s), "Sig does not have a low s value")
      s
    }
  }
}

object TransactionSignatureCreator extends TransactionSignatureCreator