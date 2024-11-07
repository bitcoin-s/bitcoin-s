package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.utxo.{InputInfo, InputSigningInfo}
import org.bitcoins.crypto.{
  DERSignatureUtil,
  DigitalSignature,
  ECAdaptorSignature,
  ECDigitalSignature,
  ECPrivateKey,
  HashType
}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** Created by chris on 7/21/16.
  */
sealed abstract class TransactionSignatureCreator {

  /** Creates a signature from a tx signature component
    *
    * @param txSignatureComponent
    *   contains the tx, inputIndex which specify which input we are creating a
    *   sig for
    * @param privateKey
    *   the private key which we are signing the hash with
    * @param hashType
    *   the procedure to use for hashing to transaction
    * @return
    */
  def createSig(
      txSignatureComponent: TxSigComponent,
      privateKey: ECPrivateKey,
      hashType: HashType): ECDigitalSignature = {
    createSig(txSignatureComponent, privateKey.signWithHashType, hashType)
  }

  /** This is intended to be a low level hardware wallet API. At a fundamental
    * level, a hardware wallet expects a scodec.bits.ByteVector as input, and
    * returns an [[ECDigitalSignature]] if it is able to sign the
    * scodec.bits.ByteVector's correctly.
    * @param component
    *   \- the information needed to sign the transaction
    * @param sign
    *   \- the implementation of the hardware wallet protocol to sign the
    *   scodec.bits.ByteVector w/ the given public key
    * @param hashType
    *   \- the hash type to be appended on the digital signature when the
    *   hardware wallet is done being signed
    * @return
    *   the digital signature returned by the hardware wallet
    */
  def createSig[S <: DigitalSignature](
      component: TxSigComponent,
      sign: (ByteVector, HashType) => S,
      hashType: HashType): S = {
    val hash =
      TransactionSignatureSerializer.hashForSignature(
        component,
        hashType,
        taprootOptions = TaprootSerializationOptions.empty)
    val signature = sign(hash.bytes, hashType)
    signature
  }

  /** This is the same as createSig above, except the 'sign' function returns a
    * Future[ECDigitalSignature]
    */
  @deprecated("use an InputSigningInfo[InputInfo] instead", since = "6/23/2020")
  def createSig(
      component: TxSigComponent,
      sign: ByteVector => Future[ECDigitalSignature],
      hashType: HashType)(implicit
      ec: ExecutionContext): Future[ECDigitalSignature] = {
    val hash =
      TransactionSignatureSerializer.hashForSignature(
        component,
        hashType,
        taprootOptions = TaprootSerializationOptions.empty)
    val signature = sign(hash.bytes)
    // append 1 byte hash type onto the end
    val sig = signature.map(_.appendHashType(hashType))
    sig.map { s =>
      require(
        s.isStrictEncoded,
        "We did not create a signature that is strictly encoded, got: " + sig)
      require(DERSignatureUtil.isLowS(s), "Sig does not have a low s value")
      s
    }
  }

  /** Creates a signature from a tx signature component
    *
    * @param privateKey
    *   the private key which we are signing the hash with
    * @param hashType
    *   the procedure to use for hashing to transaction
    * @return
    */
  def createSig(
      spendingTransaction: Transaction,
      signingInfo: InputSigningInfo[InputInfo],
      privateKey: ECPrivateKey,
      hashType: HashType): ECDigitalSignature = {
    createSig(spendingTransaction,
              signingInfo,
              privateKey.signWithHashType,
              hashType)
  }

  /** This is intended to be a low level hardware wallet API. At a fundamental
    * level, a hardware wallet expects a scodec.bits.ByteVector as input, and
    * returns an [[ECDigitalSignature]] if it is able to sign the
    * scodec.bits.ByteVector's correctly.
    * @param sign
    *   \- the implementation of the hardware wallet protocol to sign the
    *   scodec.bits.ByteVector w/ the given public key
    * @param hashType
    *   \- the hash type to be appended on the digital signature when the
    *   hardware wallet is done being signed
    * @return
    *   the digital signature returned by the hardware wallet
    */
  def createSig(
      spendingTransaction: Transaction,
      signingInfo: InputSigningInfo[InputInfo],
      sign: (ByteVector, HashType) => ECDigitalSignature,
      hashType: HashType): ECDigitalSignature = {
    val hash = TransactionSignatureSerializer.hashForSignature(
      spendingTransaction = spendingTransaction,
      signingInfo = signingInfo,
      hashType = hashType,
      taprootOptions = TaprootSerializationOptions.empty)

    val signature = sign(hash.bytes, hashType)
    signature
  }

  /** This is the same as createSig above, except the 'sign' function returns a
    * Future[ECDigitalSignature]
    */
  def createSig(
      spendingTransaction: Transaction,
      signingInfo: InputSigningInfo[InputInfo],
      sign: (ByteVector, HashType) => Future[ECDigitalSignature],
      hashType: HashType): Future[ECDigitalSignature] = {
    val hash =
      TransactionSignatureSerializer.hashForSignature(
        spendingTransaction,
        signingInfo,
        hashType,
        taprootOptions = TaprootSerializationOptions.empty)
    val signatureF = sign(hash.bytes, hashType)
    signatureF
  }

  def createSig(
      component: TxSigComponent,
      adaptorSign: ByteVector => ECAdaptorSignature,
      hashType: HashType): ECAdaptorSignature = {
    val hash =
      TransactionSignatureSerializer.hashForSignature(
        component,
        hashType,
        taprootOptions = TaprootSerializationOptions.empty)
    adaptorSign(hash.bytes)
  }
}

object TransactionSignatureCreator extends TransactionSignatureCreator
