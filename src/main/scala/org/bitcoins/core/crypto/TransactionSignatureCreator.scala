package org.bitcoins.core.crypto

import org.bitcoins.core.script.crypto.{SIGHASH_ALL, HashType}
import org.bitcoins.core.util.{BitcoinSUtil, BitcoinSLogger}

/**
  * Created by chris on 7/21/16.
  */
trait TransactionSignatureCreator extends BitcoinSLogger {

  /**
    * Creates a signature from a tx signature component
    *
    * @param txSignatureComponent contains the tx, inputIndex which specify which input we are creating a sig for
    * @param privateKey the private key which we are signing the hash with
    * @param hashType the procedure to use for hashing to transaction
    * @return
    */
  def createSig(txSignatureComponent: TransactionSignatureComponent, privateKey: ECPrivateKey, hashType: HashType): ECDigitalSignature = {
    val hash = TransactionSignatureSerializer.hashForSignature(txSignatureComponent, hashType)
    val signature = privateKey.sign(hash)
    //append 1 byte hash type onto the end
    val sig = ECDigitalSignature(signature.bytes ++ Seq(hashType.byte))
    logger.debug("TxSigCreator sig: " + sig)
    require(sig.isStrictEncoded, "We did not create a signature that is strictly encoded, got: " + sig)
    require(DERSignatureUtil.isLowS(sig), "Sig does not have a low s value")
    sig
  }
}

object TransactionSignatureCreator extends TransactionSignatureCreator