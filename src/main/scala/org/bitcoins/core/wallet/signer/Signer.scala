package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.P2PKHScriptSignature
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint, TransactionOutput}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.builder.TxBuilderError

/** The class used to represent a signing process for a specific [[org.bitcoins.core.protocol.script.ScriptPubKey]] type */
sealed abstract class Signer {
  /**
    * The method used to sign a bitcoin unspent transaction output
    * @param privKey the private keys needed to sign the utxo
    * @param output the utxo we are spending
    * @param unsignedTx the unsigned transaction which is spending the utxo
    * @param inputIndex the input index inside of the unsigned transaction which spends the utxo
    * @param hashType the signature hashing algorithm we should use to sign the utxo
    * @return
    */
  def sign(privKey: Seq[ECPrivateKey], output: TransactionOutput, unsignedTx: Transaction, inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError]
}


sealed abstract class BitcoinSigner extends Signer

sealed abstract class P2PKHSigner extends BitcoinSigner {

  /** This sign function gives you full customizability of what version/locktime/sequence number are used on the tx */
  override def sign(privKeys: Seq[ECPrivateKey], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    if (privKeys.size != 1) {
      Right(TxBuilderError.TooManyKeys)
    } else {
      val privKey = privKeys.head
      val spk = output.scriptPubKey
      val publicKey = privKey.publicKey
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
      //val inputIndex = UInt32(unsignedInputs.indexOf(unsignedInput))
      val txSigComponent = TxSigComponent(unsignedTx,inputIndex,spk, Policy.standardFlags)
      val signature = TransactionSignatureCreator.createSig(txSigComponent,privKey,hashType)

      val signedInput = buildP2PKHInput(signature,publicKey,unsignedInput.previousOutput,unsignedInput.sequence)
      val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt,signedInput)
      val signedTx = Transaction(unsignedTx.version,signedInputs, unsignedTx.outputs, unsignedTx.lockTime)
      Left(TxSigComponent(signedTx,inputIndex,spk,txSigComponent.flags))
    }
  }

  private def buildP2PKHInput(signature: ECDigitalSignature, publicKey: ECPublicKey,
                              outPoint: TransactionOutPoint, sequence: UInt32): TransactionInput = {
    val scriptSig = P2PKHScriptSignature(signature,publicKey)
    TransactionInput(outPoint,scriptSig,sequence)
  }
}

object P2PKHSigner extends P2PKHSigner