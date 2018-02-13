package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.WTxSigComponentHelper
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

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner extends BitcoinSigner {

  /** Helper function to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]], this is slightly different
    * than the other sign variant because it only takes ONE PRIVATE KEY rather than a Seq[ECPrivateKey].
    * Only one private key is required to spend a p2pkh spk/
    */
  def sign(privKey: ECPrivateKey, output: TransactionOutput, unsignedTx: Transaction,
           inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    sign(Seq(privKey), output, unsignedTx, inputIndex, hashType)
  }

  override def sign(privKeys: Seq[ECPrivateKey], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    val spk = output.scriptPubKey
    if (privKeys.size != 1) {
      Right(TxBuilderError.TooManyKeys)
    } else if (!spk.isInstanceOf[P2PKScriptPubKey]) {
      Right(TxBuilderError.WrongSigner)
    } else {
      val privKey = privKeys.head
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
      //val inputIndex = UInt32(unsignedInputs.indexOf(unsignedInput))
      val txSigComponent = TxSigComponent(unsignedTx,inputIndex,spk, Policy.standardFlags)
      val signature = TransactionSignatureCreator.createSig(txSigComponent,privKey,hashType)
      val p2pkScriptSig = P2PKScriptSignature(signature)
      val signedInput = TransactionInput(unsignedInput.previousOutput,p2pkScriptSig,unsignedInput.sequence)
      val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt,signedInput)
      val signedTx = unsignedTx match {
        case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
          btx.outputs,btx.lockTime)
        case wtx: WitnessTransaction => WitnessTransaction(wtx.version, wtx.inputs.updated(inputIndex.toInt, signedInput),
          wtx.outputs,wtx.lockTime, wtx.witness)
      }
      Left(TxSigComponent(signedTx,inputIndex,spk,txSigComponent.flags))
    }
  }
}

object P2PKSigner extends P2PKSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHSigner extends BitcoinSigner {

  /** Helper function to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]], this is slightly different
    * than the other sign variant because it only takes ONE PRIVATE KEY rather than a Seq[ECPrivateKey].
    * Only one private key is required to spend a p2pkh spk/
    */
  def sign(privKey: ECPrivateKey, output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    sign(Seq(privKey), output, unsignedTx, inputIndex, hashType)
  }
  /** This sign function gives you full customizability of what version/locktime/sequence number are used on the tx */
  override def sign(privKeys: Seq[ECPrivateKey], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    val spk = output.scriptPubKey
    if (privKeys.size != 1) {
      Right(TxBuilderError.TooManyKeys)
    } else if (!spk.isInstanceOf[P2PKHScriptPubKey]) {
      Right(TxBuilderError.WrongSigner)
    } else {
      val privKey = privKeys.head
      val publicKey = privKey.publicKey
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
      val txSigComponent = TxSigComponent(unsignedTx,inputIndex,spk, Policy.standardFlags)
      val signature = TransactionSignatureCreator.createSig(txSigComponent,privKey,hashType)
      val signedInput = buildP2PKHInput(signature,publicKey,unsignedInput.previousOutput,unsignedInput.sequence)
      val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt,signedInput)
      val signedTx = unsignedTx match {
        case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
          btx.outputs,btx.lockTime)
        case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
          wtx.outputs,wtx.lockTime, wtx.witness)
      }
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

sealed abstract class MultiSigSigner extends BitcoinSigner {
  override def sign(privKeys: Seq[ECPrivateKey], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    val spk = output.scriptPubKey
    if (!spk.isInstanceOf[MultiSignatureScriptPubKey]) {
      Right(TxBuilderError.WrongSigner)
    } else {
      val multisig = spk.asInstanceOf[MultiSignatureScriptPubKey]
      val requiredSigs = multisig.requiredSigs
      val emptyDigitalSignatures = privKeys.map(_ => EmptyDigitalSignature)
      val emptyScriptSig = MultiSignatureScriptSignature(emptyDigitalSignatures)
      val oldInput = unsignedTx.inputs(inputIndex.toInt)
      val emptyInput = TransactionInput(oldInput.previousOutput,emptyScriptSig,oldInput.sequence)
      val unsignedSpendingTx = Transaction(unsignedTx.version,unsignedTx.inputs.updated(inputIndex.toInt,emptyInput), unsignedTx.outputs, unsignedTx.lockTime)
      val txSignatureComponent = TxSigComponent(unsignedSpendingTx,inputIndex,
        multisig,Policy.standardFlags)
      val sigs = (0 until requiredSigs).map(i => TransactionSignatureCreator.createSig(txSignatureComponent, privKeys(i), hashType))
      val signedScriptSig = MultiSignatureScriptSignature(sigs)
      val signedInput = TransactionInput(oldInput.previousOutput,signedScriptSig,oldInput.sequence)
      val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt,signedInput)
      val signedTx = unsignedTx match {
        case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
          btx.outputs,btx.lockTime)
        case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
          wtx.outputs,wtx.lockTime, wtx.witness)
      }
      Left(TxSigComponent(signedTx,inputIndex,multisig,Policy.standardFlags))
    }
  }
}

object MultiSigSigner extends MultiSigSigner

sealed abstract class P2WPKHSigner extends BitcoinSigner {
  override def sign(privKeys: Seq[ECPrivateKey], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError] = unsignedTx match {
    case wtx: WitnessTransaction =>
      if (!output.scriptPubKey.isInstanceOf[WitnessScriptPubKeyV0]) {
        Right(TxBuilderError.NonWitnessSPK)
      } else if (privKeys.size != 1) {
        Right(TxBuilderError.TooManyKeys)
      } else {
        val privKey = privKeys.head
        val unsignedScriptWit = ScriptWitness(EmptyDigitalSignature,privKey.publicKey)
        val unsignedTxWitness = TransactionWitness(wtx.witness.witnesses.updated(inputIndex.toInt, unsignedScriptWit))
        val unsignedWtx = WitnessTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime, unsignedTxWitness)
        val witSPK = output.scriptPubKey.asInstanceOf[WitnessScriptPubKeyV0]
        val wtxComp = WitnessTxSigComponentRaw(unsignedWtx,inputIndex,witSPK,Policy.standardFlags,output.value)
        val signature = TransactionSignatureCreator.createSig(wtxComp,privKeys.head,hashType)
        val scriptWitness = ScriptWitness(signature,privKey.publicKey)
        val signedTxWitness = TransactionWitness(unsignedWtx.witness.witnesses.updated(inputIndex.toInt,scriptWitness))
        val signedTx = WitnessTransaction(unsignedWtx.version, unsignedWtx.inputs, unsignedWtx.outputs,
          unsignedWtx.lockTime, signedTxWitness)
        Left(WitnessTxSigComponentRaw(signedTx,inputIndex,witSPK,Policy.standardFlags,output.value))
      }
    case btx: BaseTransaction =>
      //convert to WitnessTransaction
      val witnesses = 0.until(btx.inputs.size).map(_ => EmptyScriptWitness)
      val txWitness = TransactionWitness(witnesses)
      val wtx = WitnessTransaction(btx.version, btx.inputs, btx.outputs, btx.lockTime, txWitness)
      sign(privKeys,output,wtx,inputIndex,hashType)
  }
}

object P2WPKHSigner extends P2WPKHSigner