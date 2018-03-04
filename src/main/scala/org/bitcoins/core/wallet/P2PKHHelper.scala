package org.bitcoins.core.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, P2PKHScriptSignature, ScriptPubKey}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType

/**
  * Created by chris on 6/12/17.
  */
sealed trait P2PKHHelper {

  /** This sign function generates a [[P2PKHScriptSignature]] for the given [[ScriptPubKey]]
    * This function guesses the defaults for version/locktime/sequence
    */
  def sign(privKey: ECPrivateKey, spk: ScriptPubKey, outPoint: TransactionOutPoint,
           outputs: Seq[TransactionOutput], hashType: HashType): BaseTxSigComponent = {
    sign(privKey,spk,outPoint,outputs,hashType,TransactionConstants.version,TransactionConstants.sequence,
      TransactionConstants.lockTime)
  }

  /** This sign function gives you full customizability of what version/locktime/sequence number are used on the tx */
  def sign(privKey: ECPrivateKey, spk: ScriptPubKey, outPoint: TransactionOutPoint,
           outputs: Seq[TransactionOutput], hashType: HashType,
           version: UInt32, sequence: UInt32, lockTime: UInt32): BaseTxSigComponent = {
    val publicKey = privKey.publicKey
    val unsignedInput = buildP2PKHInput(EmptyDigitalSignature,publicKey,outPoint,sequence)
    val unsignedInputs = Seq(unsignedInput)
    val unsignedTx = BaseTransaction(version,unsignedInputs,outputs,lockTime)
    val inputIndex = UInt32(unsignedInputs.indexOf(unsignedInput))
    val baseTxSigComponent = BaseTxSigComponent(unsignedTx,inputIndex,spk,
      Policy.standardScriptVerifyFlags)
    val signature = TransactionSignatureCreator.createSig(baseTxSigComponent,privKey,hashType)

    val signedInput = buildP2PKHInput(signature,publicKey,outPoint,sequence)
    val signedInputs = Seq(signedInput)
    val signedTx = BaseTransaction(unsignedTx.version,signedInputs, unsignedTx.outputs, unsignedTx.lockTime)
    BaseTxSigComponent(signedTx,inputIndex,spk,baseTxSigComponent.flags)
  }

  private def buildP2PKHInput(signature: ECDigitalSignature, publicKey: ECPublicKey,
                              outPoint: TransactionOutPoint, sequence: UInt32): TransactionInput = {
    val scriptSig = P2PKHScriptSignature(signature,publicKey)
    TransactionInput(outPoint,scriptSig,sequence)
  }
}

object P2PKHHelper extends P2PKHHelper
