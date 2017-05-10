package org.bitcoins.core.wallet

import org.bitcoins.core.crypto.{ECPrivateKey, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.gen.WitnessGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}

/**
  * Created by chris on 5/9/17.
  */
sealed trait EscrowTimeoutHelper extends BitcoinSLogger {

  /** Signs a [[org.bitcoins.core.protocol.transaction.WitnessTransaction]] with the given private key
    * This means the WitnessTransaction is partially signed, we need to send the transaction to
    * the server to be fully signed
    * */
  def clientSign(inputs: Seq[TransactionInput], outputs: Seq[TransactionOutput], inputIndex: UInt32, privKey: ECPrivateKey,
                 lock: EscrowTimeoutScriptPubKey, witnessScriptPubKey: WitnessScriptPubKey,lockedAmount: CurrencyUnit,
                 hashType: HashType): WitnessTxSigComponent = {
    val unsignedScriptWitness = ScriptWitness(Seq(lock.asmBytes))
    val txWitness = TransactionWitness(Seq(unsignedScriptWitness))
    val wtx = WitnessTransaction(TransactionConstants.validLockVersion,inputs,outputs,TransactionConstants.lockTime,txWitness)
    val unsignedWTxSigComponent = WitnessTxSigComponent(wtx,inputIndex,witnessScriptPubKey,
      Policy.standardScriptVerifyFlags,lockedAmount)
    val signature = WitnessGenerators.csvEscrowTimeoutGenSignature(privKey,lock,unsignedWTxSigComponent,hashType)
    val sigs = Seq(signature)
    val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
    val escrow = EscrowTimeoutScriptSignature.fromMultiSig(multiSigScriptSig)
    val minimal = BitcoinScriptUtil.minimalDummy(BitcoinScriptUtil.minimalIfOp(escrow.asm))
    val signedScriptSigPushOpsRemoved = BitcoinScriptUtil.filterPushOps(minimal).reverse
    val signedScriptWitness = ScriptWitness(lock.asmBytes +: (signedScriptSigPushOpsRemoved.map(_.bytes)))
    val signedWTxSigComponent = WTxSigComponentHelper.createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
    signedWTxSigComponent._2
  }


}

object EscrowTimeoutHelper extends EscrowTimeoutHelper
