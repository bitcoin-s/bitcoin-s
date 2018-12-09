package org.bitcoins.core.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.builder.TxBuilderError

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 5/9/17.
  */
sealed abstract class EscrowTimeoutHelper {

  /**
    * Signs a [[org.bitcoins.core.protocol.transaction.WitnessTransaction]] with the given private key
    * This will result in a Transaction that is partially signed, we need to send the transaction to
    * the server to be fully signed
    */
  def clientSign(
      outPoint: TransactionOutPoint,
      creditingTx: Transaction,
      destinations: Seq[TransactionOutput],
      signer: Sign,
      lock: EscrowTimeoutScriptPubKey,
      hashType: HashType)(
      implicit ec: ExecutionContext): Future[WitnessTxSigComponentRaw] = {
    val creditingOutput = creditingTx.outputs(outPoint.vout.toInt)
    if (creditingOutput.scriptPubKey != P2WSHWitnessSPKV0(lock)) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val (_, amount) =
        (creditingOutput.scriptPubKey.asInstanceOf[P2WSHWitnessSPKV0],
         creditingOutput.value)
      val tc = TransactionConstants
      val uScriptWitness = P2WSHWitnessV0(lock)
      val inputs = Seq(
        TransactionInput(outPoint, EmptyScriptSignature, tc.sequence))
      val wtx = WitnessTransaction(tc.validLockVersion,
                                   inputs,
                                   destinations,
                                   tc.lockTime,
                                   TransactionWitness(Vector(uScriptWitness)))
      val witSPK = P2WSHWitnessSPKV0(lock)
      val witOutput = TransactionOutput(amount, witSPK)
      val u = WitnessTxSigComponentRaw(wtx,
                                       UInt32.zero,
                                       witOutput,
                                       Policy.standardFlags)
      val sign = signer.signFunction
      val signature = TransactionSignatureCreator.createSig(u, sign, hashType)
      signature.map { sig =>
        val multiSigScriptSig = MultiSignatureScriptSignature(Seq(sig))
        val escrow =
          EscrowTimeoutScriptSignature.fromMultiSig(multiSigScriptSig)
        val witness = buildEscrowTimeoutScriptWitness(escrow, lock, u)
        val signedWTx = WitnessTransaction(wtx.version,
                                           wtx.inputs,
                                           wtx.outputs,
                                           wtx.lockTime,
                                           witness)
        WitnessTxSigComponentRaw(signedWTx,
                                 u.inputIndex,
                                 creditingOutput,
                                 u.flags)
      }

    }
  }

  /**
    * Helper function to build a [[org.bitcoins.core.protocol.transaction.TransactionWitness]]
    * for an [[EscrowTimeoutScriptPubKey]]
    */
  def buildEscrowTimeoutScriptWitness(
      signedScriptSig: EscrowTimeoutScriptSignature,
      lock: EscrowTimeoutScriptPubKey,
      unsigned: WitnessTxSigComponentRaw): TransactionWitness = {
    //need to remove the OP_0 or OP_1 and replace it with ScriptNumber.zero / ScriptNumber.one since witnesses are *not* run through the interpreter
    val signedScriptWitness = P2WSHWitnessV0(lock, signedScriptSig)
    val txWitness: TransactionWitness = unsigned.transaction.witness
      .updated(unsigned.inputIndex.toInt, signedScriptWitness)
    txWitness
  }

  /** Signs the partially signed [[org.bitcoins.core.crypto.WitnessTxSigComponent]] with the server's private key */
  def serverSign(
      privKey: ECPrivateKey,
      clientSigned: WitnessTxSigComponentRaw,
      hashType: HashType)(
      implicit ec: ExecutionContext): Future[WitnessTxSigComponentRaw] = {
    val oldTx = clientSigned.transaction
    val lock: Future[EscrowTimeoutScriptPubKey] = clientSigned.witness match {
      case _: P2WPKHWitnessV0 | EmptyScriptWitness =>
        Future.fromTry(TxBuilderError.NoRedeemScript)
      case p2wsh: P2WSHWitnessV0 =>
        p2wsh.redeemScript match {
          case lock: EscrowTimeoutScriptPubKey => Future.successful(lock)
          case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
              _: MultiSignatureScriptPubKey | _: NonStandardScriptPubKey |
              _: P2SHScriptPubKey | _: P2WSHWitnessSPKV0 |
              _: P2WPKHWitnessSPKV0 | _: CLTVScriptPubKey | _: CSVScriptPubKey |
              EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey |
              _: WitnessCommitment =>
            Future.fromTry(TxBuilderError.WrongRedeemScript)
        }
    }
    val witSPK = lock.map(P2WSHWitnessSPKV0(_))
    val stack = clientSigned.witness.stack
    val clientSignature = ECDigitalSignature(stack(stack.size - 2))
    val raw = witSPK.map { w =>
      val output = TransactionOutput(clientSigned.amount, w)

      WitnessTxSigComponentRaw(clientSigned.transaction,
                               clientSigned.inputIndex,
                               output,
                               clientSigned.flags)
    }
    val signature =
      raw.map(r => TransactionSignatureCreator.createSig(r, privKey, hashType))
    val escrow = signature.map { s =>
      val sigs = Seq(clientSignature, s)
      val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
      EscrowTimeoutScriptSignature.fromMultiSig(multiSigScriptSig)
    }

    val witness = lock.flatMap { l =>
      raw.flatMap { r =>
        escrow.map { e =>
          buildEscrowTimeoutScriptWitness(e, l, r)
        }
      }
    }
    witness.flatMap { w =>
      witSPK.map { spk =>
        val output = TransactionOutput(clientSigned.amount, spk)
        val wtx = WitnessTransaction(oldTx.version,
                                     oldTx.inputs,
                                     oldTx.outputs,
                                     oldTx.lockTime,
                                     w)
        WitnessTxSigComponentRaw(wtx,
                                 clientSigned.inputIndex,
                                 output,
                                 clientSigned.flags)
      }
    }
  }

  /**
    * Closes the given [[EscrowTimeoutScriptPubKey]] with it's timeout branch.
    * Assumes we are spending the given [[TransactionOutPoint]]
    *
    * It is important to note that we assume the nestedScriptPubKey inside of the EscrowTimeoutScriptPubKey
    * is a [[P2PKHScriptPubKey]] that can be spent by the given [[ECPrivateKey]]
    */
  def closeWithTimeout(
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput],
      inputIndex: UInt32,
      privKey: ECPrivateKey,
      lock: EscrowTimeoutScriptPubKey,
      creditingOutput: TransactionOutput,
      hashType: HashType): Future[WitnessTxSigComponentRaw] =
    lock.timeout.nestedScriptPubKey match {
      case _: P2PKHScriptPubKey =>
        //for now we require the nested spk is p2pkh, this is an arbitrary limitation for now to make this simple
        val witSPK = P2WSHWitnessSPKV0(lock)
        val amount = creditingOutput.value
        val tc = TransactionConstants
        val uScriptWitness = P2WSHWitnessV0(lock)
        val uTxWitness = TransactionWitness(Vector(uScriptWitness))
        val uwtx = WitnessTransaction(tc.validLockVersion,
                                      inputs,
                                      outputs,
                                      tc.lockTime,
                                      uTxWitness)
        val witOutput = TransactionOutput(amount, witSPK)
        val u = WitnessTxSigComponentRaw(uwtx,
                                         inputIndex,
                                         witOutput,
                                         Policy.standardFlags)
        val signature =
          TransactionSignatureCreator.createSig(u, privKey, hashType)
        val scriptSig = CSVScriptSignature(
          P2PKHScriptSignature(signature, privKey.publicKey))
        val escrowScriptSig: Try[EscrowTimeoutScriptSignature] =
          EscrowTimeoutScriptSignature(scriptSig)
        escrowScriptSig match {
          case Success(e) =>
            val scriptWitness = P2WSHWitnessV0(lock, e)
            val witness = TransactionWitness(
              u.transaction.witness.witnesses
                .updated(u.inputIndex.toInt, scriptWitness))
            val wtx = WitnessTransaction(uwtx.version,
                                         uwtx.inputs,
                                         uwtx.outputs,
                                         uwtx.lockTime,
                                         witness)
            Future.successful(
              WitnessTxSigComponentRaw(wtx, u.inputIndex, witOutput, u.flags))
          case Failure(_) => Future.fromTry(TxBuilderError.UnknownError)
        }
      case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey |
          _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
          _: NonStandardScriptPubKey | _: EscrowTimeoutScriptPubKey |
          _: CSVScriptPubKey | _: CLTVScriptPubKey | _: WitnessCommitment |
          _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey =>
        Future.fromTry(TxBuilderError.WrongRedeemScript)
    }
}

object EscrowTimeoutHelper extends EscrowTimeoutHelper
