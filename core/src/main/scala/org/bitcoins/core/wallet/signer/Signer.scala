package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.builder.TxBuilderError
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** The class used to represent a signing process for a specific [[org.bitcoins.core.protocol.script.ScriptPubKey]] type */
sealed abstract class Signer {

  /**
    * The method used to sign a bitcoin unspent transaction output
    * @param signers the [[Signer]] needed to sign the utxo
    * @param txSigComponent The TxSigComponent for the input being signed
    * @param hashType the signature hashing algorithm we should use to sign the utxo
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @return
    */
  def sign(
      signers: Seq[Sign],
      txSigComponent: TxSigComponent,
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    sign(signers,
         txSigComponent,
         hashType,
         isDummySignature,
         nestedSPKToSatisfy = None)
  }

  /**
    * The method used to sign a bitcoin unspent transaction output that is potentially nested
    * @param signers the [[Signer]] needed to sign the utxo
    * @param txSigComponent The TxSigComponent for the input being signed
    * @param hashType the signature hashing algorithm we should use to sign the utxo
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @param nestedSPKToSatisfy specifies the nested SPK for which a valid ScriptSignature should be produced
    * @return
    */
  def sign(
      signers: Seq[Sign],
      txSigComponent: TxSigComponent,
      hashType: HashType,
      isDummySignature: Boolean,
      nestedSPKToSatisfy: Option[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[TxSigComponent]

  def doSign(
      sigComponent: TxSigComponent,
      sign: ByteVector => Future[ECDigitalSignature],
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[ECDigitalSignature] = {
    if (isDummySignature) {
      Future.successful(DummyECDigitalSignature)
    } else {
      TransactionSignatureCreator.createSig(sigComponent, sign, hashType)
    }
  }
}

/** Represents all signers for the bitcoin protocol, we could add another network later like litecoin */
sealed abstract class BitcoinSigner extends Signer

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner extends BitcoinSigner {

  override def sign(
      signers: Seq[Sign],
      txSigComponent: TxSigComponent,
      hashType: HashType,
      isDummySignature: Boolean,
      nestedSPKToSatisfy: Option[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = nestedSPKToSatisfy match {
      case None               => txSigComponent.output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }
    if (signers.size != 1) {
      Future.fromTry(TxBuilderError.TooManySigners)
    } else {
      val sign: ByteVector => Future[ECDigitalSignature] =
        signers.head.signFunction
      val unsignedInput = txSigComponent.input
      val flags = Policy.standardFlags

      val signed: Future[TxSigComponent] = spk match {
        case _: P2PKScriptPubKey =>
          val signature =
            doSign(txSigComponent, sign, hashType, isDummySignature)
          signature.map { sig =>
            val p2pkScriptSig = P2PKScriptSignature(sig)
            val signedInput = TransactionInput(unsignedInput.previousOutput,
                                               p2pkScriptSig,
                                               unsignedInput.sequence)
            val signedInputs =
              txSigComponent.transaction.inputs
                .updated(txSigComponent.inputIndex.toInt, signedInput)
            val signedTx = txSigComponent.transaction match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,
                                signedInputs,
                                btx.outputs,
                                btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,
                                   signedInputs,
                                   wtx.outputs,
                                   wtx.lockTime,
                                   wtx.witness)
            }
            BaseTxSigComponent(signedTx,
                               txSigComponent.inputIndex,
                               txSigComponent.output,
                               flags)
          }
        case _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey |
            _: LockTimeScriptPubKey | _: P2SHScriptPubKey |
            _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
            _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
          Future.fromTry(TxBuilderError.WrongSigner)
      }
      signed
    }
  }
}

object P2PKSigner extends P2PKSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHSigner extends BitcoinSigner {

  override def sign(
      signers: Seq[Sign],
      txSigComponent: TxSigComponent,
      hashType: HashType,
      isDummySignature: Boolean,
      nestedSPKToSatisfy: Option[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = nestedSPKToSatisfy match {
      case None               => txSigComponent.output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }
    if (signers.size != 1) {
      Future.fromTry(TxBuilderError.TooManySigners)
    } else {
      val sign = signers.head.signFunction
      val pubKey = signers.head.publicKey
      val unsignedInput = txSigComponent.input
      val flags = Policy.standardFlags

      val signed: Future[TxSigComponent] = spk match {
        case p2pkh: P2PKHScriptPubKey =>
          if (p2pkh != P2PKHScriptPubKey(pubKey)) {
            Future.fromTry(TxBuilderError.WrongPublicKey)
          } else {
            val signature =
              doSign(txSigComponent, sign, hashType, isDummySignature)
            signature.map { sig =>
              val p2pkhScriptSig = P2PKHScriptSignature(sig, pubKey)
              val signedInput = TransactionInput(unsignedInput.previousOutput,
                                                 p2pkhScriptSig,
                                                 unsignedInput.sequence)
              val signedInputs =
                txSigComponent.transaction.inputs
                  .updated(txSigComponent.inputIndex.toInt, signedInput)
              val signedTx = txSigComponent.transaction match {
                case btx: BaseTransaction =>
                  BaseTransaction(btx.version,
                                  signedInputs,
                                  btx.outputs,
                                  btx.lockTime)
                case wtx: WitnessTransaction =>
                  WitnessTransaction(wtx.version,
                                     signedInputs,
                                     wtx.outputs,
                                     wtx.lockTime,
                                     wtx.witness)
              }
              BaseTxSigComponent(signedTx,
                                 txSigComponent.inputIndex,
                                 txSigComponent.output,
                                 flags)
            }
          }
        case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey |
            _: LockTimeScriptPubKey | _: P2SHScriptPubKey |
            _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
            _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
          Future.fromTry(TxBuilderError.WrongSigner)
      }
      signed
    }
  }
}

object P2PKHSigner extends P2PKHSigner

sealed abstract class MultiSigSigner extends BitcoinSigner {

  override def sign(
      signersWithPubKeys: Seq[Sign],
      txSigComponent: TxSigComponent,
      hashType: HashType,
      isDummySignature: Boolean,
      nestedSPKToSatisfy: Option[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = nestedSPKToSatisfy match {
      case None               => txSigComponent.output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }
    val signers = signersWithPubKeys.map(_.signFunction)
    val unsignedInput = txSigComponent.input

    val signed: Future[TxSigComponent] = spk match {
      case multiSigSPK: MultiSignatureScriptPubKey =>
        val requiredSigs = multiSigSPK.requiredSigs
        if (signers.size < requiredSigs) {
          Future.fromTry(TxBuilderError.WrongSigner)
        } else {
          val signaturesNested = 0
            .until(requiredSigs)
            .map(i =>
              doSign(txSigComponent, signers(i), hashType, isDummySignature))
          val signatures = Future.sequence(signaturesNested)
          signatures.map { sigs =>
            val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
            val signedInput = TransactionInput(unsignedInput.previousOutput,
                                               multiSigScriptSig,
                                               unsignedInput.sequence)
            val signedInputs =
              txSigComponent.transaction.inputs
                .updated(txSigComponent.inputIndex.toInt, signedInput)
            val signedTx = txSigComponent.transaction match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,
                                signedInputs,
                                btx.outputs,
                                btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,
                                   signedInputs,
                                   wtx.outputs,
                                   wtx.lockTime,
                                   wtx.witness)
            }
            BaseTxSigComponent(signedTx,
                               txSigComponent.inputIndex,
                               txSigComponent.output,
                               Policy.standardFlags)
          }
        }
      case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: P2SHScriptPubKey |
          _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
          _: LockTimeScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | _: UnassignedWitnessScriptPubKey |
          EmptyScriptPubKey =>
        Future.fromTry(TxBuilderError.WrongSigner)
    }
    signed
  }
}

object MultiSigSigner extends MultiSigSigner

sealed abstract class P2WPKHSigner extends BitcoinSigner {
  override def sign(
      signers: Seq[Sign],
      txSigComponent: TxSigComponent,
      hashType: HashType,
      isDummySignature: Boolean,
      nestedSPKToSatisfy: Option[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[TxSigComponent] =
    if (nestedSPKToSatisfy.isDefined) {
      Future.fromTry(TxBuilderError.NestedWitnessSPK)
    } else {
      txSigComponent.transaction match {
        case wtx: WitnessTransaction =>
          if (signers.size != 1) {
            Future.fromTry(TxBuilderError.TooManySigners)
          } else {

            val sign = signers.head.signFunction

            val pubKey = signers.head.publicKey

            val unsignedScriptWit = P2WPKHWitnessV0(pubKey)

            val unsignedTxWitness = TransactionWitness(
              wtx.witness.witnesses
                .updated(txSigComponent.inputIndex.toInt, unsignedScriptWit))

            val unsignedWtx = WitnessTransaction(wtx.version,
                                                 wtx.inputs,
                                                 wtx.outputs,
                                                 wtx.lockTime,
                                                 unsignedTxWitness)

            val witSPK = txSigComponent.output.scriptPubKey match {
              case p2wpkh: P2WPKHWitnessSPKV0 =>
                if (p2wpkh != P2WPKHWitnessSPKV0(pubKey)) {
                  Future.fromTry(TxBuilderError.WrongPublicKey)
                } else Future.successful(p2wpkh)
              case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                  _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey |
                  _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey |
                  _: CLTVScriptPubKey | _: CSVScriptPubKey |
                  _: WitnessCommitment | EmptyScriptPubKey |
                  _: UnassignedWitnessScriptPubKey =>
                Future.fromTry(TxBuilderError.NonWitnessSPK)
            }

            witSPK.flatMap { w =>
              val witOutput = TransactionOutput(txSigComponent.output.value, w)

              val wtxComp = WitnessTxSigComponentRaw(unsignedWtx,
                                                     txSigComponent.inputIndex,
                                                     witOutput,
                                                     Policy.standardFlags)

              val signature = doSign(wtxComp, sign, hashType, isDummySignature)

              signature.map { sig =>
                val scriptWitness = P2WPKHWitnessV0(pubKey, sig)
                val signedTxWitness =
                  wtx.witness.updated(txSigComponent.inputIndex.toInt,
                                      scriptWitness)
                val signedTx = WitnessTransaction(unsignedWtx.version,
                                                  unsignedWtx.inputs,
                                                  unsignedWtx.outputs,
                                                  unsignedWtx.lockTime,
                                                  signedTxWitness)
                WitnessTxSigComponentRaw(signedTx,
                                         txSigComponent.inputIndex,
                                         witOutput,
                                         Policy.standardFlags)
              }

            }

          }
        case btx: BaseTransaction =>
          val wtx = WitnessTransaction(btx.version,
                                       btx.inputs,
                                       btx.outputs,
                                       btx.lockTime,
                                       EmptyWitness)

          val fixedTxSigComponent = WitnessTxSigComponent(
            wtx,
            txSigComponent.inputIndex,
            txSigComponent.output,
            txSigComponent.flags)

          sign(signers, fixedTxSigComponent, hashType, isDummySignature)
      }
    }
}
object P2WPKHSigner extends P2WPKHSigner

sealed abstract class P2WSHSigner extends BitcoinSigner {
  override def sign(
      signers: Seq[Sign],
      txSigComponent: TxSigComponent,
      hashType: HashType,
      isDummySignature: Boolean,
      nestedSPKToSatisfy: Option[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    if (nestedSPKToSatisfy.isDefined) {
      Future.fromTry(TxBuilderError.NestedWitnessSPK)
    } else {
      val spk = txSigComponent.output.scriptPubKey
      val flags = Policy.standardFlags

      spk match {
        case _: P2WSHWitnessSPKV0 =>
          val wtx = txSigComponent.transaction match {
            case btx: BaseTransaction =>
              WitnessTransaction(btx.version,
                                 btx.inputs,
                                 btx.outputs,
                                 btx.lockTime,
                                 EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val redeemScriptF =
            wtx.witness.witnesses(txSigComponent.inputIndex.toInt) match {
              case x: P2WSHWitnessV0 => Future.successful(x.redeemScript)
              case _: P2WPKHWitnessV0 =>
                Future.fromTry(TxBuilderError.NoRedeemScript)
              case EmptyScriptWitness =>
                Future.fromTry(TxBuilderError.NoWitness)
            }
          val sigComponent =
            WitnessTxSigComponentRaw(wtx,
                                     txSigComponent.inputIndex,
                                     txSigComponent.output,
                                     flags)
          val signerF: Future[Signer] = redeemScriptF.flatMap {
            case _: P2PKScriptPubKey  => Future.successful(P2PKSigner)
            case _: P2PKHScriptPubKey => Future.successful(P2PKHSigner)
            case _: MultiSignatureScriptPubKey =>
              Future.successful(MultiSigSigner)
            case _: LockTimeScriptPubKey =>
              Future.successful(LockTimeSigner)
            case _: P2SHScriptPubKey =>
              Future.fromTry(TxBuilderError.NestedP2SHSPK)
            case _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 =>
              Future.fromTry(TxBuilderError.NestedWitnessSPK)
            case _: NonStandardScriptPubKey | _: WitnessCommitment |
                EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
              Future.fromTry(TxBuilderError.NoSigner)
          }
          val signedSigComponentF = signerF.flatMap { signer =>
            redeemScriptF.flatMap { redeemScript =>
              signer.sign(signers,
                          sigComponent,
                          hashType,
                          isDummySignature,
                          Some(redeemScript))
            }
          }

          val scriptWitF = signedSigComponentF.flatMap { signedSigComponent =>
            redeemScriptF.map { rs =>
              P2WSHWitnessV0(rs, signedSigComponent.scriptSignature)
            }
          }

          scriptWitF.map { scriptWit =>
            val signedWitness =
              wtx.witness.updated(txSigComponent.inputIndex.toInt, scriptWit)
            val signedWTx = WitnessTransaction(wtx.version,
                                               wtx.inputs,
                                               wtx.outputs,
                                               wtx.lockTime,
                                               signedWitness)
            WitnessTxSigComponentRaw(signedWTx,
                                     txSigComponent.inputIndex,
                                     txSigComponent.output,
                                     flags)
          }

        case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
            _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey |
            _: P2WPKHWitnessSPKV0 | _: LockTimeScriptPubKey =>
          Future.fromTry(TxBuilderError.WrongSigner)
        case _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
          Future.fromTry(TxBuilderError.NoSigner)
      }
    }
  }
}
object P2WSHSigner extends P2WSHSigner

sealed abstract class LockTimeSigner extends BitcoinSigner {
  override def sign(
      signers: Seq[Sign],
      txSigComponent: TxSigComponent,
      hashType: HashType,
      isDummySignature: Boolean,
      nestedSPKToSatisfy: Option[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = nestedSPKToSatisfy match {
      case None               => txSigComponent.output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }

    spk match {
      case lockSPK: LockTimeScriptPubKey =>
        val signerF = lockSPK.nestedScriptPubKey match {
          case _: P2PKScriptPubKey  => Future.successful(P2PKSigner)
          case _: P2PKHScriptPubKey => Future.successful(P2PKHSigner)
          case _: MultiSignatureScriptPubKey =>
            Future.successful(MultiSigSigner)
          case _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 |
              _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey |
              _: CLTVScriptPubKey | _: CSVScriptPubKey | _: WitnessCommitment |
              EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
            Future.fromTry(TxBuilderError.WrongSigner)
        }
        signerF.flatMap { signer =>
          signer.sign(signers,
                      txSigComponent,
                      hashType,
                      isDummySignature,
                      Some(lockSPK.nestedScriptPubKey))
        }
      case _: P2SHScriptPubKey => Future.fromTry(TxBuilderError.NestedP2SHSPK)
      case _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 =>
        Future.fromTry(TxBuilderError.NestedWitnessSPK)
      case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: MultiSignatureScriptPubKey =>
        Future.fromTry(TxBuilderError.WrongSigner)
      case _: NonStandardScriptPubKey | _: WitnessCommitment |
          EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
        Future.fromTry(TxBuilderError.NoSigner)
    }
  }
}
object LockTimeSigner extends LockTimeSigner
