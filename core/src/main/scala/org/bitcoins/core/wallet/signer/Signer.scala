package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
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
    * @param output the utxo we are spending
    * @param unsignedTx the unsigned transaction which is spending the utxo
    * @param inputIndex the input index inside of the unsigned transaction which spends the utxo
    * @param hashType the signature hashing algorithm we should use to sign the utxo
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @return
    */
  def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    sign(signers,
         output,
         unsignedTx,
         inputIndex,
         hashType,
         isDummySignature,
         overrides = NestedSigning.NoOverrides)
  }

  /**
    * The method used to sign a bitcoin unspent transaction output that is potentially nested
    * @param signers the [[Signer]] needed to sign the utxo
    * @param output the utxo we are spending
    * @param unsignedTx the unsigned transaction which is spending the utxo
    * @param inputIndex the input index inside of the unsigned transaction which spends the utxo
    * @param hashType the signature hashing algorithm we should use to sign the utxo
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @param overrides specifies things that should be overriden when signing such as TxSigComponent if this is nested
    * @return
    */
  def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
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

/** For use when signing nested ScriptPubKeys.
  *
  * This will require the hash to be signed to come from some external
  * (to the Signer being used) TxSigComponent (think P2SH, P2WSH).
  *
  * This will also require that the ScriptPubKey that the nested Signer
  * looks at needs to be overriden (not output.scriptPubKey)
  */
sealed abstract class OverridesForNestedSigning {

  /** Depending on the external [[ScriptPubKey scriptpubkey]] type, there
    * are different rules for which script is meant to be signed.
    *
    * For instance, if you are spending a [[P2PKHScriptPubKey p2pkh]]
    * you sign the p2pkh script itself.
    *
    * If you are signing a [[P2WPKHWitnessSPKV0 p2wpkh spk]], you sign the
    * re-constructed [[P2PKHScriptPubKey p2pkh]] that is associated with p2wpkh script.
    *
    * If you are signing a [[LockTimeScriptPubKey locktime spk]], you must sign
    * the entire [[LockTimeScriptPubKey]], not the nested script.
    */
  def sigComponentToSignOpt: Option[TxSigComponent]

  /** This represents the [[ScriptPubKey scriptpubkey]] for which a valid ScriptSignature is
    * being generated. This is usually some nested SPK such as a redeem script in P2SH and P2WSH.
    */
  def scriptPubKeyToSatisfyOpt: Option[ScriptPubKey]
}

object NestedSigning {

  /** The NoOverrides case where a Signer is used raw, without nesting */
  case object NoOverrides extends OverridesForNestedSigning {
    override val sigComponentToSignOpt: Option[TxSigComponent] = None
    override val scriptPubKeyToSatisfyOpt: Option[ScriptPubKey] = None
  }

  /** For P2WSH signing, this will be passed to the nested Signer */
  case class P2WSHOverrides(
      externalSigComponent: TxSigComponent,
      redeemScript: ScriptPubKey)
      extends OverridesForNestedSigning {
    override val sigComponentToSignOpt: Option[TxSigComponent] = Some(
      externalSigComponent)
    override val scriptPubKeyToSatisfyOpt: Option[ScriptPubKey] = Some(
      redeemScript)
  }

  /** For Locktime signing, this will be passed to the nested Signer.
    *
    * @param nestedSPK This should be [[LockTimeScriptPubKey.nestedScriptPubKey]]
    */
  case class LockTimeOverrides(nestedSPK: ScriptPubKey)
      extends OverridesForNestedSigning {
    override val sigComponentToSignOpt: Option[TxSigComponent] = None
    override val scriptPubKeyToSatisfyOpt: Option[ScriptPubKey] = Some(
      nestedSPK)
  }

  /** For signing when a LockTime script is nested, such as within P2WSH.
    *
    * @param nestedSPK This should be [[LockTimeScriptPubKey.nestedScriptPubKey]]
    */
  case class NestedLockTimeOverrides(
      externalSigComponent: TxSigComponent,
      nestedSPK: ScriptPubKey)
      extends OverridesForNestedSigning {
    override val sigComponentToSignOpt: Option[TxSigComponent] = Some(
      externalSigComponent)
    override val scriptPubKeyToSatisfyOpt: Option[ScriptPubKey] = Some(
      nestedSPK)
  }
}

/** Represents all signers for the bitcoin protocol, we could add another network later like litecoin */
sealed abstract class BitcoinSigner extends Signer

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner extends BitcoinSigner {

  override def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = overrides.scriptPubKeyToSatisfyOpt match {
      case None               => output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }
    if (signers.size != 1) {
      Future.fromTry(TxBuilderError.TooManySigners)
    } else {
      val sign: ByteVector => Future[ECDigitalSignature] =
        signers.head.signFunction
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
      val flags = Policy.standardFlags

      val signed: Future[TxSigComponent] = spk match {
        case _: P2PKScriptPubKey =>
          val sigComponent = overrides.sigComponentToSignOpt match {
            case None =>
              BaseTxSigComponent(unsignedTx, inputIndex, output, flags)
            case Some(sigComponent) => sigComponent
          }
          val signature = doSign(sigComponent, sign, hashType, isDummySignature)
          signature.map { sig =>
            val p2pkScriptSig = P2PKScriptSignature(sig)
            val signedInput = TransactionInput(unsignedInput.previousOutput,
                                               p2pkScriptSig,
                                               unsignedInput.sequence)
            val signedInputs =
              unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
            val signedTx = unsignedTx match {
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
            BaseTxSigComponent(signedTx, inputIndex, output, flags)
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
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = overrides.scriptPubKeyToSatisfyOpt match {
      case None               => output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }
    if (signers.size != 1) {
      Future.fromTry(TxBuilderError.TooManySigners)
    } else {
      val sign = signers.head.signFunction
      val pubKey = signers.head.publicKey
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
      val flags = Policy.standardFlags

      val signed: Future[TxSigComponent] = spk match {
        case p2pkh: P2PKHScriptPubKey =>
          if (p2pkh != P2PKHScriptPubKey(pubKey)) {
            Future.fromTry(TxBuilderError.WrongPublicKey)
          } else {
            val sigComponent = overrides.sigComponentToSignOpt match {
              case None =>
                BaseTxSigComponent(unsignedTx, inputIndex, output, flags)
              case Some(sigComponent) => sigComponent
            }
            val signature =
              doSign(sigComponent, sign, hashType, isDummySignature)
            signature.map { sig =>
              val p2pkhScriptSig = P2PKHScriptSignature(sig, pubKey)
              val signedInput = TransactionInput(unsignedInput.previousOutput,
                                                 p2pkhScriptSig,
                                                 unsignedInput.sequence)
              val signedInputs =
                unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
              val signedTx = unsignedTx match {
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
              BaseTxSigComponent(signedTx, inputIndex, output, flags)
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
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = overrides.scriptPubKeyToSatisfyOpt match {
      case None               => output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }
    val signers = signersWithPubKeys.map(_.signFunction)
    val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
    val flags = Policy.standardFlags

    val signed: Future[TxSigComponent] = spk match {
      case multiSigSPK: MultiSignatureScriptPubKey =>
        val requiredSigs = multiSigSPK.requiredSigs
        if (signers.size < requiredSigs) {
          Future.fromTry(TxBuilderError.WrongSigner)
        } else {
          val sigComponent = overrides.sigComponentToSignOpt match {
            case None =>
              BaseTxSigComponent(unsignedTx, inputIndex, output, flags)
            case Some(sigComponent) => sigComponent
          }
          val signaturesNested = 0
            .until(requiredSigs)
            .map(i =>
              doSign(sigComponent, signers(i), hashType, isDummySignature))
          val signatures = Future.sequence(signaturesNested)
          signatures.map { sigs =>
            val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
            val signedInput = TransactionInput(unsignedInput.previousOutput,
                                               multiSigScriptSig,
                                               unsignedInput.sequence)
            val signedInputs =
              unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
            val signedTx = unsignedTx match {
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
                               inputIndex,
                               output,
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
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
      implicit ec: ExecutionContext): Future[TxSigComponent] =
    if (overrides != NestedSigning.NoOverrides) {
      Future.fromTry(TxBuilderError.NestedWitnessSPK)
    } else {
      unsignedTx match {
        case wtx: WitnessTransaction =>
          if (signers.size != 1) {
            Future.fromTry(TxBuilderError.TooManySigners)
          } else {

            val sign = signers.head.signFunction

            val pubKey = signers.head.publicKey

            val unsignedScriptWit = P2WPKHWitnessV0(pubKey)

            val unsignedTxWitness = TransactionWitness(
              wtx.witness.witnesses
                .updated(inputIndex.toInt, unsignedScriptWit))

            val unsignedWtx = WitnessTransaction(wtx.version,
                                                 wtx.inputs,
                                                 wtx.outputs,
                                                 wtx.lockTime,
                                                 unsignedTxWitness)

            val witSPK = output.scriptPubKey match {
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
              val witOutput = TransactionOutput(output.value, w)

              val wtxComp = WitnessTxSigComponentRaw(unsignedWtx,
                                                     inputIndex,
                                                     witOutput,
                                                     Policy.standardFlags)

              val signature = doSign(wtxComp, sign, hashType, isDummySignature)

              signature.map { sig =>
                val scriptWitness = P2WPKHWitnessV0(pubKey, sig)
                val signedTxWitness =
                  wtx.witness.updated(inputIndex.toInt, scriptWitness)
                val signedTx = WitnessTransaction(unsignedWtx.version,
                                                  unsignedWtx.inputs,
                                                  unsignedWtx.outputs,
                                                  unsignedWtx.lockTime,
                                                  signedTxWitness)
                WitnessTxSigComponentRaw(signedTx,
                                         inputIndex,
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
          sign(signers, output, wtx, inputIndex, hashType, isDummySignature)
      }
    }
}
object P2WPKHSigner extends P2WPKHSigner

sealed abstract class P2WSHSigner extends BitcoinSigner {
  override def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    if (overrides != NestedSigning.NoOverrides) {
      Future.fromTry(TxBuilderError.NestedWitnessSPK)
    } else {
      val spk = output.scriptPubKey
      val flags = Policy.standardFlags

      spk match {
        case _: P2WSHWitnessSPKV0 =>
          val wtx = unsignedTx match {
            case btx: BaseTransaction =>
              WitnessTransaction(btx.version,
                                 btx.inputs,
                                 btx.outputs,
                                 btx.lockTime,
                                 EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val redeemScriptF = wtx.witness.witnesses(inputIndex.toInt) match {
            case x: P2WSHWitnessV0 => Future.successful(x.redeemScript)
            case _: P2WPKHWitnessV0 =>
              Future.fromTry(TxBuilderError.NoRedeemScript)
            case EmptyScriptWitness => Future.fromTry(TxBuilderError.NoWitness)
          }
          val sigComponent =
            WitnessTxSigComponentRaw(wtx, inputIndex, output, flags)
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
              signer.sign(
                signers,
                output,
                wtx,
                inputIndex,
                hashType,
                isDummySignature,
                NestedSigning.P2WSHOverrides(sigComponent, redeemScript))
            }
          }

          val scriptWitF = signedSigComponentF.flatMap { signedSigComponent =>
            redeemScriptF.map { rs =>
              P2WSHWitnessV0(rs, signedSigComponent.scriptSignature)
            }
          }

          scriptWitF.map { scriptWit =>
            val signedWitness =
              wtx.witness.updated(inputIndex.toInt, scriptWit)
            val signedWTx = WitnessTransaction(wtx.version,
                                               wtx.inputs,
                                               wtx.outputs,
                                               wtx.lockTime,
                                               signedWitness)
            WitnessTxSigComponentRaw(signedWTx, inputIndex, output, flags)
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
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = overrides.scriptPubKeyToSatisfyOpt match {
      case None               => output.scriptPubKey
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
          val overridesForNestedSigning =
            overrides.sigComponentToSignOpt match {
              case None =>
                NestedSigning.LockTimeOverrides(lockSPK.nestedScriptPubKey)
              case Some(sigComponent) =>
                NestedSigning.NestedLockTimeOverrides(
                  externalSigComponent = sigComponent,
                  lockSPK.nestedScriptPubKey)
            }

          signer.sign(signers,
                      output,
                      unsignedTx,
                      inputIndex,
                      hashType,
                      isDummySignature,
                      overridesForNestedSigning)
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
