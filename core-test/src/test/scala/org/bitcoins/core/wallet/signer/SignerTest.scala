package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto.{
  BaseTxSigComponent,
  TxSigComponent,
  WitnessTxSigComponentP2SH,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.wallet.builder.{
  RawTxSigner,
  StandardNonInteractiveFinalizer
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.ECDigitalSignature
import org.bitcoins.testkitcore.gen.{
  CreditingTxGen,
  GenUtil,
  ScriptGenerators,
  TransactionGenerators
}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.annotation.nowarn

class SignerTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "Signer"

  it should "fail to sign a UnassignedSegwit UTXO" in {
    val p2wpkh = GenUtil.sample(CreditingTxGen.p2wpkhOutput)
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    val spendingInfo = ScriptSignatureParams(
      UnassignedSegwitNativeInputInfo(
        p2wpkh.outPoint,
        p2wpkh.amount,
        p2wpkh.output.scriptPubKey.asInstanceOf[WitnessScriptPubKey],
        InputInfo.getScriptWitness(p2wpkh.inputInfo).get,
        p2wpkh.conditionalPath,
        p2wpkh.signers.map(_.publicKey)
      ),
      p2wpkh.prevTransaction,
      p2wpkh.signers,
      p2wpkh.hashType
    )
    assertThrows[UnsupportedOperationException](
      BitcoinSigner.sign(spendingInfo, tx, isDummySignature = false))
  }

  it should "fail to sign a P2SH UTXO" in {
    val p2sh = GenUtil.sample(CreditingTxGen.p2shOutput)
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    assertThrows[IllegalArgumentException](
      BitcoinSigner.sign(p2sh, tx, isDummySignature = false))
  }

  it should "fail if there are inconsistent P2WPKH spending infos" in {
    val dumbSpendingInfo = GenUtil.sample(CreditingTxGen.output)
    val p2wpkh = GenUtil
      .sample(CreditingTxGen.p2wpkhOutput)
      .asInstanceOf[ScriptSignatureParams[P2WPKHV0InputInfo]]
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    assertThrows[IllegalArgumentException] {
      P2WPKHSigner.sign(dumbSpendingInfo, tx, isDummySignature = false, p2wpkh)
    }
  }

  it should "fail if there are inconsistent P2WSH spending infos" in {
    val dumbSpendingInfo = GenUtil.sample(CreditingTxGen.output)
    val p2wsh = GenUtil
      .sample(CreditingTxGen.p2wshOutput)
      .asInstanceOf[ScriptSignatureParams[P2WSHV0InputInfo]]
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    assertThrows[IllegalArgumentException] {
      P2WSHSigner.sign(dumbSpendingInfo, tx, isDummySignature = false, p2wsh)
    }
  }

  it must "sign a mix of spks in a tx and then verify that single signing agrees" in {
    forAll(CreditingTxGen.inputsAndOutputs(), ScriptGenerators.scriptPubKey) {
      case ((creditingTxsInfos, destinations), (changeSPK, _)) =>
        val fee = SatoshisPerVirtualByte(Satoshis(1000))

        val unsignedTx =
          StandardNonInteractiveFinalizer.txFrom(destinations,
                                                 creditingTxsInfos,
                                                 fee,
                                                 changeSPK)

        val signedTx =
          RawTxSigner.sign(unsignedTx, creditingTxsInfos.toVector, fee)

        val singleSigs: Vector[Vector[ECDigitalSignature]] = {
          val singleInfosVec: Vector[Vector[ECSignatureParams[InputInfo]]] =
            creditingTxsInfos.toVector.map(_.toSingles)
          singleInfosVec.map { singleInfos =>
            singleInfos.map { singleInfo =>
              val keyAndSig =
                BitcoinSigner.signSingle(singleInfo,
                                         unsignedTx,
                                         isDummySignature = false)

              keyAndSig.signature
            }
          }
        }

        signedTx.inputs.zipWithIndex.foreach { case (input, inputIndex) =>
          val infoAndIndexOpt = creditingTxsInfos.zipWithIndex
            .find(_._1.outPoint == input.previousOutput)
          assert(infoAndIndexOpt.isDefined)
          val (info, index) = infoAndIndexOpt.get
          val sigs = singleSigs(index)

          val expectedSigs =
            if (InputInfo.getScriptWitness(info.inputInfo).isEmpty) {
              input.scriptSignature.signatures
            } else {
              signedTx
                .asInstanceOf[WitnessTransaction]
                .witness
                .witnesses(inputIndex) match {
                case p2wpkh: P2WPKHWitnessV0 => Vector(p2wpkh.signature)
                case p2wsh: P2WSHWitnessV0   => p2wsh.signatures
                case EmptyScriptWitness      => Vector.empty
                case taprootWitness: TaprootWitness =>
                  throw new UnsupportedOperationException(
                    s"Taproot not supported, got=$taprootWitness")
              }
            }

          assert(sigs.length == expectedSigs.length)
          assert(sigs.forall(expectedSigs.contains))
        }

        succeed
    }
  }

  it should "have old and new doSign functions agree" in {
    forAll(CreditingTxGen.inputsAndOutputs(), ScriptGenerators.scriptPubKey) {
      case ((creditingTxsInfo, destinations), (changeSPK, _)) =>
        val fee = SatoshisPerVirtualByte(Satoshis(100))

        val spendingTx = StandardNonInteractiveFinalizer
          .txFrom(outputs = destinations,
                  utxos = creditingTxsInfo,
                  feeRate = fee,
                  changeSPK = changeSPK)

        val prevOutMap =
          PreviousOutputMap.fromScriptSignatureParams(creditingTxsInfo)

        val correctSigs =
          creditingTxsInfo.flatMap { signInfo =>
            signInfo.signers.map { signer =>
              val txSignatureComponent =
                TxSigComponent(signInfo.inputInfo, spendingTx, prevOutMap)
              @nowarn val oldSig = BitcoinSigner.doSign(txSignatureComponent,
                                                        signer.sign,
                                                        signInfo.hashType,
                                                        isDummySignature =
                                                          false)

              val newSig = BitcoinSigner.doSign(spendingTx,
                                                signInfo,
                                                signer.sign,
                                                signInfo.hashType,
                                                isDummySignature = false)

              (oldSig.r == newSig.r) &&
              (oldSig.s == newSig.s) &&
              (oldSig.hex == newSig.hex)
            }
          }

        assert(correctSigs.forall(_ == true))
    }
  }

  def inputIndex(
      spendingInfo: InputSigningInfo[InputInfo],
      tx: Transaction): Int = {
    tx.inputs.zipWithIndex
      .find(_._1.previousOutput == spendingInfo.outPoint) match {
      case Some((_, index)) => index
      case None =>
        throw new IllegalArgumentException(
          "Transaction did not contain expected input.")
    }
  }

  def createProgram(
      tx: Transaction,
      idx: Int,
      utxo: InputSigningInfo[InputInfo]): PreExecutionScriptProgram = {
    val output = utxo.output

    val spk = output.scriptPubKey

    val amount = output.value

    val txSigComponent = spk match {
      case witSPK: WitnessScriptPubKeyV0 =>
        val o = TransactionOutput(amount, witSPK)
        WitnessTxSigComponentRaw(tx.asInstanceOf[WitnessTransaction],
                                 UInt32(idx),
                                 o,
                                 Policy.standardFlags)
      case _: UnassignedWitnessScriptPubKey | _: TaprootScriptPubKey => ???
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: WitnessCommitment | _: CSVScriptPubKey | _: CLTVScriptPubKey |
          _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
          EmptyScriptPubKey) =>
        val o = TransactionOutput(CurrencyUnits.zero, x)
        BaseTxSigComponent(tx, UInt32(idx), o, Policy.standardFlags)

      case _: P2SHScriptPubKey =>
        val p2shScriptSig =
          tx.inputs(idx).scriptSignature.asInstanceOf[P2SHScriptSignature]
        p2shScriptSig.redeemScript match {

          case _: WitnessScriptPubKey =>
            WitnessTxSigComponentP2SH(transaction =
                                        tx.asInstanceOf[WitnessTransaction],
                                      inputIndex = UInt32(idx),
                                      output = output,
                                      flags = Policy.standardFlags)

          case _ =>
            BaseTxSigComponent(tx, UInt32(idx), output, Policy.standardFlags)
        }
    }

    PreExecutionScriptProgram(txSigComponent)
  }

  def verifyScripts(
      tx: Transaction,
      utxos: Vector[InputSigningInfo[InputInfo]]): Boolean = {
    val programs: Vector[PreExecutionScriptProgram] =
      tx.inputs.zipWithIndex.toVector.map {
        case (input: TransactionInput, idx: Int) =>
          val utxo = utxos.find(_.outPoint == input.previousOutput).get
          createProgram(tx, idx, utxo)
      }
    ScriptInterpreter.runAllVerify(programs)
  }

  it must "sign p2wsh inputs correctly when provided no witness data" in {
    forAll(CreditingTxGen.inputsAndOutputs(CreditingTxGen.p2wshOutputs),
           ScriptGenerators.scriptPubKey) {
      case ((creditingTxsInfos, destinations), (changeSPK, _)) =>
        val fee = SatoshisPerVirtualByte(Satoshis(100))

        val unsignedTx =
          StandardNonInteractiveFinalizer.txFrom(destinations,
                                                 creditingTxsInfos,
                                                 fee,
                                                 changeSPK)

        val singleSigs: Vector[Vector[PartialSignature]] = {
          val singleInfosVec: Vector[Vector[ECSignatureParams[InputInfo]]] =
            creditingTxsInfos.toVector.map(_.toSingles)
          singleInfosVec.map { singleInfos =>
            singleInfos.map { singleInfo =>
              val wtx =
                WitnessTransaction(unsignedTx.version,
                                   unsignedTx.inputs,
                                   unsignedTx.outputs,
                                   unsignedTx.lockTime,
                                   EmptyWitness.fromInputs(unsignedTx.inputs))
              BitcoinSigner.signSingle(singleInfo,
                                       wtx,
                                       isDummySignature = false)

            }
          }
        }

        val psbt =
          creditingTxsInfos.foldLeft(PSBT.fromUnsignedTx(unsignedTx)) {
            (psbt, spendInfo) =>
              val idx = inputIndex(spendInfo, unsignedTx)
              psbt
                .addUTXOToInput(spendInfo.prevTransaction, idx)
                .addScriptWitnessToInput(
                  InputInfo.getScriptWitness(spendInfo.inputInfo).get,
                  idx)
                .addSignatures(singleSigs(idx), idx)
          }

        val signedTx = psbt.finalizePSBT.get.extractTransactionAndValidate

        assert(verifyScripts(signedTx.get, creditingTxsInfos.toVector))
    }
  }
}
