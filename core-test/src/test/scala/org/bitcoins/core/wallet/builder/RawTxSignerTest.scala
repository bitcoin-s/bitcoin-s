package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  LockTimeInputInfo,
  ScriptSignatureParams
}
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPrivateKey}
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.{CreditingTxGen, ScriptGenerators}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class RawTxSignerTest extends BitcoinSAsyncTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "RawTxSigner"

  private val (spk, privKey) = ScriptGenerators.p2pkhScriptPubKey.sampleSome

  it should "fail a transaction when the user invariants fail" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(TransactionConstants.validLockVersion,
                                      Nil,
                                      Vector(creditingOutput),
                                      TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo =
      ScriptSignatureParams(
        InputInfo(
          outPoint = outPoint,
          output = creditingOutput,
          redeemScriptOpt = None,
          scriptWitnessOpt = None,
          conditionalPath = ConditionalPath.NoCondition,
          hashPreImages = Vector(privKey.publicKey)
        ),
        prevTransaction = creditingTx,
        signer = privKey,
        hashType = HashType.sigHashAll
      )
    val utxos = Vector(utxo)
    val feeUnit = SatoshisPerVirtualByte(currencyUnit = Satoshis(1))
    val utxF = StandardNonInteractiveFinalizer.txFrom(outputs = destinations,
                                                      utxos = utxos,
                                                      feeRate = feeUnit,
                                                      changeSPK =
                                                        EmptyScriptPubKey)
    //trivially false
    val f = (_: Seq[ScriptSignatureParams[InputInfo]], _: Transaction) => false

    recoverToSucceededIf[IllegalArgumentException] {
      utxF.flatMap(utx => RawTxSigner.sign(utx, utxos, feeUnit, f))
    }
  }

  it should "fail to sign a p2pkh if we don't pass in the public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(TransactionConstants.validLockVersion,
                                      Nil,
                                      Vector(creditingOutput),
                                      TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = ScriptSignatureParams(
      InputInfo(
        outPoint = outPoint,
        output = creditingOutput,
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        conditionalPath = ConditionalPath.NoCondition,
        hashPreImages = Vector(privKey.publicKey)
      ),
      prevTransaction = creditingTx,
      signer = privKey,
      hashType = HashType.sigHashAll
    )
    val utxos = Vector(utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val utxF = StandardNonInteractiveFinalizer.txFrom(outputs = destinations,
                                                      utxos = utxos,
                                                      feeRate = feeUnit,
                                                      changeSPK =
                                                        EmptyScriptPubKey)

    recoverToSucceededIf[IllegalArgumentException] {
      utxF.flatMap(utx => RawTxSigner.sign(utx, utxos, feeUnit))
    }
  }

  it should "fail to sign a p2pkh if we pass in the wrong public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version =
                                        TransactionConstants.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint =
      TransactionOutPoint(txId = creditingTx.txId, vout = UInt32.zero)
    val utxo = ScriptSignatureParams(
      InputInfo(
        outPoint = outPoint,
        output = creditingOutput,
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        conditionalPath = ConditionalPath.NoCondition,
        hashPreImages = Vector(privKey.publicKey)
      ),
      prevTransaction = creditingTx,
      signer = privKey,
      hashType = HashType.sigHashAll
    )
    val utxos = Vector(utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val utxF = StandardNonInteractiveFinalizer.txFrom(outputs = destinations,
                                                      utxos = utxos,
                                                      feeRate = feeUnit,
                                                      changeSPK =
                                                        EmptyScriptPubKey)

    recoverToSucceededIf[IllegalArgumentException] {
      utxF.flatMap(utx => RawTxSigner.sign(utx, utxos, feeUnit))
    }
  }

  it should "succeed to sign a cltv spk that uses a second-based locktime" in {
    val fundingPrivKey = ECPrivateKey.freshPrivateKey

    val lockTime = System.currentTimeMillis / 1000

    val cltvSPK =
      CLTVScriptPubKey(ScriptNumber(lockTime),
                       P2PKScriptPubKey(fundingPrivKey.publicKey))

    val creditingTx = BaseTransaction(
      version = TransactionConstants.validLockVersion,
      inputs = Nil,
      outputs = Vector(TransactionOutput(Bitcoins.one, cltvSPK)),
      lockTime = TransactionConstants.lockTime
    )

    val cltvSpendingInfo = ScriptSignatureParams(
      LockTimeInputInfo(TransactionOutPoint(creditingTx.txId, UInt32.zero),
                        Bitcoins.one,
                        cltvSPK,
                        ConditionalPath.NoCondition),
      prevTransaction = creditingTx,
      signers = Vector(fundingPrivKey),
      hashType = HashType.sigHashAll
    )

    val utxos = Vector(cltvSpendingInfo)
    val feeUnit = SatoshisPerByte(Satoshis.one)

    val utxF =
      StandardNonInteractiveFinalizer.txFrom(
        outputs = Vector(
          TransactionOutput(Bitcoins.one - CurrencyUnits.oneMBTC,
                            EmptyScriptPubKey)),
        utxos = utxos,
        feeRate = feeUnit,
        changeSPK = EmptyScriptPubKey
      )

    utxF
      .flatMap(utx => RawTxSigner.sign(utx, utxos, feeUnit))
      .map(tx => assert(tx.lockTime == UInt32(lockTime)))
  }

  it should "succeed to sign a cltv spk that uses a block height locktime" in {
    val fundingPrivKey = ECPrivateKey.freshPrivateKey

    val lockTime = 1000

    val cltvSPK =
      CLTVScriptPubKey(ScriptNumber(lockTime),
                       P2PKScriptPubKey(fundingPrivKey.publicKey))

    val creditingTx = BaseTransaction(
      version = TransactionConstants.validLockVersion,
      inputs = Nil,
      outputs = Vector(TransactionOutput(Bitcoins.one, cltvSPK)),
      lockTime = TransactionConstants.lockTime
    )

    val cltvSpendingInfo = ScriptSignatureParams(
      LockTimeInputInfo(TransactionOutPoint(creditingTx.txId, UInt32.zero),
                        Bitcoins.one,
                        cltvSPK,
                        ConditionalPath.NoCondition),
      prevTransaction = creditingTx,
      signers = Vector(fundingPrivKey),
      hashType = HashType.sigHashAll
    )

    val utxos = Vector(cltvSpendingInfo)
    val feeUnit = SatoshisPerByte(Satoshis.one)

    val utxF =
      StandardNonInteractiveFinalizer.txFrom(
        outputs = Vector(
          TransactionOutput(Bitcoins.one - CurrencyUnits.oneMBTC,
                            EmptyScriptPubKey)),
        utxos = utxos,
        feeRate = feeUnit,
        changeSPK = EmptyScriptPubKey
      )

    utxF
      .flatMap(utx => RawTxSigner.sign(utx, utxos, feeUnit))
      .map(tx => assert(tx.lockTime == UInt32(lockTime)))
  }

  it should "fail to sign a cltv spk that uses both a second-based and a block height locktime" in {
    val fundingPrivKey1 = ECPrivateKey.freshPrivateKey
    val fundingPrivKey2 = ECPrivateKey.freshPrivateKey

    val lockTime1 = System.currentTimeMillis / 1000
    val lockTime2 = 1000

    val cltvSPK1 =
      CLTVScriptPubKey(ScriptNumber(lockTime1),
                       P2PKScriptPubKey(fundingPrivKey1.publicKey))
    val cltvSPK2 =
      CLTVScriptPubKey(ScriptNumber(lockTime2),
                       P2PKScriptPubKey(fundingPrivKey2.publicKey))

    val cltvSpendingInfo1 = ScriptSignatureParams(
      LockTimeInputInfo(TransactionOutPoint(DoubleSha256DigestBE.empty,
                                            UInt32.zero),
                        Bitcoins.one,
                        cltvSPK1,
                        ConditionalPath.NoCondition),
      prevTransaction = EmptyTransaction,
      signers = Vector(fundingPrivKey1),
      hashType = HashType.sigHashAll
    )

    val cltvSpendingInfo2 = ScriptSignatureParams(
      LockTimeInputInfo(TransactionOutPoint(DoubleSha256DigestBE.empty,
                                            UInt32.one),
                        Bitcoins.one,
                        cltvSPK2,
                        ConditionalPath.NoCondition),
      prevTransaction = EmptyTransaction,
      signers = Vector(fundingPrivKey2),
      hashType = HashType.sigHashAll
    )

    val utxos = Vector(cltvSpendingInfo1, cltvSpendingInfo2)
    val feeRate = SatoshisPerByte(Satoshis.one)

    val utxF =
      StandardNonInteractiveFinalizer.txFrom(
        Vector(
          TransactionOutput(Bitcoins.one + Bitcoins.one - CurrencyUnits.oneMBTC,
                            EmptyScriptPubKey)),
        utxos,
        feeRate,
        EmptyScriptPubKey
      )

    recoverToSucceededIf[IllegalArgumentException](
      utxF.flatMap(utx => RawTxSigner.sign(utx, utxos, feeRate))
    )
  }

  it should "sign a mix of spks in a tx and then have it verified" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs(),
                ScriptGenerators.scriptPubKey) {
      case ((creditingTxsInfo, destinations), (changeSPK, _)) =>
        val fee = SatoshisPerVirtualByte(Satoshis(1000))
        val utxF =
          StandardNonInteractiveFinalizer.txFrom(outputs = destinations,
                                                 utxos = creditingTxsInfo,
                                                 feeRate = fee,
                                                 changeSPK = changeSPK)
        val txF = utxF.flatMap(utx =>
          RawTxSigner.sign(utx, creditingTxsInfo.toVector, fee))

        txF.map { tx =>
          assert(BitcoinScriptUtil.verifyScript(tx, creditingTxsInfo.toVector))
        }
    }
  }

  it should "sign a mix of p2sh/p2wsh in a tx and then have it verified" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs(CreditingTxGen.nestedOutputs),
                ScriptGenerators.scriptPubKey) {
      case ((creditingTxsInfo, destinations), (changeSPK, _)) =>
        val fee = SatoshisPerByte(Satoshis(1000))
        val utxF =
          StandardNonInteractiveFinalizer.txFrom(outputs = destinations,
                                                 utxos = creditingTxsInfo,
                                                 feeRate = fee,
                                                 changeSPK = changeSPK)
        val txF = utxF.flatMap(utx =>
          RawTxSigner.sign(utx, creditingTxsInfo.toVector, fee))

        txF.map { tx =>
          assert(BitcoinScriptUtil.verifyScript(tx, creditingTxsInfo.toVector))
        }
    }
  }
}
