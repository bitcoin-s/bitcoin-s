package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  LockTimeInputInfo,
  ScriptSignatureParams
}
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  ECDigitalSignature,
  ECPrivateKey,
  HashType,
  Sign
}
import org.bitcoins.testkitcore.Implicits.*
import org.bitcoins.testkitcore.gen.{CreditingTxGen, ScriptGenerators}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class RawTxSignerTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "RawTxSigner"

  private val (spk, privKey) = ScriptGenerators.p2pkhScriptPubKey.sampleSome

  it should "fail a transaction when the user invariants fail" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector.empty,
      Vector(creditingOutput),
      TransactionConstants.lockTime
    )
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val previousOutputMap = PreviousOutputMap(Map(outPoint -> creditingOutput))
    val utxo =
      ScriptSignatureParams(
        InputInfo(
          outPoint = outPoint,
          output = creditingOutput,
          redeemScriptOpt = None,
          scriptWitnessOpt = None,
          conditionalPath = ConditionalPath.NoCondition,
          previousOutputMap = previousOutputMap,
          hashPreImages = Vector(privKey.publicKey)
        ),
        prevTransaction = creditingTx,
        signer = privKey,
        hashType = HashType.sigHashAll
      )
    val utxos = Vector(utxo)
    val feeUnit = SatoshisPerVirtualByte(currencyUnit = Satoshis(1))
    val utx = RawFinalizerFactory.txFrom(
      outputs = destinations,
      utxos = utxos,
      feeRate = feeUnit,
      changeSPK = EmptyScriptPubKey
    )
    // trivially false
    val f = (_: Seq[ScriptSignatureParams[InputInfo]], _: Transaction) => false

    assertThrows[IllegalArgumentException] {
      RawTxSigner.sign(utx, utxos, feeUnit, f)
    }
  }

  it should "fail to sign a p2pkh if we don't pass in the public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector.empty,
      Vector(creditingOutput),
      TransactionConstants.lockTime
    )
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val previousOutputMap = PreviousOutputMap(Map(outPoint -> creditingOutput))
    val utxo = ScriptSignatureParams(
      InputInfo(
        outPoint = outPoint,
        output = creditingOutput,
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        conditionalPath = ConditionalPath.NoCondition,
        previousOutputMap = previousOutputMap,
        hashPreImages = Vector(privKey.publicKey)
      ),
      prevTransaction = creditingTx,
      signer = privKey,
      hashType = HashType.sigHashAll
    )
    val utxos = Vector(utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val utx = RawFinalizerFactory.txFrom(
      outputs = destinations,
      utxos = utxos,
      feeRate = feeUnit,
      changeSPK = EmptyScriptPubKey
    )

    assertThrows[IllegalArgumentException] {
      RawTxSigner.sign(utx, utxos, feeUnit)
    }
  }

  it should "fail to sign a p2pkh if we pass in the wrong public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(
      version = TransactionConstants.validLockVersion,
      inputs = Vector.empty,
      outputs = Vector(creditingOutput),
      lockTime = TransactionConstants.lockTime
    )
    val outPoint =
      TransactionOutPoint(txId = creditingTx.txId, vout = UInt32.zero)
    val previousOutputMap = PreviousOutputMap(Map(outPoint -> creditingOutput))
    val utxo = ScriptSignatureParams(
      InputInfo(
        outPoint = outPoint,
        output = creditingOutput,
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        conditionalPath = ConditionalPath.NoCondition,
        previousOutputMap = previousOutputMap,
        hashPreImages = Vector(privKey.publicKey)
      ),
      prevTransaction = creditingTx,
      signer = privKey,
      hashType = HashType.sigHashAll
    )
    val utxos = Vector(utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val utx = RawFinalizerFactory.txFrom(
      outputs = destinations,
      utxos = utxos,
      feeRate = feeUnit,
      changeSPK = EmptyScriptPubKey
    )

    assertThrows[IllegalArgumentException] {
      RawTxSigner.sign(utx, utxos, feeUnit)
    }
  }

  it should "succeed to sign a cltv spk that uses a second-based locktime" in {
    val fundingPrivKey = ECPrivateKey.freshPrivateKey

    val lockTime = System.currentTimeMillis / 1000

    val cltvSPK =
      CLTVScriptPubKey(
        ScriptNumber(lockTime),
        P2PKScriptPubKey(fundingPrivKey.publicKey)
      )

    val creditingTx = BaseTransaction(
      version = TransactionConstants.validLockVersion,
      inputs = Vector.empty,
      outputs = Vector(TransactionOutput(Bitcoins.one, cltvSPK)),
      lockTime = TransactionConstants.lockTime
    )

    val cltvSpendingInfo = ScriptSignatureParams(
      LockTimeInputInfo(
        TransactionOutPoint(creditingTx.txId, UInt32.zero),
        Bitcoins.one,
        cltvSPK,
        ConditionalPath.NoCondition
      ),
      prevTransaction = creditingTx,
      signers = Vector(fundingPrivKey),
      hashType = HashType.sigHashAll
    )

    val utxos = Vector(cltvSpendingInfo)
    val feeUnit = SatoshisPerByte(Satoshis.one)

    val utx =
      StandardNonInteractiveFinalizer.txFrom(
        outputs = Vector(
          TransactionOutput(
            Bitcoins.one - CurrencyUnits.oneMBTC,
            EmptyScriptPubKey
          )
        ),
        utxos = utxos,
        feeRate = feeUnit,
        changeSPK = EmptyScriptPubKey
      )

    val tx = RawTxSigner.sign(utx, utxos, feeUnit)

    assert(tx.lockTime == UInt32(lockTime))
  }

  it should "succeed to sign a cltv spk that uses a block height locktime" in {
    val fundingPrivKey = ECPrivateKey.freshPrivateKey

    val lockTime = 1000

    val cltvSPK =
      CLTVScriptPubKey(
        ScriptNumber(lockTime),
        P2PKScriptPubKey(fundingPrivKey.publicKey)
      )

    val creditingTx = BaseTransaction(
      version = TransactionConstants.validLockVersion,
      inputs = Vector.empty,
      outputs = Vector(TransactionOutput(Bitcoins.one, cltvSPK)),
      lockTime = TransactionConstants.lockTime
    )

    val cltvSpendingInfo = ScriptSignatureParams(
      LockTimeInputInfo(
        TransactionOutPoint(creditingTx.txId, UInt32.zero),
        Bitcoins.one,
        cltvSPK,
        ConditionalPath.NoCondition
      ),
      prevTransaction = creditingTx,
      signers = Vector(fundingPrivKey),
      hashType = HashType.sigHashAll
    )

    val utxos = Vector(cltvSpendingInfo)
    val feeUnit = SatoshisPerByte(Satoshis.one)

    val utx =
      StandardNonInteractiveFinalizer.txFrom(
        outputs = Vector(
          TransactionOutput(
            Bitcoins.one - CurrencyUnits.oneMBTC,
            EmptyScriptPubKey
          )
        ),
        utxos = utxos,
        feeRate = feeUnit,
        changeSPK = EmptyScriptPubKey
      )

    val tx = RawTxSigner.sign(utx, utxos, feeUnit)

    assert(tx.lockTime == UInt32(lockTime))
  }

  it should "fail to sign a cltv spk that uses both a second-based and a block height locktime" in {
    val fundingPrivKey1 = ECPrivateKey.freshPrivateKey
    val fundingPrivKey2 = ECPrivateKey.freshPrivateKey

    val lockTime1 = System.currentTimeMillis / 1000
    val lockTime2 = 1000

    val cltvSPK1 =
      CLTVScriptPubKey(
        ScriptNumber(lockTime1),
        P2PKScriptPubKey(fundingPrivKey1.publicKey)
      )
    val cltvSPK2 =
      CLTVScriptPubKey(
        ScriptNumber(lockTime2),
        P2PKScriptPubKey(fundingPrivKey2.publicKey)
      )

    val cltvSpendingInfo1 = ScriptSignatureParams(
      LockTimeInputInfo(
        TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
        Bitcoins.one,
        cltvSPK1,
        ConditionalPath.NoCondition
      ),
      prevTransaction = EmptyTransaction,
      signers = Vector(fundingPrivKey1),
      hashType = HashType.sigHashAll
    )

    val cltvSpendingInfo2 = ScriptSignatureParams(
      LockTimeInputInfo(
        TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
        Bitcoins.one,
        cltvSPK2,
        ConditionalPath.NoCondition
      ),
      prevTransaction = EmptyTransaction,
      signers = Vector(fundingPrivKey2),
      hashType = HashType.sigHashAll
    )

    val utxos = Vector(cltvSpendingInfo1, cltvSpendingInfo2)
    val feeRate = SatoshisPerByte(Satoshis.one)

    val utx = RawFinalizerFactory.txFrom(
      Vector(
        TransactionOutput(
          Bitcoins.one + Bitcoins.one - CurrencyUnits.oneMBTC,
          EmptyScriptPubKey
        )
      ),
      utxos,
      UInt32.zero,
      feeRate,
      EmptyScriptPubKey
    )

    assertThrows[IllegalArgumentException](
      RawTxSigner.sign(utx, utxos, feeRate)
    )
  }

  it should "sign a mix of spks in a tx and then have it verified" in {
    forAll(CreditingTxGen.inputsAndOutputs(), ScriptGenerators.scriptPubKey) {
      case ((creditingTxsInfo, destinations), (changeSPK, _)) =>
        val fee = SatoshisPerVirtualByte(Satoshis(1000))
        val utx =
          StandardNonInteractiveFinalizer.txFrom(
            outputs = destinations,
            utxos = creditingTxsInfo,
            feeRate = fee,
            changeSPK = changeSPK
          )
        val tx = RawTxSigner.sign(utx, creditingTxsInfo.toVector, fee)

        assert(BitcoinScriptUtil.verifyScript(tx, creditingTxsInfo.toVector))
    }
  }

  it should "dummy sign a mix of spks in a tx and fill it with dummy signatures" in {
    forAll(CreditingTxGen.inputsAndOutputs(), ScriptGenerators.scriptPubKey) {
      case ((creditingTxsInfo, destinations), (changeSPK, _)) =>
        val fee = SatoshisPerVirtualByte(Satoshis(1000))
        val hashType = HashType.sigHashAll
        val dummySpendingInfos = creditingTxsInfo.map { spendingInfo =>
          val inputInfo = spendingInfo.inputInfo

          val mockSigners =
            inputInfo.pubKeys.take(inputInfo.requiredSigs).map(Sign.dummySign)

          inputInfo.toSpendingInfo(
            EmptyTransaction,
            mockSigners,
            hashType
          )
        }

        val utx =
          StandardNonInteractiveFinalizer.txFrom(
            outputs = destinations,
            utxos = dummySpendingInfos,
            feeRate = fee,
            changeSPK = changeSPK
          )
        val tx = RawTxSigner.sign(
          utx,
          dummySpendingInfos.toVector,
          RawTxSigner.emptyInvariant
        )

        val dummyLowRHashType =
          ECDigitalSignature.dummyLowR.appendHashType(hashType)
        // Can't use BitcoinScriptUtil.verifyScript because it will pass for things
        // with EmptyScriptPubKeys or Multisig with 0 required sigs
        tx match {
          case EmptyTransaction =>
            succeed
          case btx: BaseTransaction =>
            assert(
              btx.inputs.forall(
                _.scriptSignature.signatures.forall(
                  _ == dummyLowRHashType
                )
              ),
              s"btx.inputs.scriptSigs=${btx.inputs.map(_.scriptSignature)}"
            )
          case wtx: WitnessTransaction =>
            assert(
              wtx.witness.witnesses.forall {
                case p2wsh: P2WSHWitnessV0 =>
                  p2wsh.signatures.forall(_ == dummyLowRHashType)
                case p2wpkh: P2WPKHWitnessV0 =>
                  p2wpkh.signature == dummyLowRHashType
                case EmptyScriptWitness =>
                  true

                case taprootWitness: TaprootWitness =>
                  throw new UnsupportedOperationException(
                    s"Taproot not supported, got=$taprootWitness"
                  )
              }
            )
        }
    }
  }

  it should "sign a mix of p2sh/p2wsh in a tx and then have it verified" in {
    forAll(
      CreditingTxGen.inputsAndOutputs(CreditingTxGen.nestedOutputs),
      ScriptGenerators.scriptPubKey
    ) { case ((creditingTxsInfo, destinations), (changeSPK, _)) =>
      val fee = SatoshisPerByte(Satoshis(1000))
      val utx =
        StandardNonInteractiveFinalizer.txFrom(
          outputs = destinations,
          utxos = creditingTxsInfo,
          feeRate = fee,
          changeSPK = changeSPK
        )
      val tx = RawTxSigner.sign(utx, creditingTxsInfo.toVector, fee)

      assert(BitcoinScriptUtil.verifyScript(tx, creditingTxsInfo.toVector))
    }
  }
}
