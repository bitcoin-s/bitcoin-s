package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.{RegTest, TestNet3}
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder.UTXOMap
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfoFull,
  ConditionalPath,
  LockTimeSpendingInfoFull,
  UTXOSpendingInfo
}
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.{
  ChainParamsGenerator,
  CreditingTxGen,
  ScriptGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class BitcoinTxBuilderTest extends BitcoinSAsyncTest {
  val tc: TransactionConstants.type = TransactionConstants
  val (spk, privKey) = ScriptGenerators.p2pkhScriptPubKey.sampleSome

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "BitcoinTxBuilder"

  // We had a matcherror when passing in a vector of UTXOs,
  // because a match statement on a Seq relied on the ::
  // deconstructor. You would assume the compiler could
  // warn you about that...
  it must "work with a list and a vector of UTXOs" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfoFull(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Vector(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )

    val listF =
      BitcoinTxBuilder(destinations = Vector.empty,
                       utxos = List(utxo),
                       feeRate = SatoshisPerByte(1.sat),
                       changeSPK = EmptyScriptPubKey,
                       network = RegTest)

    val vecF =
      BitcoinTxBuilder(destinations = Vector.empty,
                       utxos = List(utxo),
                       feeRate = SatoshisPerByte(1.sat),
                       changeSPK = EmptyScriptPubKey,
                       network = RegTest)

    for {
      _ <- listF
      _ <- vecF
    } yield succeed

  }

  it must "failed to build a transaction that mints money out of thin air" in {

    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfoFull(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Vector(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilder = BitcoinTxBuilder(destinations = destinations,
                                     utxos = utxoMap,
                                     feeRate = feeUnit,
                                     changeSPK = EmptyScriptPubKey,
                                     network = TestNet3)
    val resultFuture = txBuilder.flatMap(_.sign)
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "fail to build a transaction when we pass in a negative fee rate" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfoFull(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Vector(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis(-1))
    val txBuilder = BitcoinTxBuilder(destinations = destinations,
                                     utxos = utxoMap,
                                     feeRate = feeUnit,
                                     changeSPK = EmptyScriptPubKey,
                                     network = TestNet3)
    recoverToSucceededIf[IllegalArgumentException] {
      txBuilder
    }
  }

  it must "fail a transaction when the user invariants fail" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Vector(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfoFull(outPoint,
                                           creditingOutput,
                                           Vector(privKey),
                                           None,
                                           None,
                                           HashType.sigHashAll,
                                           conditionalPath =
                                             ConditionalPath.NoConditionsLeft)
    val utxoMap: UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(currencyUnit = Satoshis(1))
    val txBuilder = BitcoinTxBuilder(destinations = destinations,
                                     utxos = utxoMap,
                                     feeRate = feeUnit,
                                     changeSPK = EmptyScriptPubKey,
                                     network = TestNet3)
    //trivially false
    val f = (_: Seq[BitcoinUTXOSpendingInfoFull], _: Transaction) => false
    val resultFuture = txBuilder.flatMap(_.sign(f))
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "be able to create a BitcoinTxBuilder from UTXOTuple and UTXOMap" in {
    val creditingOutput =
      TransactionOutput(value = CurrencyUnits.zero, scriptPubKey = spk)
    val destinations = {
      Vector(
        TransactionOutput(value = Satoshis.one,
                          scriptPubKey = EmptyScriptPubKey))
    }
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfoFull(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Vector(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)
    val utxoSpendingInfo = BitcoinUTXOSpendingInfoFull(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Vector(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilderMap = BitcoinTxBuilder(destinations = destinations,
                                        utxos = utxoMap,
                                        feeRate = feeUnit,
                                        changeSPK = EmptyScriptPubKey,
                                        network = TestNet3)
    val txBuilderTuple = BitcoinTxBuilder(destinations = destinations,
                                          utxos = Vector(utxoSpendingInfo),
                                          feeRate = feeUnit,
                                          changeSPK = EmptyScriptPubKey,
                                          network = TestNet3)

    txBuilderTuple.flatMap { tup =>
      txBuilderMap.map { map =>
        assert(map == tup)
      }
    }
  }

  it must "fail to build a tx if you have the wrong redeemscript" in {
    val p2sh = P2SHScriptPubKey(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2sh)
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfoFull(
        outPoint = outPoint,
        output = creditingOutput,
        signers = Vector(privKey),
        redeemScriptOpt = Some(EmptyScriptPubKey),
        scriptWitnessOpt = None,
        hashType = HashType.sigHashAll,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "fail to build a tx if you have the wrong script witness" in {
    val p2wsh = P2WSHWitnessSPKV0(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wsh)
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Vector(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfoFull(
        outPoint,
        creditingOutput,
        Vector(privKey),
        None,
        Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        HashType.sigHashAll,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "fail to sign a p2pkh if we don't pass in the public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Vector(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfoFull(
      outPoint,
      creditingOutput,
      Vector(privKey),
      None,
      Some(P2WSHWitnessV0(EmptyScriptPubKey)),
      HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilderWitness = BitcoinTxBuilder(destinations,
                                            utxoMap,
                                            feeUnit,
                                            EmptyScriptPubKey,
                                            TestNet3)
    val resultFuture = txBuilderWitness.flatMap(_.sign)
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "fail to sign a p2pkh if we pass in the wrong public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint =
      TransactionOutPoint(txId = creditingTx.txId, vout = UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfoFull(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Vector(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilderWitness = BitcoinTxBuilder(destinations = destinations,
                                            utxos = utxoMap,
                                            feeRate = feeUnit,
                                            changeSPK = EmptyScriptPubKey,
                                            network = TestNet3)
    val resultFuture = txBuilderWitness.flatMap(_.sign)
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "fail to sign a p2wpkh if we don't pass in the public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(pubKey = privKey.publicKey)
    val creditingOutput =
      TransactionOutput(value = CurrencyUnits.zero, scriptPubKey = p2wpkh)
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint =
      TransactionOutPoint(txId = creditingTx.txId, vout = UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfoFull(
        outPoint = outPoint,
        output = creditingOutput,
        signers = Vector(privKey),
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        hashType = HashType.sigHashAll,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "fail to sign a p2wpkh if we pass in the wrong public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wpkh)
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Vector(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfoFull(
        outPoint,
        creditingOutput,
        Vector(privKey),
        None,
        Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        HashType.sigHashAll,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "succeed to sign a cltv spk that uses a second-based locktime" in {
    val fundingPrivKey = ECPrivateKey.freshPrivateKey

    val lockTime = System.currentTimeMillis / 1000

    val cltvSPK =
      CLTVScriptPubKey(ScriptNumber(lockTime),
                       P2PKScriptPubKey(fundingPrivKey.publicKey))

    val cltvSpendingInfo = LockTimeSpendingInfoFull(
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
      Bitcoins.one,
      cltvSPK,
      Vector(fundingPrivKey),
      HashType.sigHashAll,
      ConditionalPath.NoConditionsLeft
    )

    val txBuilderF =
      BitcoinTxBuilder(
        Vector(
          TransactionOutput(Bitcoins.one - CurrencyUnits.oneMBTC,
                            EmptyScriptPubKey)),
        Vector(cltvSpendingInfo),
        SatoshisPerByte(Satoshis.one),
        EmptyScriptPubKey,
        RegTest
      )

    txBuilderF
      .flatMap(_.sign)
      .map(tx => assert(tx.lockTime == UInt32(lockTime)))
  }

  it must "succeed to sign a cltv spk that uses a block height locktime" in {
    val fundingPrivKey = ECPrivateKey.freshPrivateKey

    val lockTime = 1000

    val cltvSPK =
      CLTVScriptPubKey(ScriptNumber(lockTime),
                       P2PKScriptPubKey(fundingPrivKey.publicKey))

    val cltvSpendingInfo = LockTimeSpendingInfoFull(
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
      Bitcoins.one,
      cltvSPK,
      Vector(fundingPrivKey),
      HashType.sigHashAll,
      ConditionalPath.NoConditionsLeft
    )

    val txBuilderF =
      BitcoinTxBuilder(
        Vector(
          TransactionOutput(Bitcoins.one - CurrencyUnits.oneMBTC,
                            EmptyScriptPubKey)),
        Vector(cltvSpendingInfo),
        SatoshisPerByte(Satoshis.one),
        EmptyScriptPubKey,
        RegTest
      )

    txBuilderF
      .flatMap(_.sign)
      .map(tx => assert(tx.lockTime == UInt32(lockTime)))
  }

  it must "fail to sign a cltv spk that uses both a second-based and a block height locktime" in {
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

    val cltvSpendingInfo1 = LockTimeSpendingInfoFull(
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
      Bitcoins.one,
      cltvSPK1,
      Vector(fundingPrivKey1),
      HashType.sigHashAll,
      ConditionalPath.NoConditionsLeft
    )

    val cltvSpendingInfo2 = LockTimeSpendingInfoFull(
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
      Bitcoins.one,
      cltvSPK2,
      Vector(fundingPrivKey2),
      HashType.sigHashAll,
      ConditionalPath.NoConditionsLeft
    )

    val txBuilderF =
      BitcoinTxBuilder(
        Vector(
          TransactionOutput(Bitcoins.one + Bitcoins.one - CurrencyUnits.oneMBTC,
                            EmptyScriptPubKey)),
        Vector(cltvSpendingInfo1, cltvSpendingInfo2),
        SatoshisPerByte(Satoshis.one),
        EmptyScriptPubKey,
        RegTest
      )

    recoverToSucceededIf[IllegalArgumentException](
      txBuilderF.flatMap(_.unsignedTx)
    )
  }

  def verifyScript(
      tx: Transaction,
      utxos: Vector[UTXOSpendingInfo]): Boolean = {
    val programs: Vector[PreExecutionScriptProgram] =
      tx.inputs.zipWithIndex.toVector.map {
        case (input: TransactionInput, idx: Int) =>
          val outpoint = input.previousOutput

          val creditingTx =
            utxos.find(u => u.outPoint.txId == outpoint.txId).get

          val output = creditingTx.output

          val spk = output.scriptPubKey

          val amount = output.value

          val txSigComponent = spk match {
            case witSPK: WitnessScriptPubKeyV0 =>
              val o = TransactionOutput(amount, witSPK)
              WitnessTxSigComponentRaw(tx.asInstanceOf[WitnessTransaction],
                                       UInt32(idx),
                                       o,
                                       Policy.standardFlags)
            case _: UnassignedWitnessScriptPubKey => ???
            case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
                _: WitnessCommitment | _: CSVScriptPubKey |
                _: CLTVScriptPubKey | _: ConditionalScriptPubKey |
                _: NonStandardScriptPubKey | EmptyScriptPubKey) =>
              val o = TransactionOutput(CurrencyUnits.zero, x)
              BaseTxSigComponent(tx, UInt32(idx), o, Policy.standardFlags)

            case _: P2SHScriptPubKey =>
              val p2shScriptSig =
                tx.inputs(idx).scriptSignature.asInstanceOf[P2SHScriptSignature]
              p2shScriptSig.redeemScript match {

                case _: WitnessScriptPubKey =>
                  WitnessTxSigComponentP2SH(
                    transaction = tx.asInstanceOf[WitnessTransaction],
                    inputIndex = UInt32(idx),
                    output = output,
                    flags = Policy.standardFlags)

                case _ =>
                  BaseTxSigComponent(tx,
                                     UInt32(idx),
                                     output,
                                     Policy.standardFlags)
              }
          }

          PreExecutionScriptProgram(txSigComponent)
      }
    ScriptInterpreter.runAllVerify(programs)
  }

  it must "sign a mix of spks in a tx and then have it verified" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs(),
                ScriptGenerators.scriptPubKey,
                ChainParamsGenerator.bitcoinNetworkParams) {
      case ((creditingTxsInfo, destinations), changeSPK, network) =>
        val fee = SatoshisPerVirtualByte(Satoshis(1000))
        val builder = BitcoinTxBuilder(destinations,
                                       creditingTxsInfo,
                                       fee,
                                       changeSPK._1,
                                       network)
        val txF = builder.flatMap(_.sign)

        txF.map { tx =>
          assert(BitcoinScriptUtil.verifyScript(tx, creditingTxsInfo.toVector))
        }
    }
  }

  it must "sign a mix of p2sh/p2wsh in a tx and then have it verified" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs(CreditingTxGen.nestedOutputs),
                ScriptGenerators.scriptPubKey,
                ChainParamsGenerator.bitcoinNetworkParams) {
      case ((creditingTxsInfo, destinations), changeSPK, network) =>
        val fee = SatoshisPerByte(Satoshis(1000))
        val builder = BitcoinTxBuilder(destinations,
                                       creditingTxsInfo,
                                       fee,
                                       changeSPK._1,
                                       network)
        val txF = builder.flatMap(_.sign)

        txF.map { tx =>
          assert(BitcoinScriptUtil.verifyScript(tx, creditingTxsInfo.toVector))
        }
    }
  }
}
