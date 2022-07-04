package org.bitcoins.core.crypto

import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.wallet.builder.StandardNonInteractiveFinalizer
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{ECSignatureParams, P2PKHInputInfo}
import org.bitcoins.crypto.{
  ECDigitalSignature,
  ECPrivateKey,
  EmptyDigitalSignature,
  HashType
}
import org.bitcoins.testkitcore.util.TransactionTestUtil
import org.bitcoins.testkitcore.gen.{CreditingTxGen, ScriptGenerators}
import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import scodec.bits.ByteVector

import scala.annotation.nowarn
import scala.concurrent.Future

/** Created by chris on 7/21/16.
  */
class TransactionSignatureCreatorTest extends BitcoinSJvmTest {

  "TransactionSignatureCreator" must "create a signature for a scriptSignature in a transaction" in {
    //this is a signed tx, but since TransactionSignatureSerializer removes scriptSigs, it will work for testing this
    //from fe6ef8e20a9ca9cb5d59cb1c0f30eff2b23be2e3cc2bf4b4cfff519414e9a300 on testnet
    //"30440220357864ae2beba3d6ec34c0ce42262c1c12939502f0f8f4bd338c9d8b307593420220656687c327589dc3e464700fa7b784c7efc2b465c627a60c2f1ce402d05fc39d01"
    val expectedSig = ECDigitalSignature(
      "30440220357864ae2beba3d6ec34c0ce42262c1c12939502f0f8f4bd338c9d8b307593420220656687c327589dc3e464700fa7b784c7efc2b465c627a60c2f1ce402d05fc39d01")
    val rawTx =
      "01000000021d50bf7c05b6169ea8d8fb5b79dd2978bbd2ac756a656a777279da43b19fd9d9000000006b4830450221008f2c818a55045a1c9dcda54fcd5b6377f5d09723a9ccd8c71df76ee4bdf7c16802201817cbd71d8148a5d53b11d33c9c58ad1086fe7ddf308da2a7cceb7d85df293e01210381c82dc267a958be06f1c920dc635bcd191d698c167e67a45a882a551c57ce1dfeffffffd4a6a37abfe003a9d10155df215e662f88d5b878b908d1a3772a9fbd195d008d010000006a4730440220357864ae2beba3d6ec34c0ce42262c1c12939502f0f8f4bd338c9d8b307593420220656687c327589dc3e464700fa7b784c7efc2b465c627a60c2f1ce402d05fc39d0121036301d848aec3dfc47789a63ee3c85c6d3bf757162ef77cb1580981b422838ed7feffffff0200e1f505000000001976a9146d39bac171d0bf450698fa0ebd93f51e79dcb6ac88ac35a96d00000000001976a914e11753f499ac7a910148e53156ab273557ed517e88acd6090b00"
    val transaction = Transaction(rawTx)
    /*val prevTransaction = Transaction(
      "01000000018b7dde71a52d40b9f3a03e604eceda72e26e9be2ce17a09ae540d15eddac6d36000000006a473044022069d7be9fcdbc846ff8b24fedf6aeabb1619e3087b4e18b2bb0f8dc174b5a29dc022033fbcb40a9e834ebb051f4a864e00de420ed14b954897280b5f7399971a14b6d012102f81ce897b559c07724a5a52a0e4650f2f43bbf1357f0c1e4c8c238899e9d7523feffffff02dc4c6d0a000000001976a914c23317eae4fcd12cbbae7784fbabbbb6448b4e3d88ac80969800000000001976a914d7b4717a934386601ac3f980d01b48c83b8a0b4b88acf0b60a00")
     */
    val scriptPubKey =
      ScriptPubKey("1976a914d7b4717a934386601ac3f980d01b48c83b8a0b4b88ac")
    val txSignatureComponent =
      BaseTxSigComponent(transaction = transaction,
                         inputIndex = UInt32.one,
                         output =
                           TransactionOutput(10000000.sats, scriptPubKey),
                         Policy.standardScriptVerifyFlags)
    val privateKey = ECPrivateKeyUtil.fromWIFToPrivateKey(
      "cTPg4Zc5Jis2EZXy3NXShgbn487GWBTapbU63BerLDZM3w2hQSjC")
    val txSignature =
      TransactionSignatureCreator.createSig(txSignatureComponent,
                                            privateKey.toPrivateKey,
                                            HashType.sigHashAll)
    txSignature.r must be(expectedSig.r)
    txSignature.s must be(expectedSig.s)
    txSignature.hex must be(expectedSig.hex)

  }
  it must "create the correct digital signature for a transaction with 1 input using a TxSignatureComponent" in {
    //66f48fa8ef5db20a3b4be6b13f024b6e23480fd83df26ffbe7449110b113a665 on testnet
    val expectedSig = ECDigitalSignature(
      "3044022075b4ab08ff34799ee6f8048a5044be98dff493fc5a0b8a36dcaee3bd7a9993ae02207bc532ceab09c10f1d54035d03ff9aad0e1004c3e0325a8b97b6be04b7d6c3a201")
    val rawTx =
      "0100000001b8a1278696acfa85f1f576836aa30d335207b69bdaff43d9464cc1db40fe19ae000000006a473044022075b4ab08ff34799ee6f8048a5044be98dff493fc5a0b8a36dcaee3bd7a9993ae02207bc532ceab09c10f1d54035d03ff9aad0e1004c3e0325a8b97b6be04b7d6c3a2012102a01aaa27b468ec3fb2ae0c2a9fa1d5dce9b79b35062178f479156d8daa6c0e50feffffff02a0860100000000001976a914775bd9c79a9e988c0d6177a9205a611a50b7229188acb6342900000000001976a914f23a46f930320ab3cc7ad8c1660325f4c434d11688ac63b70d00"
    val transaction = Transaction(rawTx)
    /*val prevTransaction = Transaction(
      "0100000001e4dbac0d73f4e3a9e99e70596a5f81b35a75f95b0474d051fbfd9dc249a5b67e000000006a4730440220486f112aee12997f6e484754d53d5c2158c18cc6d1d3f13aefcdf0ed19c47b290220136133d934d9e79a57408166c39fbce38e217ea9d417cabc20744134f04f06960121021f8cb5c3d611cf24dd665adff3fd540e4c155a05adaa6b672bfa7897c126d9b6feffffff0293d22a00000000001976a914cd0385f813ec73f8fc340b7069daf566878a0d6b88ac40420f000000000017a91480f7a6c14a8407da3546b4abfc3086876ca9a0668700000000")
     */
    val scriptPubKey =
      ScriptPubKey("1976a914cd0385f813ec73f8fc340b7069daf566878a0d6b88ac")
    val txSignatureComponent =
      BaseTxSigComponent(transaction = transaction,
                         inputIndex = UInt32.zero,
                         output = TransactionOutput(2806419.sats, scriptPubKey),
                         Policy.standardScriptVerifyFlags)
    val privateKey = ECPrivateKeyUtil.fromWIFToPrivateKey(
      "cTTh7jNtZhg3vHTjvYK8zcHkLfsMAS8iqL7pfZ6eVAVHHF8fN1qy")
    val txSignature =
      TransactionSignatureCreator.createSig(txSignatureComponent,
                                            privateKey.toPrivateKey,
                                            HashType.sigHashAll)
    txSignature.r must be(expectedSig.r)
    txSignature.s must be(expectedSig.s)
    txSignature.hex must be(expectedSig.hex)
  }

  it must "create the correct digital signature for a transaction with 1 input" in {
    //66f48fa8ef5db20a3b4be6b13f024b6e23480fd83df26ffbe7449110b113a665 on testnet
    val expectedSig = ECDigitalSignature(
      "3044022075b4ab08ff34799ee6f8048a5044be98dff493fc5a0b8a36dcaee3bd7a9993ae02207bc532ceab09c10f1d54035d03ff9aad0e1004c3e0325a8b97b6be04b7d6c3a201")
    val rawTx =
      "0100000001b8a1278696acfa85f1f576836aa30d335207b69bdaff43d9464cc1db40fe19ae000000006a473044022075b4ab08ff34799ee6f8048a5044be98dff493fc5a0b8a36dcaee3bd7a9993ae02207bc532ceab09c10f1d54035d03ff9aad0e1004c3e0325a8b97b6be04b7d6c3a2012102a01aaa27b468ec3fb2ae0c2a9fa1d5dce9b79b35062178f479156d8daa6c0e50feffffff02a0860100000000001976a914775bd9c79a9e988c0d6177a9205a611a50b7229188acb6342900000000001976a914f23a46f930320ab3cc7ad8c1660325f4c434d11688ac63b70d00"
    val transaction = Transaction(rawTx)
    val prevTransaction = Transaction(
      "0100000001e4dbac0d73f4e3a9e99e70596a5f81b35a75f95b0474d051fbfd9dc249a5b67e000000006a4730440220486f112aee12997f6e484754d53d5c2158c18cc6d1d3f13aefcdf0ed19c47b290220136133d934d9e79a57408166c39fbce38e217ea9d417cabc20744134f04f06960121021f8cb5c3d611cf24dd665adff3fd540e4c155a05adaa6b672bfa7897c126d9b6feffffff0293d22a00000000001976a914cd0385f813ec73f8fc340b7069daf566878a0d6b88ac40420f000000000017a91480f7a6c14a8407da3546b4abfc3086876ca9a0668700000000")
    val privateKey = ECPrivateKeyUtil
      .fromWIFToPrivateKey(
        "cTTh7jNtZhg3vHTjvYK8zcHkLfsMAS8iqL7pfZ6eVAVHHF8fN1qy")
      .toPrivateKey

    val inputInfo =
      P2PKHInputInfo(TransactionOutPoint(prevTransaction.txId, UInt32.zero),
                     2806419.sats,
                     privateKey.publicKey)
    val signingInfo = ECSignatureParams(inputInfo,
                                        prevTransaction,
                                        privateKey,
                                        HashType.sigHashAll)

    val txSignature =
      TransactionSignatureCreator.createSig(transaction,
                                            signingInfo,
                                            privateKey,
                                            HashType.sigHashAll)
    txSignature.r must be(expectedSig.r)
    txSignature.s must be(expectedSig.s)
    txSignature.hex must be(expectedSig.hex)
  }

  it should "have old and new createSig functions agree" in {
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

              val oldSig =
                TransactionSignatureCreator.createSig(txSignatureComponent,
                                                      signer.sign(_),
                                                      signInfo.hashType)

              val newSig =
                TransactionSignatureCreator.createSig(spendingTx,
                                                      signInfo,
                                                      signer.sign(_),
                                                      signInfo.hashType)

              (oldSig.r == newSig.r) &&
              (oldSig.s == newSig.s) &&
              (oldSig.hex == newSig.hex)
            }
          }

        assert(correctSigs.forall(_ == true))
    }
  }

  it must "create a p2pk scriptPubKey, create a crediting tx for scriptPubKey, " +
    "then create spending tx and make sure it evaluates to true in the interpreter" in {
      val privateKey = ECPrivateKey()
      val publicKey = privateKey.publicKey
      val scriptPubKey = P2PKScriptPubKey(publicKey)
      val (creditingTx, outputIndex) =
        TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
      val scriptSig = EmptyScriptSignature
      val (spendingTx, inputIndex) =
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     scriptSig,
                                                     outputIndex)

      val txSignatureComponent =
        BaseTxSigComponent(
          transaction = spendingTx,
          inputIndex = inputIndex,
          output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
          flags = Policy.standardScriptVerifyFlags)

      val txSignature =
        TransactionSignatureCreator.createSig(txSignatureComponent,
                                              privateKey,
                                              HashType.sigHashAll)

      //add the signature to the scriptSig instead of having an empty scriptSig
      val signedScriptSig = P2PKScriptSignature(txSignature)
      val (signedTx, _) =
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     signedScriptSig,
                                                     outputIndex)

      val signedTxSigComponent =
        BaseTxSigComponent(
          transaction = signedTx,
          inputIndex = inputIndex,
          output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
          flags = Policy.standardScriptVerifyFlags)

      //run it through the interpreter
      val program = PreExecutionScriptProgram(signedTxSigComponent)

      val result = ScriptInterpreter.run(program)

      result must be(ScriptOk)
    }

  it must "create a p2pkh scriptPubKey, create a crediting tx for the scriptPubkey" +
    "then create a spending tx and make sure it evaluates to true in the interpreter" in {
      val privateKey = ECPrivateKey()
      val publicKey = privateKey.publicKey
      val scriptPubKey = P2PKHScriptPubKey(publicKey)
      val (creditingTx, outputIndex) =
        TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
      val scriptSig = EmptyScriptSignature
      val (spendingTx, inputIndex) =
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     scriptSig,
                                                     outputIndex)
      val txSignatureComponent =
        BaseTxSigComponent(
          transaction = spendingTx,
          inputIndex = inputIndex,
          output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
          Policy.standardScriptVerifyFlags)
      val txSignature =
        TransactionSignatureCreator.createSig(txSignatureComponent,
                                              privateKey,
                                              HashType.sigHashAll)

      //add the signature to the scriptSig instead of having an empty scriptSig
      val signedScriptSig = P2PKHScriptSignature(txSignature, publicKey)
      val (signedTx, _) =
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     signedScriptSig,
                                                     outputIndex)

      //run it through the interpreter
      val signedTxSigComponent =
        BaseTxSigComponent(
          transaction = signedTx,
          inputIndex = inputIndex,
          output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
          Policy.standardScriptVerifyFlags)
      val program = PreExecutionScriptProgram(signedTxSigComponent)
      val result = ScriptInterpreter.run(program)

      result must be(ScriptOk)
    }

  it must "create a multisignature scriptPubKey, create a crediting tx for the scriptPubkey, " +
    "then create a spending tx and make sure it evaluates to true in the interpreter" in {
      val privateKey = ECPrivateKey()
      val publicKey = privateKey.publicKey
      val scriptPubKey = MultiSignatureScriptPubKey(1, Seq(publicKey))
      val (creditingTx, outputIndex) =
        TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
      val scriptSig = MultiSignatureScriptSignature(Seq(EmptyDigitalSignature))
      val (spendingTx, inputIndex) =
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     scriptSig,
                                                     outputIndex)
      val txSignatureComponent =
        BaseTxSigComponent(
          transaction = spendingTx,
          inputIndex = inputIndex,
          output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
          Policy.standardScriptVerifyFlags)
      val txSignature =
        TransactionSignatureCreator.createSig(txSignatureComponent,
                                              privateKey,
                                              HashType.sigHashAll)

      //add the signature to the scriptSig instead of having an empty scriptSig
      val signedScriptSig = MultiSignatureScriptSignature(Seq(txSignature))
      val (signedTx, _) =
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     signedScriptSig,
                                                     outputIndex)

      val signedTxSigComponent =
        BaseTxSigComponent(
          transaction = signedTx,
          inputIndex = inputIndex,
          output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
          Policy.standardScriptVerifyFlags)
      //run it through the interpreter
      val program = PreExecutionScriptProgram(signedTxSigComponent)

      val result = ScriptInterpreter.run(program)

      result must be(ScriptOk)
    }

  it must "create a p2sh scriptPubKey, create a crediting tx for the scriptPubKey, " +
    "then create a spending tx and make sure it evaluates to true in the interpreter" in {
      val privateKey = ECPrivateKey()
      val publicKey = privateKey.publicKey
      val redeemScript = MultiSignatureScriptPubKey(1, Seq(publicKey))
      val scriptPubKey = P2SHScriptPubKey(redeemScript)
      val (creditingTx, outputIndex) =
        TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
      val scriptSig = MultiSignatureScriptSignature(Seq(EmptyDigitalSignature))

      val (spendingTx, inputIndex) =
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     scriptSig,
                                                     outputIndex)
      val txSignatureComponent =
        BaseTxSigComponent(
          transaction = spendingTx,
          inputIndex = inputIndex,
          output = TransactionOutput(CurrencyUnits.zero, redeemScript),
          Policy.standardScriptVerifyFlags)
      val txSignature =
        TransactionSignatureCreator.createSig(txSignatureComponent,
                                              privateKey,
                                              HashType.sigHashAll)

      val signedScriptSig = MultiSignatureScriptSignature(Seq(txSignature))
      val p2shScriptSig = P2SHScriptSignature(signedScriptSig, redeemScript)
      val (signedTx, _) =
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     p2shScriptSig,
                                                     outputIndex)

      val signedTxSigComponent =
        BaseTxSigComponent(
          transaction = signedTx,
          inputIndex = inputIndex,
          output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
          Policy.standardScriptVerifyFlags)
      //run it through the interpreter
      val program = PreExecutionScriptProgram(signedTxSigComponent)
      val result = ScriptInterpreter.run(program)
      result must be(ScriptOk)
    }

  it must "be able to use a sign function that returns a Future[ECDigitalSignature] and have the sig validate" in {
    val privateKey = ECPrivateKey()
    val publicKey = privateKey.publicKey
    val redeemScript = MultiSignatureScriptPubKey(1, Seq(publicKey))
    val scriptPubKey = P2SHScriptPubKey(redeemScript)
    val (creditingTx, outputIndex) =
      TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
    val scriptSig = MultiSignatureScriptSignature(Seq(EmptyDigitalSignature))

    val (spendingTx, inputIndex) =
      TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                   scriptSig,
                                                   outputIndex)
    val txSignatureComponent =
      BaseTxSigComponent(transaction = spendingTx,
                         inputIndex = inputIndex,
                         output =
                           TransactionOutput(CurrencyUnits.zero, redeemScript),
                         flags = Policy.standardScriptVerifyFlags)
    val sign: ByteVector => Future[ECDigitalSignature] = { bytes: ByteVector =>
      Future(privateKey.sign(bytes))
    }
    @nowarn val txSignature =
      TransactionSignatureCreator.createSig(txSignatureComponent,
                                            sign,
                                            HashType.sigHashAll)

    val signedScriptSig =
      txSignature.map(sig => MultiSignatureScriptSignature(Seq(sig)))
    val p2shScriptSig =
      signedScriptSig.map(ss => P2SHScriptSignature(ss, redeemScript))
    val signedTxFuture: Future[(Transaction, UInt32)] = p2shScriptSig.map {
      ss =>
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     ss,
                                                     outputIndex)
    }
    //run it through the interpreter
    val program = signedTxFuture.map { case (tx, _) =>
      val signedTxSigComponent = BaseTxSigComponent(
        transaction = tx,
        inputIndex = inputIndex,
        output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
        Policy.standardScriptVerifyFlags)
      PreExecutionScriptProgram(signedTxSigComponent)
    }

    val resultF = program.map(ScriptInterpreter.run(_))
    resultF.map { r =>
      assert(r == ScriptOk)
    }
  }

}
