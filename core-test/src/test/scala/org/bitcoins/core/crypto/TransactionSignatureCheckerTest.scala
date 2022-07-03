package org.bitcoins.core.crypto

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.crypto.{ECDigitalSignature, ECPublicKey, ECPublicKeyBytes}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 2/29/16.
  */
class TransactionSignatureCheckerTest extends BitcoinSUnitTest {

  behavior of "TransactionSignatureChecker"

  // Positive Test Cases

  // first tx from satoshi to hal :)
  // f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16 on mainnet
  val p2pkTx: Transaction = Transaction(
    "0100000001c997a5e56e104102fa209c6a852dd90660a20b2d9c352423edce25857fcd37040000000000ffffffff0200ca9a3b00000000434104ae1a62fe09c5f51b13905f07f06b99a2f7159b2225f374cd378d71302fa28414e7aab37397f554a7df5f142c21c1b7303b8a0626f1baded5c72a704f7e6cd84cac00286bee0000000043410411db93e1dcdb8a016b49840f8c53bc1eb68a382e97b1482ecad7b148a6909a5cb2e0eaddfb84ccf9744464f82e160bfa9b8b64f9d4c03f999b8643f656b412a3ac00000000")

  val p2pkOutput: TransactionOutput = TransactionOutput(
    "00f2052a0100000043410411db93e1dcdb8a016b49840f8c53bc1eb68a382e97b1482ecad7b148a6909a5cb2e0eaddfb84ccf9744464f82e160bfa9b8b64f9d4c03f999b8643f656b412a3ac")

  val p2pkPubKey: ECPublicKeyBytes = ECPublicKeyBytes(
    "0311db93e1dcdb8a016b49840f8c53bc1eb68a382e97b1482ecad7b148a6909a5c")

  val p2pkSig: ECDigitalSignature = ECDigitalSignature(
    "304402204e45e16932b8af514961a1d3a1a25fdf3f4f7732e9d624c6c61548ab5fb8cd410220181522ec8eca07de4860a4acdd12909d831cc56cbbac4622082221a8768d1d0901")

  it must "validate a P2PK sig" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2pkTx,
                         inputIndex = UInt32.zero,
                         output = p2pkOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2pkPubKey,
                                                 p2pkSig)

    assert(result.isValid, s"result: $result")
  }

  // 66f48fa8ef5db20a3b4be6b13f024b6e23480fd83df26ffbe7449110b113a665 on testnet
  val p2pkhTx: Transaction = Transaction(
    "0100000001b8a1278696acfa85f1f576836aa30d335207b69bdaff43d9464cc1db40fe19ae0000000000feffffff02a0860100000000001976a914775bd9c79a9e988c0d6177a9205a611a50b7229188acb6342900000000001976a914f23a46f930320ab3cc7ad8c1660325f4c434d11688ac63b70d00")

  val p2pkhOutput: TransactionOutput = TransactionOutput(
    "00000000000000001976a914cd0385f813ec73f8fc340b7069daf566878a0d6b88ac")

  val p2pkhPubKey: ECPublicKeyBytes = ECPublicKeyBytes(
    "02a01aaa27b468ec3fb2ae0c2a9fa1d5dce9b79b35062178f479156d8daa6c0e50")

  val p2pkhSig: ECDigitalSignature = ECDigitalSignature(
    "3044022075b4ab08ff34799ee6f8048a5044be98dff493fc5a0b8a36dcaee3bd7a9993ae02207bc532ceab09c10f1d54035d03ff9aad0e1004c3e0325a8b97b6be04b7d6c3a201")

  it must "validate a P2PKH sig" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2pkhTx,
                         inputIndex = UInt32.zero,
                         output = p2pkhOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2pkhPubKey,
                                                 p2pkhSig)
    assert(result.isValid, s"result: $result")
  }

  // aed79e83630b092dd4888760e322d27cea80b33b877b9ea2563bccfe37c31ff3 on testnet
  val p2shMultiTx: Transaction = Transaction(
    "0100000001d96b47c1dad0f0845529abed89dae027df4bc8d2332125fc2d8e32f173cb17ca01000000da004830450221008c6feca23958570d87e5dfb3c30a4ee3f07ef808bbafb21d0bf99ac62122498d022067c9b99dde72840d070a1a6e8527810d4ff3d6567c0177419f97210d8fa272470147304402201540a3467e3d04f149c23917630784e9ceafdf53c8557123df6fc3738ae9aacb022047fd2ac7ba925914bad08c951cad86b300bc9a49f6a1eca777686ed58be47bb90147522102895a52495c4c370d50e6bef622ff28d87eec2df00c546b8921b6d07e844bfb9c210283fe2cf10b7dba0d635b3e408532183e27cd43adc11e125027107c095a2bfbc552aefcffffff030000000000000000166a146f6d6e69000000000000001f0000015b9dbc1200041d471a0000000017a9146e78f4b07eec1ae32ac26bf449a7dede47f2b5aa8722020000000000001976a91499f8677f2465513d4d7fe77cac7f4c184c3d84dd88ac00000000")

  val p2shMultiOutput: TransactionOutput = TransactionOutput(
    "96b3471a0000000017a9146e78f4b07eec1ae32ac26bf449a7dede47f2b5aa87")

  val p2shMultiRedeemScript: MultiSignatureScriptPubKey =
    MultiSignatureScriptPubKey(
      "47522102895a52495c4c370d50e6bef622ff28d87eec2df00c546b8921b6d07e844bfb9c210283fe2cf10b7dba0d635b3e408532183e27cd43adc11e125027107c095a2bfbc552ae")

  val p2shMultiPubKey1: ECPublicKeyBytes = ECPublicKeyBytes(
    "02895a52495c4c370d50e6bef622ff28d87eec2df00c546b8921b6d07e844bfb9c")

  val p2shMultiPubKey2: ECPublicKeyBytes = ECPublicKeyBytes(
    "0283fe2cf10b7dba0d635b3e408532183e27cd43adc11e125027107c095a2bfbc5")

  val p2shMultiSig1: ECDigitalSignature = ECDigitalSignature(
    "30450221008c6feca23958570d87e5dfb3c30a4ee3f07ef808bbafb21d0bf99ac62122498d022067c9b99dde72840d070a1a6e8527810d4ff3d6567c0177419f97210d8fa2724701")

  val p2shMultiSig2: ECDigitalSignature = ECDigitalSignature(
    "304402201540a3467e3d04f149c23917630784e9ceafdf53c8557123df6fc3738ae9aacb022047fd2ac7ba925914bad08c951cad86b300bc9a49f6a1eca777686ed58be47bb901")

  it must "validate P2SH(Multi) signatures" in {
    val txSignatureComponent =
      P2SHTxSigComponent(transaction = p2shMultiTx,
                         inputIndex = UInt32.zero,
                         output = p2shMultiOutput,
                         Policy.standardFlags)

    val result1 =
      TransactionSignatureChecker.checkSignature(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        p2shMultiPubKey1,
        p2shMultiSig1,
        Policy.standardFlags)

    assert(result1.isValid, s"result: $result1")

    val result2 =
      TransactionSignatureChecker.checkSignature(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        p2shMultiPubKey2,
        p2shMultiSig2,
        Policy.standardFlags)

    assert(result2.isValid, s"result: $result2")

    val result3 =
      TransactionSignatureChecker.multiSignatureEvaluator(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        List(p2shMultiSig1, p2shMultiSig2),
        List(p2shMultiPubKey1, p2shMultiPubKey2),
        Policy.standardFlags,
        2
      )

    assert(result3.isValid, s"result: $result3")
  }

  // de8ed7b2dd4c354ef89ef8aab8f5f19fc6e0a2a2f4559c35c4842f499b31e94e on testnet
  val p2wpkhTx: WitnessTransaction = WitnessTransaction(
    "02000000000101ef710d693621082a77338b9c74260f01409120be7fb2a053f12d23c714adb16c01000000000000000001e803000000000000160014d931a218e08806bec7faa00157a26237c08d4101020021023b19cda60171c1660e95d9e42103d7c86a11a48e396271515d704e755b2bb2d200000000")

  val p2wpkhOutput: TransactionOutput = TransactionOutput(
    "1027000000000000160014c5aff982b31b4c0f12978c56b37f6a814454df9d")

  val p2wpkhPubKey: ECPublicKey = ECPublicKey(
    "023b19cda60171c1660e95d9e42103d7c86a11a48e396271515d704e755b2bb2d2")

  val p2wpkhSig: ECDigitalSignature = ECDigitalSignature(
    "3045022100d7d5fabac9b8486f31b2a7c1217335fc24aa0aa1ff8abd3d78845bc2f8ff3bd3022018b9c79581c2bc2f7811f235fa4c21b23c6a2b478a46351c955adaba15acc4e201")

  it must "validate a P2WPKH sig" in {
    val txSignatureComponent =
      WitnessTxSigComponentRaw(transaction = p2wpkhTx,
                               inputIndex = UInt32.zero,
                               output = p2wpkhOutput,
                               Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2wpkhPubKey,
                                                 p2wpkhSig)

    assert(result.isValid, s"result: $result")
  }

  // df8d2ddcbe81b340b117d54efb4f11753c7769745ced9af38562ad5d28ef1b1c on testnet
  val p2shwpkhTx: WitnessTransaction = WitnessTransaction(
    "020000000001012d84c922aafb935a95d0adcfd323cd127fcd5a70987013ae631470a3d09d7d81010000001716001422fe81d0b50f7a6407d4280e1603c10f5fc3fac8feffffff023062e3a00100000017a9143bdd9f87d2c1cb89ef720c0ad627262917a12ded8762831c000000000017a9144e36d1ac7c773f18b4b2ab4b88cb7fcad3d91abd870247304402207f27a20acc1e7ccae2d1fa6708fc989c5744e6a35d7b437a995fb8fa0cfbff5a02204f0a2a8879b8eccb580f8a1e0b6ed0ccc175b2ec471a1183a066f740c3dd21470121029ca7faef43714b34508589f31394e0b0b6ab24dd4e440eaa03e72841d48e50c5ed381a00")

  val p2shwpkhOutput: TransactionOutput = TransactionOutput(
    "202d00a10100000017a914aa29eed6460a03219e92a3ff54d801a7473f2a7f87")

  val p2shwpkhRedeemScript: ScriptPubKey =
    ScriptPubKey("16001422fe81d0b50f7a6407d4280e1603c10f5fc3fac8")

  val p2shwpkhPubKey: ECPublicKeyBytes = ECPublicKeyBytes(
    "029ca7faef43714b34508589f31394e0b0b6ab24dd4e440eaa03e72841d48e50c5")

  val p2shwpkhSig: ECDigitalSignature = ECDigitalSignature(
    "304402207f27a20acc1e7ccae2d1fa6708fc989c5744e6a35d7b437a995fb8fa0cfbff5a02204f0a2a8879b8eccb580f8a1e0b6ed0ccc175b2ec471a1183a066f740c3dd214701")

  it must "validate a P2SH(P2WPKH) sig" in {
    val txSignatureComponent =
      WitnessTxSigComponentP2SH(transaction = p2shwpkhTx,
                                inputIndex = UInt32.zero,
                                output = p2shwpkhOutput,
                                Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(
        txSignatureComponent,
        p2shwpkhRedeemScript.asm.toList,
        p2shwpkhPubKey,
        p2shwpkhSig,
        Policy.standardFlags)

    assert(result.isValid, s"result: $result")
  }

  // d7b77277cc38151606c604010b5cbc9a9e0e06e8b59d9f891d5cda0bb101ab39 on mainnet
  val p2wshTx: WitnessTransaction = WitnessTransaction(
    "01000000000101ed91ad279e27cea42e12cc5fbda6a3b616fe0c6e4ac2a8ec79e33a3809fc2cbd0200000000ffffffff02c0cf6a000000000017a91467d1a505334bf8cb2e1090811ecc16b339de4f4087f2207a7100000000220020701a8d401c84fb13e6baf169d59684e17abd9fa216c8cc5b9fc63d622ff8c58d040047304402207836deb67f9fe8209af34c5c83cb614f0ab9482afeab3ab44ed37dec74eb058d022001421e40e4ae478685b0845420f33621a42aded5389e72637a07a6cfaaebd8f10147304402202046a8e34daf0126ad23ca76460ad9074b373995babef5288edcccaafa052fbe0220381ebf98806a7bf6e5968e6c4374362b79f81b9ed9ed2b24ec743d3272b7a499016952210375e00eb72e29da82b89367947f29ef34afb75e8654f6ea368e0acdfd92976b7c2103a1b26313f430c4b15bb1fdce663207659d8cac749a0e53d70eff01874496feff2103c96d495bfdd5ba4145e3e046fee45e84a8a48ad05bd8dbb395c011a32cf9f88053ae00000000")

  val p2wshOutput: TransactionOutput = TransactionOutput(
    "12dbe57100000000220020701a8d401c84fb13e6baf169d59684e17abd9fa216c8cc5b9fc63d622ff8c58d")

  val p2wshWitScript: MultiSignatureScriptPubKey =
    MultiSignatureScriptPubKey(
      "6952210375e00eb72e29da82b89367947f29ef34afb75e8654f6ea368e0acdfd92976b7c2103a1b26313f430c4b15bb1fdce663207659d8cac749a0e53d70eff01874496feff2103c96d495bfdd5ba4145e3e046fee45e84a8a48ad05bd8dbb395c011a32cf9f88053ae")

  val p2wshPubKey1: ECPublicKeyBytes = ECPublicKeyBytes(
    "0375e00eb72e29da82b89367947f29ef34afb75e8654f6ea368e0acdfd92976b7c")

  val p2wshPubKey2: ECPublicKeyBytes = ECPublicKeyBytes(
    "03a1b26313f430c4b15bb1fdce663207659d8cac749a0e53d70eff01874496feff")

  val p2wshSig1: ECDigitalSignature = ECDigitalSignature(
    "304402207836deb67f9fe8209af34c5c83cb614f0ab9482afeab3ab44ed37dec74eb058d022001421e40e4ae478685b0845420f33621a42aded5389e72637a07a6cfaaebd8f101")

  val p2wshSig2: ECDigitalSignature = ECDigitalSignature(
    "304402202046a8e34daf0126ad23ca76460ad9074b373995babef5288edcccaafa052fbe0220381ebf98806a7bf6e5968e6c4374362b79f81b9ed9ed2b24ec743d3272b7a49901")

  it must "validate P2WSH(Multisig) signatures" in {
    val txSignatureComponent =
      WitnessTxSigComponent(transaction = p2wshTx,
                            inputIndex = UInt32.zero,
                            output = p2wshOutput,
                            Map.empty,
                            Policy.standardFlags)

    val result1 =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2wshWitScript.asm.toList,
                                                 p2wshPubKey1,
                                                 p2wshSig1,
                                                 Policy.standardFlags)

    assert(result1.isValid, s"result: $result1")

    val result2 =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2wshWitScript.asm.toList,
                                                 p2wshPubKey2,
                                                 p2wshSig2,
                                                 Policy.standardFlags)

    assert(result2.isValid, s"result: $result2")

    val result3 =
      TransactionSignatureChecker.multiSignatureEvaluator(
        txSignatureComponent,
        p2wshWitScript.asm.toList,
        List(p2wshSig1, p2wshSig2),
        List(p2wshPubKey1, p2wshPubKey2),
        Policy.standardFlags,
        2)

    assert(result3.isValid, s"result: $result3")
  }

  // Negative Test Cases
  val incorrectPubKey: ECPublicKeyBytes = ECPublicKeyBytes.freshPublicKey
  val incorrectOutput: EmptyTransactionOutput.type = EmptyTransactionOutput
  val incorrectTx: Transaction = EmptyTransaction
  val incorrectInputIndex: UInt32 = UInt32(100)
  val incorrectScript: Seq[ScriptToken] = Seq.empty

  it must "fail a P2PK sig with the wrong pubkey" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2pkTx,
                         inputIndex = UInt32.zero,
                         output = p2pkOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 incorrectPubKey,
                                                 p2pkSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2PK sig with the wrong output" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2pkTx,
                         inputIndex = UInt32.zero,
                         output = incorrectOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2pkPubKey,
                                                 p2pkSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2PK sig with the wrong tx" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = incorrectTx,
                         inputIndex = UInt32.zero,
                         output = p2pkOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2pkPubKey,
                                                 p2pkSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2PK sig with the wrong input index" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2pkTx,
                         inputIndex = incorrectInputIndex,
                         output = p2pkOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2pkPubKey,
                                                 p2pkSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2PKH sig with the wrong pubkey" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2pkhTx,
                         inputIndex = UInt32.zero,
                         output = p2pkhOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 incorrectPubKey,
                                                 p2pkhSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2PKH sig with the wrong output" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2pkhTx,
                         inputIndex = UInt32.zero,
                         output = incorrectOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2pkhPubKey,
                                                 p2pkhSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2PKH sig with the wrong tx" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = incorrectTx,
                         inputIndex = UInt32.zero,
                         output = p2pkhOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2pkhPubKey,
                                                 p2pkhSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2PKH sig with the wrong input index" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2pkhTx,
                         inputIndex = incorrectInputIndex,
                         output = p2pkhOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2pkhPubKey,
                                                 p2pkhSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2SH(Multi) with the wrong pub key 1" in {
    val txSignatureComponent =
      P2SHTxSigComponent(transaction = p2shMultiTx,
                         inputIndex = UInt32.zero,
                         output = p2shMultiOutput,
                         Policy.standardFlags)

    val result1 =
      TransactionSignatureChecker.checkSignature(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        incorrectPubKey,
        p2shMultiSig1,
        Policy.standardFlags)

    assert(!result1.isValid, s"result: $result1")

    val result2 =
      TransactionSignatureChecker.checkSignature(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        p2shMultiPubKey2,
        p2shMultiSig2,
        Policy.standardFlags)

    assert(result2.isValid, s"result: $result2")

    val result3 =
      TransactionSignatureChecker.multiSignatureEvaluator(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        List(p2shMultiSig1, p2shMultiSig2),
        List(incorrectPubKey, p2shMultiPubKey2),
        Policy.standardFlags,
        2
      )

    assert(!result3.isValid, s"result: $result3")
  }

  it must "fail a P2SH(Multi) with the wrong pub key 2" in {
    val txSignatureComponent =
      P2SHTxSigComponent(transaction = p2shMultiTx,
                         inputIndex = UInt32.zero,
                         output = p2shMultiOutput,
                         Policy.standardFlags)

    val result1 =
      TransactionSignatureChecker.checkSignature(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        p2shMultiPubKey1,
        p2shMultiSig1,
        Policy.standardFlags)

    assert(result1.isValid, s"result: $result1")

    val result2 =
      TransactionSignatureChecker.checkSignature(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        incorrectPubKey,
        p2shMultiSig2,
        Policy.standardFlags)

    assert(!result2.isValid, s"result: $result2")

    val result3 =
      TransactionSignatureChecker.multiSignatureEvaluator(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        List(p2shMultiSig1, p2shMultiSig2),
        List(p2shMultiPubKey1, incorrectPubKey),
        Policy.standardFlags,
        2
      )

    assert(!result3.isValid, s"result: $result3")
  }

  it must "fail a P2SH(Multi) with the wrong number of sigs" in {
    val txSignatureComponent =
      P2SHTxSigComponent(transaction = p2shMultiTx,
                         inputIndex = UInt32.zero,
                         output = p2shMultiOutput,
                         Policy.standardFlags)

    assertThrows[IllegalArgumentException](
      TransactionSignatureChecker.multiSignatureEvaluator(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        List(p2shMultiSig1, p2shMultiSig2),
        List(p2shMultiPubKey1, p2shMultiPubKey2),
        Policy.standardFlags,
        1
      ))

    assertThrows[IllegalArgumentException](
      TransactionSignatureChecker.multiSignatureEvaluator(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        List(p2shMultiSig1, p2shMultiSig2),
        List(p2shMultiPubKey1, p2shMultiPubKey2),
        Policy.standardFlags,
        -1
      ))
  }

  it must "fail a P2WPKH sig with the wrong pubkey" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2wpkhTx,
                         inputIndex = UInt32.zero,
                         output = p2wpkhOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 incorrectPubKey,
                                                 p2wpkhSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2WPKH sig with the wrong output" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2wpkhTx,
                         inputIndex = UInt32.zero,
                         output = incorrectOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2wpkhPubKey,
                                                 p2wpkhSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2WPKH sig with the wrong tx" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = incorrectTx,
                         inputIndex = UInt32.zero,
                         output = p2wpkhOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2wpkhPubKey,
                                                 p2wpkhSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2WPKH sig with the wrong input index" in {
    val txSignatureComponent =
      BaseTxSigComponent(transaction = p2wpkhTx,
                         inputIndex = incorrectInputIndex,
                         output = p2wpkhOutput,
                         Policy.standardFlags)

    val result =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2wpkhPubKey,
                                                 p2wpkhSig)

    assert(!result.isValid, s"result: $result")
  }

  it must "fail a P2WSH(Multi) with the wrong pub key 1" in {
    val txSignatureComponent =
      WitnessTxSigComponent(transaction = p2wshTx,
                            inputIndex = UInt32.zero,
                            output = p2wshOutput,
                            Map.empty,
                            Policy.standardFlags)

    val result1 =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2wshWitScript.asm.toList,
                                                 incorrectPubKey,
                                                 p2wshSig1,
                                                 Policy.standardFlags)

    assert(!result1.isValid, s"result: $result1")

    val result2 =
      TransactionSignatureChecker.checkSignature(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        p2wshPubKey2,
        p2wshSig2,
        Policy.standardFlags)

    assert(result2.isValid, s"result: $result2")

    val result3 =
      TransactionSignatureChecker.multiSignatureEvaluator(
        txSignatureComponent,
        p2wshWitScript.asm.toList,
        List(p2wshSig1, p2wshSig2),
        List(incorrectPubKey, p2wshPubKey2),
        Policy.standardFlags,
        2
      )

    assert(!result3.isValid, s"result: $result3")
  }

  it must "fail a P2WSH(Multi) with the wrong pub key 2" in {
    val txSignatureComponent =
      WitnessTxSigComponent(transaction = p2wshTx,
                            inputIndex = UInt32.zero,
                            output = p2wshOutput,
                            Map.empty,
                            Policy.standardFlags)

    val result1 =
      TransactionSignatureChecker.checkSignature(txSignatureComponent,
                                                 p2wshWitScript.asm.toList,
                                                 p2wshPubKey1,
                                                 p2wshSig1,
                                                 Policy.standardFlags)

    assert(result1.isValid, s"result: $result1")

    val result2 =
      TransactionSignatureChecker.checkSignature(
        txSignatureComponent,
        p2shMultiRedeemScript.asm.toList,
        incorrectPubKey,
        p2wshSig2,
        Policy.standardFlags)

    assert(!result2.isValid, s"result: $result2")

    val result3 =
      TransactionSignatureChecker.multiSignatureEvaluator(
        txSignatureComponent,
        p2wshWitScript.asm.toList,
        List(p2wshSig1, p2wshSig2),
        List(p2wshPubKey1, incorrectPubKey),
        Policy.standardFlags,
        2
      )

    assert(!result3.isValid, s"result: $result3")
  }

}
