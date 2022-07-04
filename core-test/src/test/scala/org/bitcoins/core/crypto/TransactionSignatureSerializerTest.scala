package org.bitcoins.core.crypto

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util._
import org.bitcoins.core.wallet.builder.StandardNonInteractiveFinalizer
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.testkitcore.gen.{CreditingTxGen, ScriptGenerators}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

import scala.util.Try

/** Created by chris on 2/19/16.
  */
class TransactionSignatureSerializerTest extends BitcoinSUnitTest {

  "TransactionSignatureSerializer" must "correctly serialize an input that is being checked where another input in the same tx is using SIGHASH_ANYONECANPAY" in {
    //this is from a test case inside of tx_valid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L91
    val rawTx =
      "01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df101010000000002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000"
    val inputIndex = UInt32.zero
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString(
      "0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)

    val txSigComponent =
      BaseTxSigComponent(spendingTx,
                         inputIndex,
                         TransactionOutput(CurrencyUnits.zero, scriptPubKey),
                         Policy.standardFlags)
    val serializedTxForSig: String = BytesUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = txSigComponent,
        hashType = HashType.sigHashAll,
        taprootOptions = TaprootSerializationOptions.empty))

    //serialization is from bitcoin core
    serializedTxForSig must be(
      "01000000020001000000000000000000000000000000000000000000000000000000000000000000002321035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac0100000000020000000000000000000000000000000000000000000000000000000000000000000000ffffffff01010000000000000001510000000001000000")

  }

  it must "correctly serialize a tx for signing with multiple inputs using SIGHASH_SINGLE" in {
    //this is from a test case inside of tx_valid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L96
    val rawTx =
      "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000006a4730440220360d20baff382059040ba9be98947fd678fb08aab2bb0c172efa996fd8ece9b702201b4fb0de67f015c90e7ac8a193aeab486a1f587e0f54d0fb9552ef7f5ce6caec032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e804010000006b483045022100c714310be1e3a9ff1c5f7cacc65c2d8e781fc3a88ceb063c6153bf950650802102200b2d0979c76e12bb480da635f192cc8dc6f905380dd4ac1ff35a4f68f462fffd032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff3f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000006c493046022100b663499ef73273a3788dea342717c2640ac43c5a1cf862c9e09b206fcb3f6bb8022100b09972e75972d9148f2bdd462e5cb69b57c1214b88fc55ca638676c07cfc10d8032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff0380841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac80841e00000000001976a9149857cc07bed33a5cf12b9c5e0500b675d500c81188ace0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac00000000"
    val inputIndex = UInt32.zero
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString(
      "DUP HASH160 0x14 0xdcf72c4fd02f5a987cf9b02f2fabfcac3341a87d EQUALVERIFY CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)
    val txSigComponent =
      BaseTxSigComponent(spendingTx,
                         inputIndex,
                         TransactionOutput(CurrencyUnits.zero, scriptPubKey),
                         Policy.standardFlags)
    val serializedTxForSig: String = BytesUtil.encodeHex(
      TransactionSignatureSerializer
        .serializeForSignature(txSigComponent,
                               HashType.sigHashSingle,
                               taprootOptions =
                                 TaprootSerializationOptions.empty))
    //serialization is from bitcoin core
    serializedTxForSig must be(
      "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000001976a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88acffffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e8040100000000000000003f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee0100000000000000000180841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac0000000003000000")

  }

  it must "correctly serialize a tx for signing with the last input using SIGHASH_SINGLE" in {
    //this is from a test case inside of tx_valid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L96
    val rawTx =
      "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000006a4730440220360d20baff382059040ba9be98947fd678fb08aab2bb0c172efa996fd8ece9b702201b4fb0de67f015c90e7ac8a193aeab486a1f587e0f54d0fb9552ef7f5ce6caec032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e804010000006b483045022100c714310be1e3a9ff1c5f7cacc65c2d8e781fc3a88ceb063c6153bf950650802102200b2d0979c76e12bb480da635f192cc8dc6f905380dd4ac1ff35a4f68f462fffd032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff3f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000006c493046022100b663499ef73273a3788dea342717c2640ac43c5a1cf862c9e09b206fcb3f6bb8022100b09972e75972d9148f2bdd462e5cb69b57c1214b88fc55ca638676c07cfc10d8032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff0380841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac80841e00000000001976a9149857cc07bed33a5cf12b9c5e0500b675d500c81188ace0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac00000000"
    val inputIndex = UInt32(2)
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString(
      "DUP HASH160 0x14 0xdcf72c4fd02f5a987cf9b02f2fabfcac3341a87d EQUALVERIFY CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)
    val txSigComponent =
      BaseTxSigComponent(spendingTx,
                         inputIndex,
                         TransactionOutput(CurrencyUnits.zero, scriptPubKey),
                         Policy.standardFlags)
    val serializedTxForSig: String = BytesUtil.encodeHex(
      TransactionSignatureSerializer
        .serializeForSignature(txSigComponent,
                               HashType.sigHashSingle,
                               taprootOptions =
                                 TaprootSerializationOptions.empty))
    //serialization is from bitcoin core
    serializedTxForSig must be(
      "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf630000000000000000007d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e8040100000000000000003f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000001976a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88acffffffff03ffffffffffffffff00ffffffffffffffff00e0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac0000000003000000")

  }

  it must "correctly serialize a tx which spends an input that pushes using a PUSHDATA1 that is negative when read as signed" in {
    //this is from a test case inside of tx_valid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L102
    val rawTx =
      "0100000001482f7a028730a233ac9b48411a8edfb107b749e61faf7531f4257ad95d0a51c5000000008b483045022100bf0bbae9bde51ad2b222e87fbf67530fbafc25c903519a1e5dcc52a32ff5844e022028c4d9ad49b006dd59974372a54291d5764be541574bb0c4dc208ec51f80b7190141049dd4aad62741dc27d5f267f7b70682eee22e7e9c1923b9c0957bdae0b96374569b460eb8d5b40d972e8c7c0ad441de3d94c4a29864b212d56050acb980b72b2bffffffff0180969800000000001976a914e336d0017a9d28de99d16472f6ca6d5a3a8ebc9988ac00000000"
    val inputIndex = UInt32.zero
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString(
      "0x4c 0xae 0x606563686f2022553246736447566b58312b5a536e587574356542793066794778625456415675534a6c376a6a334878416945325364667657734f53474f36633338584d7439435c6e543249584967306a486956304f376e775236644546673d3d22203e20743b206f70656e73736c20656e63202d7061737320706173733a5b314a564d7751432d707269766b65792d6865785d202d64202d6165732d3235362d636263202d61202d696e207460 DROP DUP HASH160 0x14 0xbfd7436b6265aa9de506f8a994f881ff08cc2872 EQUALVERIFY CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)
    val txSigComponent =
      BaseTxSigComponent(spendingTx,
                         inputIndex,
                         TransactionOutput(CurrencyUnits.zero, scriptPubKey),
                         Policy.standardFlags)
    val serializedTxForSig: String =
      TransactionSignatureSerializer
        .serializeForSignature(txSigComponent,
                               HashType.sigHashAll,
                               taprootOptions =
                                 TaprootSerializationOptions.empty)
        .toHex
    //serialization is from bitcoin core
    serializedTxForSig must be(
      "0100000001482f7a028730a233ac9b48411a8edfb107b749e61faf7531f4257ad95d0a51c500000000ca4cae606563686f2022553246736447566b58312b5a536e587574356542793066794778625456415675534a6c376a6a334878416945325364667657734f53474f36633338584d7439435c6e543249584967306a486956304f376e775236644546673d3d22203e20743b206f70656e73736c20656e63202d7061737320706173733a5b314a564d7751432d707269766b65792d6865785d202d64202d6165732d3235362d636263202d61202d696e2074607576a914bfd7436b6265aa9de506f8a994f881ff08cc287288acffffffff0180969800000000001976a914e336d0017a9d28de99d16472f6ca6d5a3a8ebc9988ac0000000001000000")
  }

  it must "correctly hash a tx with one input SIGHASH_ALL and one SIGHASH_ANYONECANPAY, but we set the _ANYONECANPAY sequence number, invalidating the SIGHASH_ALL signature" in {
    //this is from a test case inside of tx_invalid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_invalid.json#L75
    val rawTx =
      "01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df10101000000000200000000000000000000000000000000000000000000000000000000000000000000484730440220201dc2d030e380e8f9cfb41b442d930fa5a685bb2c8db5906671f865507d0670022018d9e7a8d4c8d86a73c2a724ee38ef983ec249827e0e464841735955c707ece98101000000010100000000000000015100000000"
    val inputIndex = UInt32.zero
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString(
      "0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)
    val txSigComponent =
      BaseTxSigComponent(spendingTx,
                         inputIndex,
                         TransactionOutput(CurrencyUnits.zero, scriptPubKey),
                         Policy.standardFlags)
    val serializedTxForSig: String = TransactionSignatureSerializer
      .serializeForSignature(txSigComponent,
                             HashType.sigHashAll,
                             taprootOptions = TaprootSerializationOptions.empty)
      .toHex
    serializedTxForSig must be(
      "01000000020001000000000000000000000000000000000000000000000000000000000000000000002321035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac01000000000200000000000000000000000000000000000000000000000000000000000000000000000100000001010000000000000001510000000001000000")

  }

  it must "serialize a p2wpkh with SIGHASH_SINGLE|SIGHASH_ANYONECANPAY" in {
    val rawTx =
      "0100000000010400010000000000000000000000000000000000000000000000000000000000000200000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000300000000ffffffff05540b0000000000000151d0070000000000000151840300000000000001513c0f00000000000001512c010000000000000151000248304502210092f4777a0f17bf5aeb8ae768dec5f2c14feabf9d1fe2c89c78dfed0f13fdb86902206da90a86042e252bcd1e80a168c719e4a1ddcc3cebea24b9812c5453c79107e9832103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71000000000000"
    val inputIndex = UInt32(1)
    val wtx = WitnessTransaction(rawTx)
    val witScriptPubKey =
      P2WPKHWitnessSPKV0("1600144c9c3dfac4207d5d8cb89df5722cb3d712385e3f")
    val amount = Satoshis(2000)
    val txSigComponent =
      WitnessTxSigComponentRaw(wtx,
                               inputIndex,
                               TransactionOutput(amount, witScriptPubKey),
                               Policy.standardFlags)

    val serializedForSig = TransactionSignatureSerializer
      .serializeForSignature(txSigComponent,
                             HashType.sigHashSingleAnyoneCanPay,
                             taprootOptions = TaprootSerializationOptions.empty)
      .toHex

    serializedForSig must be(
      "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000010000001976a9144c9c3dfac4207d5d8cb89df5722cb3d712385e3f88acd007000000000000ffffffff2d793f9722ac8cbea9b2e0a2929cda4007b8312c6ec3b997088439e48e7aa64e0000000083000000")
  }

  it must "work with the p2sh(p2wpkh) example in BIP143" in {
    //https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#p2sh-p2wpkh

    val expected = {
      "01000000" +
        "b0287b4a252ac05af83d2dcef00ba313af78a3e9c329afa216eb3aa2a7b4613a" +
        "18606b350cd8bf565266bc352f0caddcf01e8fa789dd8a15386327cf8cabe198" +
        "db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a547701000000" +
        "1976a91479091972186c449eb1ded22b78e40d009bdf008988ac" +
        "00ca9a3b00000000" +
        "feffffff" +
        "de984f44532e2173ca0d64314fcefe6d30da6f8cf27bafa706da61df8a226c83" +
        "92040000" +
        "01000000"
    }

    val expectedHash = DoubleSha256Digest.fromHex(
      "64f3b0f4dd2bb3aa1ce8566d220cc74dda9df97d8490cc81d89d735c92e59fb6")

    val inputIndex = UInt32.zero

    val p2sh = P2SHScriptPubKey.fromAsmHex(
      "a9144733f37cf4db86fbc2efed2500b4f4e49f31202387")
    val redeemScript = P2WPKHWitnessSPKV0.fromAsmHex(
      "001479091972186c449eb1ded22b78e40d009bdf0089")

    val amount = Bitcoins(10)

    val output = TransactionOutput(amount, p2sh)

    //https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#p2sh-p2wpkh

    val unsignedTx =
      "0100000001db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a54770100000000feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac92040000"
    val ubtx = BaseTransaction.fromHex(unsignedTx)

    val p2shScriptSig = P2SHScriptSignature(redeemScript)

    val oldInput = ubtx.inputs(inputIndex.toInt)

    val updatedInput = TransactionInput(oldInput.previousOutput,
                                        p2shScriptSig,
                                        oldInput.sequence)

    val updatedInputs = ubtx.inputs.updated(inputIndex.toInt, updatedInput)
    val witness = EmptyWitness.fromInputs(updatedInputs)

    val uwtx = WitnessTransaction(version = ubtx.version,
                                  inputs = updatedInputs,
                                  outputs = ubtx.outputs,
                                  lockTime = ubtx.lockTime,
                                  witness = witness)

    val wtxSigComp = {
      WitnessTxSigComponentP2SH(transaction = uwtx,
                                inputIndex = inputIndex,
                                output = output,
                                flags = Policy.standardFlags)
    }

    val serialized = TransactionSignatureSerializer
      .serializeForSignature(txSigComponent = wtxSigComp,
                             hashType = HashType.sigHashAll,
                             taprootOptions = TaprootSerializationOptions.empty)
      .toHex

    serialized must be(expected)

    val hash = TransactionSignatureSerializer.hashForSignature(
      txSigComponent = wtxSigComp,
      hashType = HashType.sigHashAll,
      taprootOptions = TaprootSerializationOptions.empty)

    hash must be(expectedHash)
  }

  it must "serialize the BIP143 p2sh(p2wsh) examples" in {
    //https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#p2sh-p2wsh

    val unsignedTx =
      "010000000136641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e0100000000ffffffff" +
        "0200e9a435000000001976a914389ffce9cd9ae88dcc0631e88a821ffdbe9bfe2688acc0832f05000000001976a9147480a33f95068" +
        "9af511e6e84c138dbbd3c3ee41588ac00000000"

    val ubtx = BaseTransaction.fromHex(unsignedTx)

    val inputIndex = UInt32.zero

    val bitcoins = Bitcoins(9.87654321)

    val p2wsh = P2WSHWitnessSPKV0.fromAsmHex(
      "0020a16b5755f7f6f96dbd65f5f0d6ab9418b89af4b1f14a1bb8a09062c35f0dcb54")

    val p2shSPK = P2SHScriptPubKey.fromAsmHex(
      "a9149993a429037b5d912407a71c252019287b8d27a587")

    val p2shScriptSig = P2SHScriptSignature(witnessScriptPubKey = p2wsh)

    val oldInput = ubtx.inputs(inputIndex.toInt)

    val updatedInput = TransactionInput(oldInput.previousOutput,
                                        p2shScriptSig,
                                        oldInput.sequence)

    val updatedInputs = ubtx.inputs.updated(inputIndex.toInt, updatedInput)

    val m = MultiSignatureScriptPubKey.fromAsmHex(
      "56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56ae")
    val witnessScript = P2WSHWitnessV0(m)
    val txWit = TransactionWitness(Vector(witnessScript))

    val uwtx = WitnessTransaction(ubtx.version,
                                  updatedInputs,
                                  ubtx.outputs,
                                  ubtx.lockTime,
                                  txWit)

    val output = TransactionOutput(bitcoins, p2shSPK)

    val wtxSigComp = {
      WitnessTxSigComponentP2SH(transaction = uwtx,
                                inputIndex = inputIndex,
                                output = output,
                                flags = Policy.standardFlags)
    }

    val expectedSerialization =
      "0100000074afdc312af5183c4198a40ca3c1a275b485496dd3929bca388c4b5e31f7aaa03bb13029ce7b1f559ef5e747fcac439f1455a2ec7c5f09b72290795e7066504436641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffffbc4d309071414bed932f98832b27b4d76dad7e6c1346f487a8fdbb8eb90307cc0000000001000000"

    val serialization = TransactionSignatureSerializer.serializeForSignature(
      txSigComponent = wtxSigComp,
      hashType = HashType.sigHashAll,
      taprootOptions = TaprootSerializationOptions.empty)

    serialization.toHex must be(expectedSerialization)

    //with a SIGHASH_NONE

    val expectedSigHashNone =
      "0100000074afdc312af5183c4198a40ca3c1a275b485496dd3929bca388c4b5e31f7aaa0000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffff00000000000000000000000000000000000000000000000000000000000000000000000002000000"

    val serializationSigHashNone =
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashNone,
        taprootOptions = TaprootSerializationOptions.empty)

    serializationSigHashNone.toHex must be(expectedSigHashNone)

    //with sighash single

    val expectedSigHashSingle =
      "0100000074afdc312af5183c4198a40ca3c1a275b485496dd3929bca388c4b5e31f7aaa0000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffff9efe0c13a6b16c14a41b04ebe6a63f419bdacb2f8705b494a43063ca3cd4f7080000000003000000"

    val serializationSigHashSingle =
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashSingle,
        taprootOptions = TaprootSerializationOptions.empty)

    serializationSigHashSingle.toHex must be(expectedSigHashSingle)

    val expectedSigHashAllAnyoneCanPay =
      "010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffffbc4d309071414bed932f98832b27b4d76dad7e6c1346f487a8fdbb8eb90307cc0000000081000000"

    val serializationSigHashAllAnyoneCanPay =
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashAllAnyoneCanPay,
        taprootOptions = TaprootSerializationOptions.empty)

    serializationSigHashAllAnyoneCanPay.toHex must be(
      expectedSigHashAllAnyoneCanPay)

    val expectedSigHashNoneAnyoneCanPay =
      "010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffff00000000000000000000000000000000000000000000000000000000000000000000000082000000"

    val serializationSigHashNoneAnyoneCanPay = {
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashNoneAnyoneCanPay,
        taprootOptions = TaprootSerializationOptions.empty)
    }

    serializationSigHashNoneAnyoneCanPay.toHex must be(
      expectedSigHashNoneAnyoneCanPay)

    val expectedSigHashSingleAnyoneCanPay =
      "010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffff9efe0c13a6b16c14a41b04ebe6a63f419bdacb2f8705b494a43063ca3cd4f7080000000083000000"
    val serializationSigHashSingleAnyoneCanPay = {
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashSingleAnyoneCanPay,
        taprootOptions = TaprootSerializationOptions.empty)
    }

    serializationSigHashSingleAnyoneCanPay.toHex must be(
      expectedSigHashSingleAnyoneCanPay)

  }

  it must "make sure FindAndDelete is not applied to the BIP143 serialization algorithm" in {
    val unsignedTx =
      "010000000169c12106097dc2e0526493ef67f21269fe888ef05c7a3a5dacab38e1ac8387f14c1d000000ffffffff0101000000000000000000000000"
    val ubtx = BaseTransaction.fromHex(unsignedTx)
    val inputIndex = UInt32.zero

    val p2wsh = P2WSHWitnessSPKV0.fromAsmHex(
      "00209e1be07558ea5cc8e02ed1d80c0911048afad949affa36d5c3951e3159dbea19")
    val amount = Satoshis(200000)
    val output = TransactionOutput(amount, p2wsh)

    //OP_CHECKSIGVERIFY <0x30450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01>
    val redeemScript = NonStandardScriptPubKey.fromAsmHex(
      "ad4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01")

    val scriptWit = P2WSHWitnessV0(redeemScript)
    val txWit = TransactionWitness(Vector(scriptWit))

    val uwtx = WitnessTransaction(ubtx.version,
                                  ubtx.inputs,
                                  ubtx.outputs,
                                  ubtx.lockTime,
                                  txWit)

    val wtxSigCompRaw = {
      WitnessTxSigComponentRaw(transaction = uwtx,
                               inputIndex = inputIndex,
                               output = output,
                               flags = Policy.standardFlags)
    }

    val serialized = TransactionSignatureSerializer.serializeForSignature(
      txSigComponent = wtxSigCompRaw,
      hashType = HashType.sigHashAll,
      taprootOptions = TaprootSerializationOptions.empty)

    val expectedSerialization =
      "01000000b67c76d200c6ce72962d919dc107884b9d5d0e26f2aea7474b46a1904c53359f3bb13029ce7b1f559ef5e747fcac439f1455a2ec7c5f09b72290795e7066504469c12106097dc2e0526493ef67f21269fe888ef05c7a3a5dacab38e1ac8387f14c1d00004aad4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01400d030000000000ffffffffe5d196bfb21caca9dbd654cafb3b4dc0c4882c8927d2eb300d9539dd0b9342280000000001000000"

    serialized.toHex must be(expectedSerialization)
  }

  it must "fail to create a SIGHASH from an invalid number" in {
    val z = 0
    Try(SIGHASH_NONE(z)).isFailure must be(true)
    Try(SIGHASH_SINGLE(z)).isFailure must be(true)
    Try(SIGHASH_ANYONECANPAY(z)).isFailure must be(true)
    Try(SIGHASH_ALL_ANYONECANPAY(z)).isFailure must be(true)
    Try(SIGHASH_NONE_ANYONECANPAY(z)).isFailure must be(true)
    Try(SIGHASH_SINGLE_ANYONECANPAY(z)).isFailure must be(true)
  }

  it should "have old and new serializeForSignature functions agree" in {
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

        val correctScripts =
          creditingTxsInfo.flatMap { signInfo =>
            signInfo.signers.map { _ =>
              val txSigComponent =
                TxSigComponent(signInfo.inputInfo, spendingTx, prevOutMap)

              val oldBytes =
                TransactionSignatureSerializer.serializeForSignature(
                  txSigComponent,
                  signInfo.hashType,
                  taprootOptions = TaprootSerializationOptions.empty)

              val newBytes =
                TransactionSignatureSerializer.serializeForSignature(
                  spendingTx,
                  signInfo,
                  signInfo.hashType,
                  taprootOptions = TaprootSerializationOptions.empty)

              oldBytes == newBytes
            }
          }

        assert(correctScripts.forall(_ == true))
    }
  }

  it should "have old and new hashForSignature functions agree" in {
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

        val correctHashes =
          creditingTxsInfo.flatMap { signInfo =>
            signInfo.signers.map { _ =>
              val txSigComponent =
                TxSigComponent(signInfo.inputInfo, spendingTx, prevOutMap)

              val oldHash =
                TransactionSignatureSerializer.hashForSignature(
                  txSigComponent,
                  signInfo.hashType,
                  taprootOptions = TaprootSerializationOptions.empty)

              val newHash =
                TransactionSignatureSerializer.hashForSignature(
                  spendingTx,
                  signInfo,
                  signInfo.hashType,
                  taprootOptions = TaprootSerializationOptions.empty)

              oldHash == newHash
            }
          }

        assert(correctHashes.forall(_ == true))
    }
  }

  it should "have old and new calculateScriptForSigning functions agree" in {
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

        val correctScripts =
          creditingTxsInfo.flatMap { signInfo =>
            signInfo.signers.map { _ =>
              val txSigComponent =
                TxSigComponent(signInfo.inputInfo, spendingTx, prevOutMap)

              val oldScript =
                BitcoinScriptUtil.calculateScriptForSigning(
                  txSigComponent,
                  txSigComponent.output.scriptPubKey.asm)

              val newScript =
                BitcoinScriptUtil.calculateScriptForSigning(
                  spendingTx,
                  signInfo,
                  signInfo.output.scriptPubKey.asm)

              oldScript == newScript
            }
          }

        assert(correctScripts.forall(_ == true))
    }
  }

  it must "serialize a taproot transaction correctly with SIGHASH_NONE" in {
    val spendingTxHex =
      "f705d6e8019870958e85d1d8f94aa6d74746ba974db0f5ccae49a49b32dcada4e19de4eb5ecb00000000925977cc01f9875c000000000016001431d2b00cd4687ceb34008d9894de84062def14aa05406346"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val prevOutputHex =
      "b4eae1010000000022512039f7e9232896f8100485e38afa652044f855e734a13b840a3f220cbd5d911ad5"
    val prevOutput = TransactionOutput.fromHex(prevOutputHex)
    val witnessHex =
      "25e45bd4d4b8fcd5933861355a2d376aad8daf1af1588e5fb6dfcea22d0d809acda6fadca11e97f5b5c85af99df27cb24fa69b08fa6c790234cdc671d3af5a7302"
    val stack = Vector(ByteVector.fromValidHex(witnessHex))
    val witness = TaprootWitness.fromStack(stack.reverse)
    val inputIndex = UInt32.zero

    val witnessTx =
      WitnessTransaction.toWitnessTx(spendingTx).updateWitness(0, witness)

    val outputMap: PreviousOutputMap =
      PreviousOutputMap(
        spendingTx.inputs.map(_.previousOutput).zip(Vector(prevOutput)).toMap)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx,
                                                      inputIndex,
                                                      outputMap,
                                                      Policy.standardFlags)
    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashNone,
      //keypath doesn't use the tapscript tree
      taprootOptions = TaprootSerializationOptions.empty)

    //generated from: https://github.com/Christewart/bitcoin/tree/2022-06-12-taproot-sig-serialization
    //command to run it:  DIR_UNIT_TEST_DATA=/home/chris/dev/bitcoin-s/core-test/.jvm/src/test/resources ./src/test/test_bitcoin --run_test=script_tests/script_assets_test
    val expected =
      "0002f705d6e805406346214cba8933db55eb868be9a32446d8c1a96a0fbb20ab184f4d6199b4f0355a5670646bb1b064d5cba210cc2b4c335874223c44a3efea5a465e80dd202c0356595f2625da12f1a7711bc1a16f9c9021d4b18b925c02a0b2aea05e43a38536cccf2eed245a04e82b3c153c4253d628f837e619e8ee21f76f6d11f8e45782debb6a0000000000"

    val expectedHash =
      "6506e01ec7086c7f587e86e1e9d3f92c1e22201981407f1465c3e853cb59de0d"
    val hash =
      TransactionSignatureSerializer.hashForSignature(
        txSigComponent = taprootTxSigComponent,
        hashType = HashType.sigHashNone,
        taprootOptions = TaprootSerializationOptions.empty)
    assert(serialize.toHex == expected)
    assert(hash.hex == expectedHash)
  }

  it must "serialize a transaction correctly with SIGHASH_NONE at input index 1" in {
    val spendingTxHex =
      "26dc279d02d8b1a203b653fc4e0f27f408432f3f540136d33f8f930eaeba655910095142980402000000fd697cd4eb5278f1e34545cd57b6670df806fa3a0a064fd8e385a19f1a53d9ce8d8971a30f02000000378d5fb502335dbe02000000001976a9140053a23441c8478caac4c6b769c51f8476cd4b4b88ac58020000000000001976a914f2aae94a43e0d173354201d7832b46c5269c8a2488ac4a08671e"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.one
    val prevOutputsHex = Vector(
      "91ca4c010000000017a9145658b58602cdf7b7e962cfe44e024cb0e366f27087",
      "cb127401000000002251201ebe8b90363bd097aa9f352c8b21914e1886bc09fe9e70c09f33ef2d2abdf4bc"
    )
    val prevOutputs = prevOutputsHex.map(TransactionOutput.fromHex)

    val witnessStackHex = Vector(
      "9675a9982c6398ea9d441cb7a943bcd6ff033cc3a2e01a0178a7d3be4575be863871c6bf3eef5ecd34721c784259385ca9101c3a313e010ac942c99de05aaaa602",
      "5799cf4b193b730fb99580b186f7477c2cca4d28957326f6f1a5d14116438530e7ec0ce1cd465ad96968ae8a6a09d4d37a060a115919f56fcfebe7b2277cc2df5cc08fb6cda9105ee2512b2e22635aba",
      "7520c7b5db9562078049719228db2ac80cb9643ec96c8055aa3b29c2c03d4d99edb0ac",
      "c1a7957acbaaf7b444c53d9e0c9436e8a8a3247fd515095d66ddf6201918b40a3668f9a4ccdffcf778da624dca2dda0b08e763ec52fd4ad403ec7563a3504d0cc168b9a77a410029e01dac89567c9b2e6cd726e840351df3f2f58fefe976200a19244150d04153909f660184d656ee95fa7bf8e1d4ec83da1fca34f64bc279b76d257ec623e08baba2cfa4ea9e99646e88f1eb1668c00c0f15b7443c8ab83481611cc3ae85eb89a7bfc40067eb1d2e6354a32426d0ce710e88bc4cc0718b99c325509c9d02a6a980d675a8969be10ee9bef82cafee2fc913475667ccda37b1bc7f13f64e56c449c532658ba8481631c02ead979754c809584a875951619cec8fb040c33f06468ae0266cd8693d6a64cea5912be32d8de95a6da6300b0c50fdcd6001ea41126e7b7e5280d455054a816560028f5ca53c9a50ee52f10e15c5337315bad1f5277acb109a1418649dc6ead2fe14699742fee7182f2f15e54279c7d932ed2799d01d73c97e68bbc94d6f7f56ee0a80efd7c76e3169e10d1a1ba3b5f1eb02369dc43af687461c7a2a3344d13eb5485dca29a67f16b4cb988923060fd3b65d0f0352bb634bcc44f2fe668836dcd0f604150049835135dc4b4fbf90fb334b3938a1f137eb32f047c65b85e6c1173b890b6d0162b48b186d1f1af8521945924ac8ac8efec321bf34f1d4b3d4a304a10313052c652d53f6ecb8a55586614e8950cde9ab6fe8e22802e93b3b9139112250b80ebc589aba231af535bb20f7eeec2e412f698c17f3fdc0a2e20924a5e38b21a628a9e3b2a61e35958e60c7f5087c"
    )
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = TaprootScriptPath(witnessStack.reverse)

    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val outputMap: PreviousOutputMap =
      PreviousOutputMap(
        spendingTx.inputs.map(_.previousOutput).zip(prevOutputs).toMap)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx,
                                                      inputIndex,
                                                      outputMap,
                                                      Policy.standardFlags)
    val leafHash = Sha256Digest.fromHex(
      "8d76c657582b87b087f36579a9ea78816d7e2a94098bc3e3c6113ed4b6315bb4")
    val taprootOptions = TaprootSerializationOptions(Some(leafHash), None, None)
    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashNone,
      taprootOptions)

    val expected =
      "000226dc279d4a08671ec49b9ed79ac5ddae54b41edb90d86bf61adc5366f7e569cb742c46f6a0452d0f2293f7596a74fc91f3471ca363a2" +
        "77a7ecdc34ddb9577c28cd125ab7595b9fbfe3e1c759996b0462e61f49390468ac4dfc15c75813358c5dce6114a150f74f056716b39363" +
        "5ea25c9ad479e865587219e6ca5252bd42635ba82c2c23cee4859a02010000008d76c657582b87b087f36579a9ea78816d7e2a94098bc3" +
        "e3c6113ed4b6315bb400ffffffff"

    assert(serialize.toHex == expected)
  }

  it must "serialize a transaction correctly with SIGHASH_ALL in tapscript" in {
    val expected =
      "0001020000008100000062e7fe0c8366c2b10f15337e1d6805d42b18ef2ace5074208a3226536669a01d72c6ac08691c57474733a88ea307b77676747773e271a3e6e7a882c6975859b0356e0b33edf702817496438b0ed026bdbe95df3955911a52b251faaf5848786418a9e78c8505f3a519d63ad3e0050be24c72726bab0a45253ba4740128675dba83dd2a84be1b50fa657c8ab6ee3eb70944327e8c91f35ab8c809f3149ec8f88402000000000c013c8aa4ee2a624a516c877892db854d6ccc9fd1cd8b94895cff88abaccbc600ffffffff"

    val spendingTxHex =
      "0200000001a18b98cfbfc348ed6968487a554ebc89672980515d348905347065e09e9986ff19020000007333dec7013a1141010000000016001460e6950c92cc5e9354c40c384e16099e31e4e1ed81000000"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.zero
    val prevout = TransactionOutput.fromHex(
      "5ba3440100000000225120325901f5659ff9031572e5f790166d9efbc9c693daa47d4acd2ba873176d7879")
    val prevOutputs = Vector(prevout)
    val outputMap: PreviousOutputMap =
      PreviousOutputMap(
        spendingTx.inputs.map(_.previousOutput).zip(prevOutputs).toMap)

    val witnessStackHex = Vector(
      "de7950201305f38c82b1f9743b1cecbc0dda2ea4557c1ff22b769666e12539302a0988cd0e1489b8123c1181e275b576fe8ea7c4a997d332bcf90a5dc6604a2701",
      "eff98c656ca3a880d1c0419733900856444cece1cdc9090fc76404111c5a7f39535fd10f1610a549401ae35b98f50bdcf6a20218dbe9406a7" +
        "53ccef956ecc04b50cb5a1ce05d47b35a3b9ebea0d4b050eeaec240be126dc4ade1fbfe833b229ade62139d2f74d8e63539cfffd2838ab08" +
        "9d5e989de6e4e271dd66515723ed0f955c5057b3d69c7b6d56729bf84255c60500acdb55400e1f8bc877a02b16738561b151a12e7f24fc5" +
        "c0f261c531ec4bd7422c4e8904d0704522fa0357efa9d2c3670c68d31a93a4366ab0b853a3e985182cabc77851279abb30e70c754ab125f" +
        "f7eaec80f9291786e6223",
      "4cdc663c0838dc3ca7b6ee5270b2b2cfd7ea471199107a227985dd792b2265b97a623ff374e929419929e" +
        "af4bdd44404a8736f6e432e290b72db1d160bf66e073c953f8080c973a5bf10aa6118203ef2f7352b60582c30fb346e64d8d9e644b9af" +
        "aea3def032d6b3a7213ab22f90ec8229c7e0e835f3d2dda77f1b282d213b8273c3bb5bcf363db38ea7fe3cbc91501d6117e50c23c0a86a48" +
        "c8262bfc320c0065d49fbf66391b6e89b9ad6ced597bad1e21e10c792c68f3648bba23cd3172cb882742b933930ef54a532a79469cf5d2ce" +
        "3c15f51e9e60c567254c4904356d2095f162a727767e5590104a2f3f7951d36189b69114dba53745e8094a104d3c2b51646eac69686ead6ea" +
        "d6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead547cba5587",
      "c149a7d6ebf0c84cdb169ebdb416c7ed80ded5df70ba66a5963fc040ece057c5dcc6cbacab88ff5c89948bcdd19fcc6c4d85db9278876c514a62" +
        "2aeea48a3c010b9b26cc213552ffe736f9cc8f34cb3115ac3ae5d2e461d1b67acb16fdd578b3d44285e0c3dc874f61b1f4890dc37a1fa28d" +
        "2a5ce016f86cf753f058e3c6bdcf5bffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffcdf909508ddd2051a9" +
        "22b8df1bd406cfedccd6720aa22a939fd4c7bb2ffc62001b61e617e725548d0014f6e14956c7d50995bd56940b7049ff8dcae0d811cfddff" +
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff61104e614e2fba100f24bdf350cb4e661e197d514ee91b5135" +
        "17123901cf4ed2ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff75496144e5638831f12cc0cabd503886b" +
        "4d9320dfe54b5cf4caeee0abc1071763acb8086b1e6369b077a9c5da54554d51ee3ea03bc3f7293e61b4a01632b1fb880c9a8b092f71b0b0" +
        "920f0b9391083cb42afcc03755c2c1b81a537e4d94136bebcbd4e54e83578f074f71de64dee7f502082547cfe8216051f7d4dd2f5b540240" +
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" +
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000d5c2d7bf5f9a487ef8ccdbe6426c32c6" +
        "7d6479573676a952d69bf5d511d394c41e138e483f3e622cb6df748a9a9e4bb851d2f6247cda5f109b291a76f070f599ceb01706beea3e0" +
        "bb53b64bfa4afda675993ae79e89ea381de378361ec843766b0ff93bcf23d4ddb653014e908c7643d63c94bfc064e8f31ce201f97e2a6ec" +
        "84fa5f7f72a13744311bb3ae9c0574d33169dae0a2cf53848389dfc12882cd36b1dacbcfa3022274da8c96760cd517e1f9e731744d59808" +
        "70bacb5402dc1fc14830000000000000000000000000000000000000000000000000000000000000000"
    )
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = TaprootScriptPath(witnessStack.reverse)

    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx,
                                                      inputIndex,
                                                      outputMap,
                                                      Policy.standardFlags)

    val leafHash = Sha256Digest.fromHex(
      "0c013c8aa4ee2a624a516c877892db854d6ccc9fd1cd8b94895cff88abaccbc6")
    val taprootOptions = TaprootSerializationOptions(Some(leafHash), None, None)
    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashAll,
      taprootOptions)

    assert(serialize.toHex == expected)
  }

  it must "serialize a transaction correctly when it contains an annex" in {
    val spendingTxHex =
      "fff6a5be017ee245cb1041dac057f372c63e54101f6ed39768446f7c2ba3395c40c9708673b5000000007ab210bf0166c81a00000000001600149c6294492ff7c95b641cb0c037abebab7607d608f6000000"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.zero
    val prevout = TransactionOutput.fromHex(
      "0aab5f01000000002251204aeed300fcf09260e6c44f6680f5f0eff387e6c4594700358dae78e695aafbe0")
    val prevOutputs = Vector(prevout)
    val outputMap: PreviousOutputMap =
      PreviousOutputMap(
        spendingTx.inputs.map(_.previousOutput).zip(prevOutputs).toMap)

    val witnessStackHex = Vector(
      "ce51561684cec9066e9a4f336fcf7d8a5e6386be8196bbbd283ff95f8a6dc3a3c06468b66640b0d46d2a03836250d23dff5286a98b9d7db760754289238b181701",
      "732158c5870a65f00616ce86ea3349a65fa568673f788d62dd0b1bb159c1a10f63ac6a4c020b3f091b69d9ff40c5eeba2997b7b07e0b6f0342efacd9cad9f1a3a36f2ab326c9600920a68fb1c759c145a45d4917b6a6fb8b281ff967e7fcb30794a0145f5c81274a3279c07371c5f3fcc901af4d8d5f1e01fb63d423375af0",
      "4cdc5910c3f2484d9068793fb2a2bc067a9ffa9e92e15cfaa2f1b9403d3c20f5ae27df1eb842f3b23b34f9f650c2fcd7a70279a0e31b345a55c463b80237efe3035fc1eee8a7e190be5fdc0346b077cd9de5c77dc358095c3abad518432964decd70975170620e88a77d8113e438bc5cedd72fec95bfe718da62da021cfdbaaad4da662239dddaffbdcf8d7743d302d61e6b772a6ea88a08063b12b013a58e20662cc94a388417c33189c01446ba1b109ebd657802beb11cde42eee56b4e727bec01584f36c011f473fe1796b9fce2ff3091467fb97756d270148e43e6196d20622f8d27f4bb1dcd3ffc0f4a5154346fd95b6de97266d7ae2f3dedeae38560ac51646eac69686ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead547cba5587",
      "c07ca47bef7a36368eecb0cc414ae5b7e1501d33cf18ffe577d7ac5b7544329693f8bbad239271b84c40635f887195232e18266e1dc7630c15e89287124af619a6e32d5bcbd1dc70010e340252a95dccdedd7af41a3fe9eaef71fcf7c0e0b1962c0000000000000000000000000000000000000000000000000000000000000000dd557c6de8bcc91b1c562952da9941a75723fb56775414fe1267bea04a039a6d4a331800858a68ddd729a65b34a1de3af1691822e16335e01f2b7c1a3cf0fdd7a07b85292c1b49e46f5827550f16651fae8c7a97aa7e0d38bf2480110384684a0ac5b7ab5bdb21127710ba4560050a4192a79984cd9995a18c4f2199cd990177",
      "5044334a3d687b703d2c05996fa854ce25b371930d231b06253eea7fffaf6f94502fc094ce17ae453fa70844a6dc300132a422b3f410a2fac05af16a028de24ffec4b442717ece87323eb098e4e4f1e5e807397e76eb1be7eb5ec6d0a89b4fb742ec76feea8bab3207ef0325b5eb214dabe9f318120f8c546d4e1b044f41a4ac53cabbb16ba97139796936347a5416db20f8891c2d9fa06eb76334f440d78b1e75d12589ffa86d2b4db985265f35ebeb8c9a1f9e07ca22b3589a681cb7869207278854c1a831182057019e9256fc1612d7867fd9b31a4df131865108374f4bf6f1bbd7ac3030f0cc9fbfa7d66c15cf1ca43df20bc17bd01278d38c0ede405bac44246069e7bd343f54bcdef46c06581b2255a3a1bbd9c8374a86ca4a2dccccffe74d947b934328a2df7a31999f728e40b6f0f9320cfe359b963ecf8ab6c8f90eac3e132073c096160e4754de23962c621e72a9b51b554caf68a4784dbbcec7e6446fb2053564f5145cabd52458a8f0b7b09213f7c2a0f3f83ca5883293d8f72570fb22607f66a02560442c040bd925f140ff042e95d85f4dcf31f8e5caa95401df9d2fb2dafa85a8f4ac418df81d0d71d4e90411b05ddcc349e12065026a512aea234de64d248d319061f2f5759c5e49cca7fee71d2e6a10d61be825ba90a26f08552a6c6c8a29c1e499b0b6ffe17339a4981c9b82f93db37a3694250b2fb8eacb42ba74dfa881dc0790b1a593f60b535f7aeb6fe37c83cde0f5b7b211094c165d9eb28a54a7bcf8621570b85912ad7ba3ed2e8ce446b226"
    )
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = TaprootScriptPath(witnessStack.reverse)

    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx,
                                                      inputIndex,
                                                      outputMap,
                                                      Policy.standardFlags)

    val expected =
      "0001fff6a5bef6000000808b524996f052e920857d3ee62dc9a6160b403cfcef0d2d03073e47a8871b6adc0a61e1e5fe3f77ae9a3b77a839068254192f52a55e557867aac49c8436c22c6c327e7aab9f14bfb15549767a609af732876f93dac0aab5d94f15cb5341b23a9f5a4670a9fb185fdc4c0c846835c824e40d8fcdc14f75f910ca82ce5fc1290d63628cb6889362ecf4d6c48063afdabdd9307d0638967e263e2badd09c6f9aa9030000000071795496bb62e1315323528edbc764523cec139391ab634e956558cc7fcb570ba719f64a128792e8150c63c71d6e26182e239571885f63404cb8719223adbbf800ffffffff"
    val leafHash = Sha256Digest.fromHex(
      "a719f64a128792e8150c63c71d6e26182e239571885f63404cb8719223adbbf8")
    val annexHash = Sha256Digest.fromHex(
      "71795496bb62e1315323528edbc764523cec139391ab634e956558cc7fcb570b")
    val taprootOptions =
      TaprootSerializationOptions(Some(leafHash), Some(annexHash), None)

    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashAll,
      taprootOptions)

    assert(serialize.toHex == expected)
  }

  it must "serialize a transaction correctly in taproot when it uses code sep" in {
    val expected =
      "000001000000e5010000628a401405f2f1d86ae4b04820202057ed7508a02d3b8780ab306b05bff05dac4d084776dfbb4917170d8535dd7eac99ed2fe7e2a24f648eb68e6de5005558b351ee3db4727461bbab05ac0aec4ec66a611225ee790e2f99fbd19013140e0f5ebe7656e702bca178685c66343fe1d9e91b845099b5f7fbbbb63463277b546933cdda11ea31e190a76cdca66a496322ac5b04ef2f075115444d8f32e046f0c57802000000006757e0866b83772c944df11581616b8752bb2596a43c8941de0e427ce4fa06300006000000"

    val spendingTxHex =
      "01000000018ac8b525460593c8d490a4d73d99126309191b84770b43ff226feb48d517b721c500000000482137f0011b0656000000000016001470b9b7ca06422cf6c7011977c96c1914374a8462e5010000"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.zero
    val prevout = TransactionOutput.fromHex(
      "5e6aae010000000022512045cad6b20c81a782892f064caeab47cad9c276a917bed28ac30435e343a82188")
    val prevOutputs = Vector(prevout)
    val outputMap: PreviousOutputMap =
      PreviousOutputMap(
        spendingTx.inputs.map(_.previousOutput).zip(prevOutputs).toMap)

    val witnessStackHex = Vector(
      "2c6347f19bd72e40ff0d3ffcb872973ead3100bd0dc39d2dc48d31bb039e0f281f24c963404922771ef28ec09ec6f3875dca076f8ebc0c59d99cfa3e0eafdf0483",
      "",
      "4ddb016458021de049979f9df14b08fadbdd01a3bfa1a34d0836c9ba703646dba7690bd1c651e117bf8372cbdb66c63839506ad469d70141c62b097f8672919837fbac1394af4111dbf0943b1fa66adc89a30d7ef3003e346db4956bcdc42aa2db92975ac5bbda74bcc9759c2f9ae4c470f7cad700261b95327b213a3be13a1c017794a0d03ca7785c023ce9dc01bd087ed707cad46e2e3380b9c20311ca1579469f6539183792f94ed81dfba076a1178f0552a34cb46a385b16911cb8f2d488d33c8ffb5878e8b94f057533674247fff4c1459ec17e9f51e84c1b86bc92aa3e99fa56f8c663c006b3f1a2ef3718f9f7d6cdd23c5e6bff93254a88b8cb92b1951008f4fd025d761ef6dd6875160550002d5d061b208680caf845ee8aeefd84d91b2334af649d1a977484a250be89bd578dd287e1dd5ac82826fcfc3e6b7abbde717a9fee43dca49db2dba31a67760424a18ab210d8df4d9ed10556dfc2705245b525dabc0fc5329d557ab56315b69f5aea26adfc4729e1329769d00db2a18e1cd78a9b4690c3196e6da7c17617b7398b7a10ad3bf578a9091a908cb67972d75afdd5da8c8cfb4cc002bc6bd2f5d9b46c8584e76276158b078829e987a4e3ff427e07730ce5a19b4932e1961b6c9a1139ddc5a51e7df848f64e50b96dfdb27563ab201cbc6403a1e2bfe3c9e7e7a5e8988bcd015515c82748a236fc87a0814b2390ed67ab207ca4fde194f2ce74ede0129eff73e13955c11c7d9ae59f4c76aea01c52a4e5f568ac",
      "c01cbc6403a1e2bfe3c9e7e7a5e8988bcd015515c82748a236fc87a0814b2390ed801fbd24116a58ab9033b015b1e889aa20dc3f119a49e6458cae2f8b6f042b5b"
    )
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = TaprootScriptPath(witnessStack.reverse)

    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx,
                                                      inputIndex,
                                                      outputMap,
                                                      Policy.standardFlags)

    val leafHashHex =
      "6757e0866b83772c944df11581616b8752bb2596a43c8941de0e427ce4fa0630"
    val leafHash = Sha256Digest.fromHex(leafHashHex)
    val taprootOptions =
      TaprootSerializationOptions(Some(leafHash), None, Some(UInt32(6)))
    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashDefault,
      taprootOptions)

    assert(serialize.toHex == expected)
  }

  it must "serialize a taproot transaction correctly with SIGHASH_SINGLE_ANYONECANPAY and OP_CODESEPARATOR" in {
    val expected =
      "008301000000e5010000028ac8b525460593c8d490a4d73d99126309191b84770b43ff226feb48d517b721c50000005e6aae010000000022512045cad6b20c81a782892f064caeab47cad9c276a917bed28ac30435e343a82188482137f0cdda11ea31e190a76cdca66a496322ac5b04ef2f075115444d8f32e046f0c5786757e0866b83772c944df11581616b8752bb2596a43c8941de0e427ce4fa06300006000000"

    val spendingTxHex =
      "01000000018ac8b525460593c8d490a4d73d99126309191b84770b43ff226feb48d517b721c500000000482137f0011b0656000000000016001470b9b7ca06422cf6c7011977c96c1914374a8462e5010000"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.zero
    val prevout = TransactionOutput.fromHex(
      "5e6aae010000000022512045cad6b20c81a782892f064caeab47cad9c276a917bed28ac30435e343a82188")
    val prevOutputs = Vector(prevout)
    val outputMap: PreviousOutputMap =
      PreviousOutputMap(
        spendingTx.inputs.map(_.previousOutput).zip(prevOutputs).toMap)

    val witnessStackHex = Vector(
      "2c6347f19bd72e40ff0d3ffcb872973ead3100bd0dc39d2dc48d31bb039e0f281f24c963404922771ef28ec09ec6f3875dca076f8ebc0c59d99cfa3e0eafdf0483",
      "",
      "4ddb016458021de049979f9df14b08fadbdd01a3bfa1a34d0836c9ba703646dba7690bd1c651e117bf8372cbdb66c63839506ad469d70141c62b097f8672919837fbac1394af4111dbf0943b1fa66adc89a30d7ef3003e346db4956bcdc42aa2db92975ac5bbda74bcc9759c2f9ae4c470f7cad700261b95327b213a3be13a1c017794a0d03ca7785c023ce9dc01bd087ed707cad46e2e3380b9c20311ca1579469f6539183792f94ed81dfba076a1178f0552a34cb46a385b16911cb8f2d488d33c8ffb5878e8b94f057533674247fff4c1459ec17e9f51e84c1b86bc92aa3e99fa56f8c663c006b3f1a2ef3718f9f7d6cdd23c5e6bff93254a88b8cb92b1951008f4fd025d761ef6dd6875160550002d5d061b208680caf845ee8aeefd84d91b2334af649d1a977484a250be89bd578dd287e1dd5ac82826fcfc3e6b7abbde717a9fee43dca49db2dba31a67760424a18ab210d8df4d9ed10556dfc2705245b525dabc0fc5329d557ab56315b69f5aea26adfc4729e1329769d00db2a18e1cd78a9b4690c3196e6da7c17617b7398b7a10ad3bf578a9091a908cb67972d75afdd5da8c8cfb4cc002bc6bd2f5d9b46c8584e76276158b078829e987a4e3ff427e07730ce5a19b4932e1961b6c9a1139ddc5a51e7df848f64e50b96dfdb27563ab201cbc6403a1e2bfe3c9e7e7a5e8988bcd015515c82748a236fc87a0814b2390ed67ab207ca4fde194f2ce74ede0129eff73e13955c11c7d9ae59f4c76aea01c52a4e5f568ac",
      "c01cbc6403a1e2bfe3c9e7e7a5e8988bcd015515c82748a236fc87a0814b2390ed801fbd24116a58ab9033b015b1e889aa20dc3f119a49e6458cae2f8b6f042b5b"
    )
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = TaprootScriptPath(witnessStack.reverse)

    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx,
                                                      inputIndex,
                                                      outputMap,
                                                      Policy.standardFlags)

    val leafHashHex =
      "6757e0866b83772c944df11581616b8752bb2596a43c8941de0e427ce4fa0630"
    val leafHash = Sha256Digest.fromHex(leafHashHex)
    val taprootOptions =
      TaprootSerializationOptions(Some(leafHash), None, Some(UInt32(6)))
    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashSingleAnyoneCanPay,
      taprootOptions)

    assert(serialize.toHex == expected)
  }

  it must "serialize a SigVersionWitnessV0 tx correctly with SIGHASH_SINGLE_ANYONECANPAY flag" in {
    val expected =
      "0200000004d89ccd4eb6d1471fb4046b3bfe6472c94ef32c7af11fd1dc7983cf94b50938c0a7be6fb0410fe40a6186c06ec17aeb25337f599541b3b0b3e72c3765fe99054fba9b57c66346730f1710c3182a2d3bb843cbfb551362749dcaa7fd2343f0630f0100001976a914d3165d2dcffd0c2461e33ee2de0ef4810e1630ec88aca84e9401000000005c976dcc2c78b5d9620c7cf3da08bc45b85616cd291cfd5fc3b3c3c8c3dda70497b2c3ff7bdb5f2e58000000"

    val spendingTxHex =
      "02000000014fba9b57c66346730f1710c3182a2d3bb843cbfb551362749dcaa7fd2343f0630f010000005c976dcc01e823a10000000000160014ab3ab349d01bdf301b1b78b0b3ed3c7895ebea9a7bdb5f2e"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.zero
    val prevout = TransactionOutput.fromHex(
      "a84e940100000000160014d3165d2dcffd0c2461e33ee2de0ef4810e1630ec")

    val witnessStackHex = Vector(
      "304402201ed30e9c471f4feca0557f353b7bacd5ace564ba9de5d51a0fff206468ddbb0602207dc60f5109b2bd3c80f5f79ffa7532a38e7e31a6583e355d83e1e355093d88c858",
      "041331b230dcbd3d716cc76cbf98e38da769460ec7439c48209b3028d3ab5531425bead44f30346b04806b047ebb4ad33358fa59dc02576c2cbf75f2fba0d7c328"
    )
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = ScriptWitness(witnessStack.reverse)

    val hashType = witness match {
      case p2wpkh: P2WPKHWitnessV0 =>
        p2wpkh.signature.hashTypeOpt.get
      case x =>
        sys.error(s"Not possible, $x")
    }
    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val taprootTxSigComponent =
      WitnessTxSigComponentRaw(witnessTx,
                               inputIndex,
                               prevout,
                               Policy.standardFlags)

    val taprootOptions =
      TaprootSerializationOptions.empty
    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      hashType,
      taprootOptions)

    assert(serialize.toHex == expected)
  }

  it must "serialize a SigVersionTaproot with SIGHASH_SINGLE" in {
    val expected =
      "0003f2c9f515dc020000a547b14868cb58fceeaf465ab317839a7796490cd1ed18eff44c080a8121641547b5d9ddab65cda48fa8f8c5d624733056fee1070d99f0af02a107fae870913e91fd3d0d2b685c2c5602153507a49d465b0c17d930b6ff9339ce26bae2d2e973d03635bec2ea8d09a363546c2f77ef96b2703222061a53bf4b855ddc6de8c06c0001000000aa50881629ef1f9c427944545c29e86f5405bd43e397205bd9c39195e1771338"

    val spendingTxHex =
      "f2c9f5150289433f77f04c11ab6e69d9dc463e8c7650698482ccbae50a6b94084733bf57efd40000000051b2dad3b683a298b504f1c3e5848105e60af2737797cb062c352b06cafb865b4b7c2eb8ef00000000795b6e8402d525a2020000000017a914c46f03d335d72659cff4dc2782b06bf1581496d787580200000000000017a9149f9d6473dd54d01d4e3c7fa34bde4fe83d808d0587dc020000"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.one
    val prevOutputs = Vector(
      "68002d01000000001976a9141d03185179941e287a525852993d8d5aed712a7588ac",
      "8d30780100000000225120db51afd5ed217d7a33c04dd0f8b5bb41f78a138d3aec6a6bfc2a03e266334c89"
    )
      .map(TransactionOutput.fromHex)
    val outputMap: PreviousOutputMap =
      PreviousOutputMap(
        spendingTx.inputs.map(_.previousOutput).zip(prevOutputs).toMap)

    val witnessStackHex = Vector(
      "85b5c1a31fda48328459aec83811bfd5754a24110c3fb7026500561e4880f313fbdb44125ca7e06f3a37490e1fcd0d9383ff970ff7b7f724a9df7dca8cf17d4d03")
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = TaprootWitness.fromStack(witnessStack.reverse)

    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx,
                                                      inputIndex,
                                                      outputMap,
                                                      Policy.standardFlags)

    val taprootOptions =
      TaprootSerializationOptions.empty
    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashSingle,
      taprootOptions)

    assert(serialize.toHex == expected)
  }

  it must "serialize a taprooot keypsend SIGHASH_SINGLE_ANYONECANPAY with annex correctly" in {
    val expected =
      "0083a3c824f5613e694f01ec975c5d145e8cccde50ab0ca8c5b41bfaedd041ea97b05285fff9df00dca26749010000bd2897010000000022512088ffcb569bf1dc1a9e0b22b8cb164c31bcf65e6e95fa113ddbdbbead6e85fedf4aa79981b0d1ad766994166c07455739e8a49adf25972a25046ede7f1573ec61b75beb3f7e93c573a659c28b99050bd63f8e7bd619af0175b784eadf33b52d95d3863c51"

    val spendingTxHex =
      "a3c824f501ec975c5d145e8cccde50ab0ca8c5b41bfaedd041ea97b05285fff9df00dca26749010000004aa799810120931801000000001976a914a9afd3de61b334f2bf5db90a343f96ab28c4e8ed88ac613e694f"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.zero
    val prevout = TransactionOutput.fromHex(
      "bd2897010000000022512088ffcb569bf1dc1a9e0b22b8cb164c31bcf65e6e95fa113ddbdbbead6e85fedf")
    val prevOutputs = Vector(prevout)
    val outputMap: PreviousOutputMap =
      PreviousOutputMap(
        spendingTx.inputs.map(_.previousOutput).zip(prevOutputs).toMap)
    val witnessStackHex = Vector(
      "f14afc2bf9b4a9f8e4b5e8503e396de9ad12aacf7726fd53dc147978f52bf9435d658cb326f533b39c57af2c8406f5177120eda69e51f52e0fd076040c7d831d83",
      "50d8"
    )
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = TaprootKeyPath.fromStack(witnessStack.reverse)

    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx,
                                                      inputIndex,
                                                      outputMap,
                                                      Policy.standardFlags)

    val annexHashHex =
      "b0d1ad766994166c07455739e8a49adf25972a25046ede7f1573ec61b75beb3f"
    val annexHash = Sha256Digest.fromHex(annexHashHex)
    val taprootOptions =
      TaprootSerializationOptions(tapLeafHashOpt = None,
                                  annexHashOpt = Some(annexHash),
                                  codeSeparatorPosOpt = None)
    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashSingleAnyoneCanPay,
      taprootOptions)

    assert(serialize.toHex == expected)
  }

  it must "serialize a taproot keyspend SIGHASH_ALL_ANYONECANSPEND" in {
    val expected =
      "00816c780499da010000a0e6c05b4d684eeb145d9225f15319226c5fd78617f5db646630c0584662a6a3008c033b67f78dfe0867489e0e5a032f24d14ee3c57637c22e71701fd79b64012ad200000094037f010000000022512057c1162a56ec9db80a8eb342634f613c8d990bf305925df7ddb85b356ce8f0bb7138b5c8"

    val spendingTxHex =
      "6c780499018c033b67f78dfe0867489e0e5a032f24d14ee3c57637c22e71701fd79b64012ad2000000007138b5c80168666e0000000000160014e7f8b1de86947bf379378ebd4368d37361fac307da010000"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.zero
    val prevout = TransactionOutput.fromHex(
      "94037f010000000022512057c1162a56ec9db80a8eb342634f613c8d990bf305925df7ddb85b356ce8f0bb")
    val prevOutputs = Vector(prevout)
    val outputMap: PreviousOutputMap =
      PreviousOutputMap(
        spendingTx.inputs.map(_.previousOutput).zip(prevOutputs).toMap)
    val witnessStackHex = Vector(
      "228187c314a903fe94c1c8260c243e0949a3233d404a58f90cd991a44f5dad4d89bd5763525b437a10a973abd8eb99adcfec9e2865fa235fa4d6af82c427f17a81")
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = TaprootKeyPath.fromStack(witnessStack.reverse)

    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx,
                                                      inputIndex,
                                                      outputMap,
                                                      Policy.standardFlags)

    val taprootOptions = TaprootSerializationOptions.empty

    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashAllAnyoneCanPay,
      taprootOptions)

    assert(serialize.toHex == expected)
  }

  it must "serialize a taproot transaction keyspend with SIGHASH_SINGLE with annex" in {
    val expected =
      "000302000000750000002495d592e51050fd51bb6470a99a487867735b70a0642ae47fd647dc45a5e29de5a234bdb1af9478ccd770cd2e9c72c181c16c08b2c94165a4b88524841ce15f6495778e1082884cb1f9952e2413855a598adb17cb62270a773cbe70348233b887ec369281e8627d6baa8f78e1c8ce1996f24a3fb71af38cc7303defce31883a0100000000eee244e957e1df84bc631fa02a0b7d0ef2fea17e7f5aad4a2aaa50a6aca16eee002e88213eefc990d8cc8115f39bcb43ffc09442f8923e716c71624dff0f53e4"

    val spendingTxHex =
      "020000000123b7a6b840cce4b0d7885738989f92801f48356986dc89bde06bb26813399a945c00000000bef901970387415001000000001976a914d4cdd5162868a9a0b9cf22921b3aa37967b6fb3288ac580200000000000017a9141330bf6152525e123d31a5c571baf641e4cceee8875802000000000000160014756476d75f3facf0051d02a6f46095e2eb92e85675000000"
    val spendingTx = Transaction.fromHex(spendingTxHex)
    val inputIndex = UInt32.zero
    val prevout = TransactionOutput.fromHex(
      "bf72520100000000225120b38f5c375c5df5852727048cd6d2769430afedabafa5897df1636cb87fecc14f")
    val prevOutputs = Vector(prevout)
    val outputMap: Map[TransactionOutPoint, TransactionOutput] =
      spendingTx.inputs.map(_.previousOutput).zip(prevOutputs).toMap
    val prevOutputMap = PreviousOutputMap(outputMap)
    val witnessStackHex = Vector(
      "d0630a93b0b781658c98c0a2fa53b72067c1de947ff5d73bb98b9c88066928f40ab1db157392d56ce1e137ed7121ae292697a9e29e1c8f26b993c8e839a6839403",
      "50c2ac"
    )
    val witnessStack = witnessStackHex.map(w => ByteVector.fromValidHex(w))

    val witness = TaprootKeyPath.fromStack(witnessStack.reverse)

    val witnessTx =
      WitnessTransaction
        .toWitnessTx(spendingTx)
        .updateWitness(inputIndex.toInt, witness)

    val taprootTxSigComponent = TaprootTxSigComponent(witnessTx, //spendingTx,
                                                      inputIndex,
                                                      prevOutputMap,
                                                      Policy.standardFlags)

    val annexHashHex =
      "eee244e957e1df84bc631fa02a0b7d0ef2fea17e7f5aad4a2aaa50a6aca16eee"
    val annexHash = Sha256Digest.fromHex(annexHashHex)
    val taprootOptions =
      TaprootSerializationOptions(None, Some(annexHash), None)
    val serialize = TransactionSignatureSerializer.serializeForSignature(
      taprootTxSigComponent,
      HashType.sigHashSingle,
      taprootOptions)

    assert(serialize.toHex == expected)
  }

}
