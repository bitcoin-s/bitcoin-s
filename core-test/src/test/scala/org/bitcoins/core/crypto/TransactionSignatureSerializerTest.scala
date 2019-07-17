package org.bitcoins.core.crypto

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int32, Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util._
import org.scalatest.{FlatSpec, MustMatchers}

import scala.util.Try

/**
  * Created by chris on 2/19/16.
  */
class TransactionSignatureSerializerTest extends FlatSpec with MustMatchers {
  private def logger = BitcoinSLogger.logger

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
    val serializedTxForSig: String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(txSigComponent,
                                                           HashType.sigHashAll))

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
    val serializedTxForSig: String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer
        .serializeForSignature(txSigComponent, HashType.sigHashSingle))
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
    val serializedTxForSig: String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer
        .serializeForSignature(txSigComponent, HashType.sigHashSingle))
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
    val serializedTxForSig: String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(txSigComponent,
                                                           HashType.sigHashAll))
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
    val serializedTxForSig: String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(txSigComponent,
                                                           HashType.sigHashAll))
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
    val amount = Satoshis(Int64(2000))
    val txSigComponent =
      WitnessTxSigComponentRaw(wtx,
                               inputIndex,
                               TransactionOutput(amount, witScriptPubKey),
                               Policy.standardFlags)

    val serializedForSig = TransactionSignatureSerializer.serializeForSignature(
      txSigComponent,
      HashType.sigHashSingleAnyoneCanPay)

    BitcoinSUtil.encodeHex(serializedForSig) must be(
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

    val uwtx = WitnessTransaction(ubtx.version,
                                  updatedInputs,
                                  ubtx.outputs,
                                  ubtx.lockTime,
                                  EmptyWitness)

    val wtxSigComp = {
      WitnessTxSigComponentP2SH(transaction = uwtx,
                                inputIndex = inputIndex,
                                output = output,
                                flags = Policy.standardFlags)
    }

    val serialized = TransactionSignatureSerializer.serializeForSignature(
      txSigComponent = wtxSigComp,
      hashType = HashType.sigHashAll)

    BitcoinSUtil.encodeHex(serialized) must be(expected)

    val hash = TransactionSignatureSerializer.hashForSignature(
      txSigComponent = wtxSigComp,
      hashType = HashType.sigHashAll)

    hash must be(expectedHash)
  }

  it must "serialize the BIP143 p2sh(p2wsh) examples" in {
    //https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#p2sh-p2wsh

    val unsignedTx = "010000000136641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e0100000000ffffffff" +
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
      hashType = HashType.sigHashAll)

    serialization.toHex must be(expectedSerialization)

    //with a SIGHASH_NONE

    val expectedSigHashNone =
      "0100000074afdc312af5183c4198a40ca3c1a275b485496dd3929bca388c4b5e31f7aaa0000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffff00000000000000000000000000000000000000000000000000000000000000000000000002000000"

    val serializationSigHashNone =
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashNone)

    serializationSigHashNone.toHex must be(expectedSigHashNone)

    //with sighash single

    val expectedSigHashSingle =
      "0100000074afdc312af5183c4198a40ca3c1a275b485496dd3929bca388c4b5e31f7aaa0000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffff9efe0c13a6b16c14a41b04ebe6a63f419bdacb2f8705b494a43063ca3cd4f7080000000003000000"

    val serializationSigHashSingle =
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashSingle)

    serializationSigHashSingle.toHex must be(expectedSigHashSingle)

    val expectedSigHashAllAnyoneCanPay =
      "010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffffbc4d309071414bed932f98832b27b4d76dad7e6c1346f487a8fdbb8eb90307cc0000000081000000"

    val serializationSigHashAllAnyoneCanPay =
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashAllAnyoneCanPay)

    serializationSigHashAllAnyoneCanPay.toHex must be(
      expectedSigHashAllAnyoneCanPay)

    val expectedSigHashNoneAnyoneCanPay =
      "010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffff00000000000000000000000000000000000000000000000000000000000000000000000082000000"

    val serializationSigHashNoneAnyoneCanPay = {
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashNoneAnyoneCanPay)
    }

    serializationSigHashNoneAnyoneCanPay.toHex must be(
      expectedSigHashNoneAnyoneCanPay)

    val expectedSigHashSingleAnyoneCanPay =
      "010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000036641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e01000000cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56aeb168de3a00000000ffffffff9efe0c13a6b16c14a41b04ebe6a63f419bdacb2f8705b494a43063ca3cd4f7080000000083000000"
    val serializationSigHashSingleAnyoneCanPay = {
      TransactionSignatureSerializer.serializeForSignature(
        txSigComponent = wtxSigComp,
        hashType = HashType.sigHashSingleAnyoneCanPay)
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
    val amount = Satoshis(Int64(200000))
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
      hashType = HashType.sigHashAll)

    val expectedSerialization =
      "01000000b67c76d200c6ce72962d919dc107884b9d5d0e26f2aea7474b46a1904c53359f3bb13029ce7b1f559ef5e747fcac439f1455a2ec7c5f09b72290795e7066504469c12106097dc2e0526493ef67f21269fe888ef05c7a3a5dacab38e1ac8387f14c1d00004aad4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01400d030000000000ffffffffe5d196bfb21caca9dbd654cafb3b4dc0c4882c8927d2eb300d9539dd0b9342280000000001000000"

    serialized.toHex must be(expectedSerialization)
  }

  it must "fail to create a SIGHASH from an invalid number" in {
    val z = Int32.zero
    Try(SIGHASH_NONE(z)).isFailure must be(true)
    Try(SIGHASH_SINGLE(z)).isFailure must be(true)
    Try(SIGHASH_ANYONECANPAY(z)).isFailure must be(true)
    Try(SIGHASH_ALL_ANYONECANPAY(z)).isFailure must be(true)
    Try(SIGHASH_NONE_ANYONECANPAY(z)).isFailure must be(true)
    Try(SIGHASH_SINGLE_ANYONECANPAY(z)).isFailure must be(true)
  }
}
