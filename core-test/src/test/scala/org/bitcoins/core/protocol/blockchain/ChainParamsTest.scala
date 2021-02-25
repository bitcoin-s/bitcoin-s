package org.bitcoins.core.protocol.blockchain

import java.math.BigInteger
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  CoinbaseInput,
  TransactionConstants,
  TransactionOutput
}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 5/24/16.
  */
class ChainParamsTest extends BitcoinSUnitTest {
  val genesisBlock = MainNetChainParams.genesisBlock
  val genesisTransaction = genesisBlock.transactions.head

  val expectedGenesisScriptSig = ScriptSignature(
    "4D04FFFF001D0104455468652054696D65732030332F4A616E2F32303039204368616E63656C6C6F72206F6E206272696E6B206F66207365636F6E64206261696C6F757420666F722062616E6B73"
      .toLowerCase())

  val expectedGenesisInput =
    CoinbaseInput(expectedGenesisScriptSig, TransactionConstants.sequence)

  val expectedGenesisScriptPubKey = ScriptPubKey(
    "434104678AFDB0FE5548271967F1A67130B7105CD6A828E03909A67962E0EA1F61DEB649F6BC3F4CEF38C4F35504E51EC112DE5C384DF7BA0B8D578A4C702B6BF11D5FAC".toLowerCase)

  val expectedGenesisOutput =
    TransactionOutput(Satoshis(5000000000L), expectedGenesisScriptPubKey)
  "ChainParams" must "generate correct block hex for genesis block" in {
    val hex = "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e6" +
      "7768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c010100000001000000000000000000000000000" +
      "0000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303" +
      "039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73fff" +
      "fffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3" +
      "f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000"
    genesisBlock.hex must be(hex)
  }
  it must "hash the bitcoin genesis block" in {
    genesisBlock.blockHeader.hash.hex must be(
      "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000")
  }

  it must "compute the script signature for the coinbase tx in the mainnet genesis block" in {
    val scriptSig = genesisBlock.transactions.head.inputs.head.scriptSignature
    scriptSig.hex must be(expectedGenesisScriptSig.hex)
  }

  it must "generate the input correctly for the genesis transaction's input" in {
    val input = genesisBlock.transactions.head.inputs.head
    input must be(expectedGenesisInput)
    input.hex must be(
      "0000000000000000000000000000000000000000000000000000000000000000FFFFFFFF".toLowerCase
        + expectedGenesisScriptSig.hex + "FFFFFFFF".toLowerCase)
  }

  it must "generate the correct scriptPubKey for the genesis transaction's output" in {
    val scriptPubKey = genesisTransaction.outputs.head.scriptPubKey
    scriptPubKey.hex must be(expectedGenesisScriptPubKey.hex)
  }

  it must "generate the output correctly for the genesis transaction's output" in {
    val output = genesisTransaction.outputs.head
    output.value must be(Satoshis(5000000000L))
    output.scriptPubKey.hex must be(expectedGenesisScriptPubKey.hex)
    output.hex must be(
      "00F2052A01000000".toLowerCase + expectedGenesisScriptPubKey.hex)
  }

  it must "generate the correct txid for the genesis transaction" in {
    genesisTransaction.version must be(TransactionConstants.version)
    genesisTransaction.lockTime must be(TransactionConstants.lockTime)
    genesisTransaction.inputs must be(Seq(expectedGenesisInput))
    genesisTransaction.outputs must be(Seq(expectedGenesisOutput))

    genesisTransaction.txId.hex must be(
      BytesUtil.flipEndianness(
        "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"))
  }

  it must "generate the correct merkle root for the testnet genesis block" in {
    TestNetChainParams.genesisBlock.blockHeader.merkleRootHash.hex must be(
      BytesUtil.flipEndianness(
        "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"))
  }

  it must "generate the correct block hash for the testnet genesis block" in {
    TestNetChainParams.genesisBlock.blockHeader.hash.hex must be(
      BytesUtil.flipEndianness(
        "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
  }

  it must "generate the correct merkle root for the regtest genesis block" in {
    RegTestNetChainParams.genesisBlock.blockHeader.merkleRootHash.hex must be(
      BytesUtil.flipEndianness(
        "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"))
  }

  it must "generate the correct merkle root for the signet genesis block" in {
    SigNetChainParams().genesisBlock.blockHeader.merkleRootHashBE.hex must be(
      "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
  }

  it must "generate the correct merkle root for the signet genesis block with different challenge spks" in {
    val default = ScriptPubKey.fromAsmHex(
      "512103ad5e0edad18cb1f0fc0d28a3d4f1f3e445640337489abb10404f2d1e086be430210359ef5021964fe22d6f8e05b2463c9540ce96883fe3b278760f048f5189f2e6c452ae")
    SigNetChainParams(
      default).genesisBlock.blockHeader.merkleRootHashBE.hex must be(
      "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")

    val p2pkh = P2PKHScriptPubKey(ECPublicKey.freshPublicKey)
    SigNetChainParams(
      p2pkh).genesisBlock.blockHeader.merkleRootHashBE.hex must be(
      "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")

    val trivialTrue = ScriptPubKey.fromAsm(Seq(OP_TRUE))
    SigNetChainParams(
      trivialTrue).genesisBlock.blockHeader.merkleRootHashBE.hex must be(
      "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
  }

  it must "generate the correct block header hex for the regtest genesis block" in {
    val regtestGenesisBlockHex =
      "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4adae5494dffff7f20020000000101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000"
    val expectedHeaderHex = regtestGenesisBlockHex.slice(0, 160)
    RegTestNetChainParams.genesisBlock.blockHeader.hex must be(
      expectedHeaderHex)
  }
  it must "generate the correct block hex for the regtest genesis block" in {
    val expectedHex =
      "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4adae5494dffff7f20020000000101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000"
    RegTestNetChainParams.genesisBlock.hex must be(expectedHex)
  }
  it must "generate the correct blockheader hash for the genesis block on regtest" in {
    logger.debug("Regtest genesis block: " + RegTestNetChainParams.genesisBlock)
    RegTestNetChainParams.genesisBlock.blockHeader.hash.hex must be(
      BytesUtil.flipEndianness(
        "0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206"))
  }

  it must "have the correct base58 prefix for MainNet" in {
    import Base58Type._
    //correct answers taken from https://en.bitcoin.it/wiki/List_of_address_prefixes
    BytesUtil.encodeHex(
      MainNetChainParams.base58Prefixes(PubKeyAddress)) must be("00")
    BytesUtil.encodeHex(
      MainNetChainParams.base58Prefixes(ScriptAddress)) must be("05")
    BytesUtil.encodeHex(MainNetChainParams.base58Prefixes(SecretKey)) must be(
      "80")
    BytesUtil.encodeHex(
      MainNetChainParams.base58Prefixes(ExtPublicKey)) must be(
      "0488B21E".toLowerCase)
    BytesUtil.encodeHex(
      MainNetChainParams.base58Prefixes(ExtSecretKey)) must be(
      "0488ADE4".toLowerCase)
  }

  it must "have the correct base58 prefix for TestNet" in {
    import Base58Type._
    BytesUtil.encodeHex(
      TestNetChainParams.base58Prefixes(PubKeyAddress)) must be("6f")
    BytesUtil.encodeHex(
      TestNetChainParams.base58Prefixes(ScriptAddress)) must be("c4")
    BytesUtil.encodeHex(TestNetChainParams.base58Prefixes(SecretKey)) must be(
      "ef")
    BytesUtil.encodeHex(
      TestNetChainParams.base58Prefixes(ExtPublicKey)) must be(
      "043587CF".toLowerCase)
    BytesUtil.encodeHex(
      TestNetChainParams.base58Prefixes(ExtSecretKey)) must be(
      "04358394".toLowerCase)
  }

  it must "have the correct base58 prefix for RegTest" in {
    import Base58Type._
    BytesUtil.encodeHex(
      RegTestNetChainParams.base58Prefixes(PubKeyAddress)) must be("6f")
    BytesUtil.encodeHex(
      RegTestNetChainParams.base58Prefixes(ScriptAddress)) must be("c4")
    BytesUtil.encodeHex(
      RegTestNetChainParams.base58Prefixes(SecretKey)) must be("ef")
    BytesUtil.encodeHex(
      RegTestNetChainParams.base58Prefixes(ExtPublicKey)) must be(
      "043587CF".toLowerCase)
    BytesUtil.encodeHex(
      RegTestNetChainParams.base58Prefixes(ExtSecretKey)) must be(
      "04358394".toLowerCase)
  }

  it must "have the correct base58 prefix for SigNet" in {
    import Base58Type._
    BytesUtil.encodeHex(
      SigNetChainParams().base58Prefixes(PubKeyAddress)) must be("6f")
    BytesUtil.encodeHex(
      SigNetChainParams().base58Prefixes(ScriptAddress)) must be("c4")
    BytesUtil.encodeHex(SigNetChainParams().base58Prefixes(SecretKey)) must be(
      "ef")
    BytesUtil.encodeHex(
      SigNetChainParams().base58Prefixes(ExtPublicKey)) must be(
      "043587CF".toLowerCase)
    BytesUtil.encodeHex(
      SigNetChainParams().base58Prefixes(ExtSecretKey)) must be(
      "04358394".toLowerCase)
  }

  it must "determine the correct POW intervals for bitcoin networks" in {
    MainNetChainParams.difficultyChangeInterval must be(2016)
    TestNetChainParams.difficultyChangeInterval must be(2016)
    RegTestNetChainParams.difficultyChangeInterval must be(2016)
    SigNetChainParams().difficultyChangeInterval must be(2016)
  }

  it must "determine what networks allow retargeting of proof of work" in {
    MainNetChainParams.noRetargeting must be(false)
    TestNetChainParams.noRetargeting must be(false)
    RegTestNetChainParams.noRetargeting must be(true)
    SigNetChainParams().noRetargeting must be(false)
  }

  it must "allow/not allow minimum difficulty blocks on certain networks" in {
    MainNetChainParams.allowMinDifficultyBlocks must be(false)
    TestNetChainParams.allowMinDifficultyBlocks must be(true)
    RegTestNetChainParams.allowMinDifficultyBlocks must be(true)
    SigNetChainParams().allowMinDifficultyBlocks must be(false)
  }

  it must "generate the correct default signet challenge" in {
    val challenge = SigNetChainParams().signetChallenge
    val pubKeys = Vector(
      ECPublicKey(
        "03ad5e0edad18cb1f0fc0d28a3d4f1f3e445640337489abb10404f2d1e086be430"),
      ECPublicKey(
        "0x0359ef5021964fe22d6f8e05b2463c9540ce96883fe3b278760f048f5189f2e6c4")
    )
    val expected = MultiSignatureScriptPubKey(1, pubKeys)

    assert(challenge == expected)
  }

  it must "compute the correct pow limits" in {
    val expectedTestMain = new BigInteger(
      "26959946667150639794667015087019630673637144422540572481103610249215",
      10)

    val expectedRegTest = new BigInteger(
      "57896044618658097711785492504343953926634992332820282019728792003956564819967",
      10)
    MainNetChainParams.powLimit must be(expectedTestMain)
    TestNetChainParams.powLimit must be(expectedTestMain)
    RegTestNetChainParams.powLimit must be(expectedRegTest)
  }
}
