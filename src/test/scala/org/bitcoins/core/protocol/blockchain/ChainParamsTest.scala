package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits}
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction.{TransactionConstants, TransactionInput, TransactionOutput}
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 5/24/16.
  */
class ChainParamsTest extends FlatSpec with MustMatchers {

  val genesisBlock = MainNetChainParams.genesisBlock
  val genesisTransaction = genesisBlock.transactions.head

  val expectedGenesisScriptSig = ScriptSignature("04FFFF001D0104455468652054696D65732030332F4A616E2F32303039204368616E63656C6C6F72206F6E206272696E6B206F66207365636F6E64206261696C6F757420666F722062616E6B73".toLowerCase())
  val expectedGenesisInput = TransactionInput(expectedGenesisScriptSig)
  val expectedGenesisScriptPubKey = ScriptPubKey("4104678AFDB0FE5548271967F1A67130B7105CD6A828E03909A67962E0EA1F61DEB649F6BC3F4CEF38C4F35504E51EC112DE5C384DF7BA0B8D578A4C702B6BF11D5FAC".toLowerCase)
  val expectedGenesisOutput = TransactionOutput(CurrencyUnits.toSatoshis(Bitcoins(50)),expectedGenesisScriptPubKey)
  "ChainParams" must "create the bitcoin genesis block" in {
    genesisBlock.hash must be ("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")
  }


  it must "compute the script signature for the coinbase tx in the mainnet genesis block" in {
    val scriptSig = genesisBlock.transactions.head.inputs.head.scriptSignature
    scriptSig.hex must be (expectedGenesisScriptSig.hex)
  }

  it must "generate the input correctly for the genesis transaction's input" in {
    val input = genesisBlock.transactions.head.inputs.head
    input must be (expectedGenesisInput)
    input.hex must be ("010000000000000000000000000000000000000000000000000000000000000000FFFFFFFF4D".toLowerCase
      + expectedGenesisScriptSig.hex + "FFFFFFFF".toLowerCase )
  }

  it must "generate the correct scriptPubKey for the genesis transaction's output" in {
    val scriptPubKey = genesisTransaction.outputs.head.scriptPubKey
    scriptPubKey.hex must be (expectedGenesisScriptPubKey.hex)
  }

  it must "generate the output correctly for the genesis transaction's output" in {
    val output = genesisTransaction.outputs.head
    output.value must be (Bitcoins(50))
    output.scriptPubKey.hex must be (expectedGenesisScriptPubKey.hex)
    output.hex must be ("0100F2052A0100000043".toLowerCase + expectedGenesisScriptPubKey.hex)
  }

  it must "generate the correct txid for the genesis transaction" in {
    genesisTransaction.version must be (TransactionConstants.version)
    genesisTransaction.lockTime must be (TransactionConstants.lockTime)
    genesisTransaction.inputs must be (Seq(expectedGenesisInput))
    genesisTransaction.outputs must be (Seq(expectedGenesisOutput))

    genesisTransaction.txId.hex must be ("4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
  }


  it must "generate the correctly merkle root for the testnet genesis block" in {
    TestNetChainParams.genesisBlock.merkleRoot.hex must be ("4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
  }
}
