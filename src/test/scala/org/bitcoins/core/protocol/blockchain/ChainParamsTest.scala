package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.currency.Bitcoins
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 5/24/16.
  */
class ChainParamsTest extends FlatSpec with MustMatchers {

  "ChainParams" must "create the bitcoin genesis block" in {
    val genesisBlock = MainNetChainParams.genesisBlock
    genesisBlock.hash must be ("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")
  }

  it must "compute the script signature for the coinbase tx in the mainnet genesis block" in {
    val genesisBlock = MainNetChainParams.genesisBlock
    val scriptSig = genesisBlock.transactions.head.inputs.head.scriptSignature
    scriptSig.hex must be ("04FFFF001D0104455468652054696D65732030332F4A616E2F32303039204368616E63656C6C6F72206F6E206272696E6B206F66207365636F6E64206261696C6F757420666F722062616E6B73".toLowerCase())
  }
}
