package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.currency.Bitcoins
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 5/24/16.
  */
class ChainParamsTest extends FlatSpec with MustMatchers {

  "ChainParams" must "create the bitcoin genesis block" in {
    val genesisBlock = MainNetChainParams.createGenesisBlock(1231006505, 2083236893, 0x1d00ffff, 1, Bitcoins(50))
    genesisBlock.hash must be ("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")
  }
}
