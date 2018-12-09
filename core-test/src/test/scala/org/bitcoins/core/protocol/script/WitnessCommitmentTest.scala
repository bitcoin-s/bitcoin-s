package org.bitcoins.core.protocol.script

import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 12/23/16.
  */
class WitnessCommitmentTest extends FlatSpec with MustMatchers {

  //witness commitment from https://www.blocktrail.com/tBTC/block/00000000000002f59cc8b806b2cf6bbe37a367a085de60f9e5e3386081abbb48
  val hex =
    "356a24aa21a9ed309cfb38d1015c266667d5b7888c83def872a531b8ac277fe8df623c32b562b50e6d696e65642062792062636f696e"
  "WitnessCommitment" must "be able to parse a witness commitment from testnet3" in {
    val commitment = ScriptPubKey(hex)
    commitment.isInstanceOf[WitnessCommitment] must be(true)
  }

  it must "find the correct witness root hash in a witness commitment" in {
    val commitment = WitnessCommitment(hex)
    commitment.witnessRootHash.hex must be(
      "309cfb38d1015c266667d5b7888c83def872a531b8ac277fe8df623c32b562b5")
  }
}
