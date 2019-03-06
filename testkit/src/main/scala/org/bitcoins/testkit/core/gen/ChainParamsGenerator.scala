package org.bitcoins.testkit.core.gen

import org.bitcoins.core.config._
import org.bitcoins.core.protocol.ln.LnParams
import org.scalacheck.Gen

/**
  * Created by chris on 6/6/17.
  */
sealed abstract class ChainParamsGenerator {

  def networkParams: Gen[NetworkParameters] = bitcoinNetworkParams

  def bitcoinNetworkParams: Gen[BitcoinNetwork] =
    Gen.oneOf(MainNet, TestNet3, RegTest)

  def lnNetworkParams: Gen[LnParams] = {
    Gen.oneOf(LnParams.LnBitcoinMainNet,
              LnParams.LnBitcoinTestNet,
              LnParams.LnBitcoinRegTest)
  }
}

object ChainParamsGenerator extends ChainParamsGenerator
