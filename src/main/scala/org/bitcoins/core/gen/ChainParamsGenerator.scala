package org.bitcoins.core.gen

import org.bitcoins.core.config._
import org.scalacheck.Gen

/**
  * Created by chris on 6/6/17.
  */
sealed abstract class ChainParamsGenerator {

  def networkParams: Gen[NetworkParameters] = bitcoinNetworkParams

  def bitcoinNetworkParams: Gen[BitcoinNetwork] = Gen.oneOf(MainNet, TestNet3, RegTest)
}

object ChainParamsGenerator extends ChainParamsGenerator
