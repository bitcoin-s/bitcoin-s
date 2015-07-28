package org.scalacoin

import org.bitcoinj.core.NetworkParameters
import org.bitcoinj.params.{RegTestParams, MainNetParams, TestNet3Params}

/**
 * Created by chris on 7/27/15.
 */
trait NetworkParametersWrapper {
  def network : NetworkParameters
}

trait MainNet extends NetworkParametersWrapper {
  override def network = MainNetParams.get
}

trait TestNet3 extends NetworkParametersWrapper {
  override def network = TestNet3Params.get
}

trait RegTest extends NetworkParametersWrapper {
  override def network = RegTestParams.get
}
