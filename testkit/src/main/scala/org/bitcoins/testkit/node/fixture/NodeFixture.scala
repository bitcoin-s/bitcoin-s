package org.bitcoins.testkit.node.fixture

sealed trait NodeFixture

object NodeFixture {

  /** Gives us a fixture that has a spv node connected with the bitcoind instance */
  case class SpvNodeConnectedWithBitcoindFixture(
      spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind)
      extends NodeFixture
}
