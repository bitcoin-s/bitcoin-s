package org.bitcoins.testkit.chain.fixture

import org.scalatest.Tag

/**
  * If a test file uses ChainFixture as its FixtureParam, then
  * using these tags will determine which fixture the test will get.
  *
  * Simply add taggedAs FixtureTag._ to your test before calling inFixtured.
  */
sealed abstract class ChainFixtureTag(name: String) extends Tag(name)

object ChainFixtureTag {
  case object Empty extends ChainFixtureTag("Empty")

  case object GenisisBlockHeaderDAO
      extends ChainFixtureTag("GenisisBlockHeaderDAO")

  case object PopulatedBlockHeaderDAO
      extends ChainFixtureTag("PopulatedBlockHeaderDAO")

  case object GenisisChainHandler extends ChainFixtureTag("GenisisChainHandler")

  case object PopulatedChainHandler
      extends ChainFixtureTag("PopulatedChainHandler")

  case object BitcoindZmqChainHandlerWithBlock
      extends ChainFixtureTag("BitcoindZmqChainHandlerWithBlock")

  val defaultTag: ChainFixtureTag = ChainFixtureTag.Empty

  def from(tag: String): ChainFixtureTag = {
    tag match {
      case Empty.name                   => Empty
      case GenisisBlockHeaderDAO.name   => GenisisBlockHeaderDAO
      case PopulatedBlockHeaderDAO.name => PopulatedBlockHeaderDAO
      case GenisisChainHandler.name     => GenisisChainHandler
      case PopulatedChainHandler.name   => PopulatedChainHandler
      case BitcoindZmqChainHandlerWithBlock.name =>
        BitcoindZmqChainHandlerWithBlock
      case _: String =>
        throw new IllegalArgumentException(s"$tag is not a valid tag")
    }
  }
}
