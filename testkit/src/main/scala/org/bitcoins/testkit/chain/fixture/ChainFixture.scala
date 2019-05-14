package org.bitcoins.testkit.chain.fixture

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.models.BlockHeaderDAO

/**
  * This ADT represents all Chain test fixtures. If you set this type to be your
  * FixtureParam and override withFixture to be withChainFixutre, then simply tag
  * tests to specify which fixture that test should receive and then use inFixutred
  * which takes a PartialFunction[ChainFixture, Future[Assertion] ] (i.e. just
  * specify the relevant case for your expected fixture)
  */
sealed trait ChainFixture

object ChainFixture {
  case object Empty extends ChainFixture

  case class GenisisBlockHeaderDAO(dao: BlockHeaderDAO) extends ChainFixture

  case class PopulatedBlockHeaderDAO(dao: BlockHeaderDAO) extends ChainFixture

  case class GenisisChainHandler(chainHandler: ChainHandler)
      extends ChainFixture

  case class PopulatedChainHandler(chainHandler: ChainHandler)
      extends ChainFixture

  case class BitcoindZmqChainHandlerWithBlock(
      bitcoindChainHandler: BitcoindChainHandlerViaZmq)
      extends ChainFixture
}
