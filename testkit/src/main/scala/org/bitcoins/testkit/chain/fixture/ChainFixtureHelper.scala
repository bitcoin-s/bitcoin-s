package org.bitcoins.testkit.chain.fixture

import org.bitcoins.core.util.FutureUtil
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.chain.fixture.ChainFixture.{
  BitcoindZmqChainHandlerWithBlock,
  Empty,
  GenesisChainHandlerCachedWithGenesisFilters,
  GenesisChainHandlerWithGenesisFilters,
  GenisisBlockHeaderDAO,
  GenisisChainHandler,
  PopulatedBlockHeaderDAO,
  PopulatedChainHandler
}

import scala.concurrent.Future

trait ChainFixtureHelper { this: ChainUnitTest =>

  def createFixture(tag: ChainFixtureTag): Future[ChainFixture] = {
    tag match {
      case ChainFixtureTag.Empty => Future.successful(ChainFixture.Empty)
      case ChainFixtureTag.GenisisBlockHeaderDAO =>
        ChainUnitTest.createBlockHeaderDAO().map(GenisisBlockHeaderDAO.apply)

      case ChainFixtureTag.PopulatedBlockHeaderDAO =>
        ChainUnitTest
          .createPopulatedBlockHeaderDAO()
          .map(PopulatedBlockHeaderDAO.apply)
      case ChainFixtureTag.GenisisChainHandler =>
        ChainUnitTest.createChainHandler().map(GenisisChainHandler.apply)
      case ChainFixtureTag.PopulatedChainHandler =>
        createPopulatedChainHandler().map(
          ChainFixture.PopulatedChainHandler.apply)
      case ChainFixtureTag.GenesisChainHandlerWithFilter =>
        createChainHandlerWithGenesisFilter()
          .map(ChainFixture.GenesisChainHandlerWithGenesisFilters(_))
      case ChainFixtureTag.GenesisChainHandlerCachedWithFilter =>
        createChainHandlerCachedWithGenesisFilter()
          .map(ChainFixture.GenesisChainHandlerCachedWithGenesisFilters(_))
      case ChainFixtureTag.BitcoindZmqChainHandlerWithBlock =>
        createBitcoindChainHandlerViaZmq().map(
          BitcoindZmqChainHandlerWithBlock.apply)
    }
  }

  def destroyFixture(fixture: ChainFixture): Future[Any] = {
    fixture match {
      case Empty                      => FutureUtil.unit
      case GenisisBlockHeaderDAO(_)   => ChainUnitTest.destroyAllTables()
      case PopulatedBlockHeaderDAO(_) => ChainUnitTest.destroyAllTables()
      case GenisisChainHandler(_)     => ChainUnitTest.destroyAllTables()
      case GenesisChainHandlerWithGenesisFilters(_) =>
        ChainUnitTest.destroyAllTables()
      case GenesisChainHandlerCachedWithGenesisFilters(_) =>
        ChainUnitTest.destroyAllTables()
      case PopulatedChainHandler(_) => ChainUnitTest.destroyAllTables()
      case BitcoindZmqChainHandlerWithBlock(bitcoindHandler) =>
        destroyBitcoindChainHandlerViaZmq(bitcoindHandler)
    }
  }
}
