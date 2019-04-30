package org.bitcoins.chain.util

import org.bitcoins.chain.util.ChainFixture.{BitcoindZmqChainHandlerWithBlock, Empty, GenisisBlockHeaderDAO, GenisisChainHandler, PopulatedBlockHeaderDAO}

import scala.concurrent.Future

trait ChainFixtureHelper { this : ChainUnitTest =>


  def createFixture(tag: ChainFixtureTag): Future[ChainFixture] = {
    tag match {
      case ChainFixtureTag.Empty => Future.successful(ChainFixture.Empty)
      case ChainFixtureTag.GenisisBlockHeaderDAO =>
        createBlockHeaderDAO().map(GenisisBlockHeaderDAO.apply)
      case ChainFixtureTag.PopulatedBlockHeaderDAO =>
        createPopulatedBlockHeaderDAO().map(PopulatedBlockHeaderDAO.apply)
      case ChainFixtureTag.GenisisChainHandler =>
        createChainHandler().map(GenisisChainHandler.apply)
      case ChainFixtureTag.BitcoindZmqChainHandlerWithBlock =>
        createBitcoindChainHandlerViaZmq().map(
          BitcoindZmqChainHandlerWithBlock.apply)
    }
  }

  def destroyFixture(fixture: ChainFixture): Future[Any] = {
    fixture match {
      case Empty                      => Future.successful(())
      case GenisisBlockHeaderDAO(_)   => destroyHeaderTable()
      case PopulatedBlockHeaderDAO(_) => destroyHeaderTable()
      case GenisisChainHandler(_)     => destroyHeaderTable()
      case BitcoindZmqChainHandlerWithBlock(bitcoindHandler) =>
        destroyBitcoindChainHandlerViaZmq(bitcoindHandler)
    }
  }
}