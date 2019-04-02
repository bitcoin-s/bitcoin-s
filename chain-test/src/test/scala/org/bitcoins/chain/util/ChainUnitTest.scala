package org.bitcoins.chain.util

import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.db.{DbConfig, UnitTestDbConfig}
import org.bitcoins.testkit.chain.ChainTestUtil
import org.scalatest.{
  AsyncFlatSpec,
  BeforeAndAfter,
  BeforeAndAfterAll,
  MustMatchers
}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

trait ChainUnitTest
    extends AsyncFlatSpec
    with MustMatchers
    with BeforeAndAfter
    with BeforeAndAfterAll {

  val timeout = 10.seconds
  def dbConfig: DbConfig = UnitTestDbConfig
  val genesisHeader: BlockHeaderDb = ChainTestUtil.regTestGenesisHeaderDb
  val networkParam: RegTestNetChainParams.type = RegTestNetChainParams

  implicit def ec: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global
}
