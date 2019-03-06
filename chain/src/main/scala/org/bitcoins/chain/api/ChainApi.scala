package org.bitcoins.chain.api

import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.BlockHeader

import scala.concurrent.Future

/**
  * Entry api to the chain project for adding new things to our blockchain
  */
trait ChainApi {

  /**
    * Adds a block header to our chain project
    * @param header
    * @return
    */
  def processHeader(header: BlockHeader): Future[ChainApi]

  /** Get's a [[org.bitcoins.chain.models.BlockHeaderDb]] from the chain's database */
  def getHeader(hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]]

}
