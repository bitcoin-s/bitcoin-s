package org.bitcoins.core.gen

import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.scalacheck.Gen
import scala.annotation.tailrec

/**
  * Created by tom on 7/6/16.
  */
trait BlockchainElementsGenerator {
  def block : Gen[Block] = for {
    header <- blockHeader
    randomNum <- Gen.choose(1,10)
    txs <- Gen.listOfN(randomNum, TransactionGenerators.transactions)
  } yield Block(header, txs)


  def blockHeader : Gen[BlockHeader] = for {
    version <- NumberGenerator.uInt32s
    previousBlockHash <- CryptoGenerators.doubleSha256Digest
    merkleRootHash <- CryptoGenerators.doubleSha256Digest
    time <- NumberGenerator.uInt32s
    nBits <- NumberGenerator.uInt32s
    nonce <- NumberGenerator.uInt32s
  } yield BlockHeader(version, previousBlockHash, merkleRootHash, time, nBits, nonce)

}

object BlockchainElementsGenerator extends BlockchainElementsGenerator
