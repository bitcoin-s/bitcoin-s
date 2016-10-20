package org.bitcoins.core.gen

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.scalacheck.Gen

import scala.annotation.tailrec

/**
  * Created by tom on 7/6/16.
  */
trait BlockchainElementsGenerator {
  /** Generates a random [[Block]], note that we limit this
    * to 10 transactions currently */
  def block : Gen[Block] = for {
    header <- blockHeader
    randomNum <- Gen.choose(1,10)
    txs <- Gen.listOfN(randomNum, TransactionGenerators.transactions)
  } yield Block(header, txs)


  /** Generates a random [[BlockHeader]] */
  def blockHeader : Gen[BlockHeader] = for {
    previousBlockHash <- CryptoGenerators.doubleSha256Digest
    b <- blockHeader(previousBlockHash)
  } yield b

  /** Generates a random [[BlockHeader]] with the specified previousBlockHash */
  def blockHeader(previousBlockHash: DoubleSha256Digest): Gen[BlockHeader] = for {
    nBits <- NumberGenerator.uInt32s
    b <- blockHeader(previousBlockHash,nBits)
  } yield b

  /** Generates a random [[BlockHeader]] where you can specify the previousBlockHash and nBits */
  def blockHeader(previousBlockHash: DoubleSha256Digest, nBits: UInt32): Gen[BlockHeader] = for {
    version <- NumberGenerator.uInt32s
    merkleRootHash <- CryptoGenerators.doubleSha256Digest
    time <- NumberGenerator.uInt32s
    nonce <- NumberGenerator.uInt32s
  } yield BlockHeader(version, previousBlockHash,merkleRootHash,time,nBits,nonce)

  /** Generates a chain of valid headers of the size specified by num,
    * 'valid' means their nBits are the same and each header properly
    * references the previous block header's hash */
  def validHeaderChain(num: Long): Gen[Seq[BlockHeader]] = {
    val startHeader = blockHeader.sample.get
    validHeaderChain(num,startHeader)
  }

  def validHeaderChain(num: Long, startHeader: BlockHeader): Gen[Seq[BlockHeader]] = {
    @tailrec
    def loop(remainingHeaders: Long, accum: Seq[BlockHeader]): Seq[BlockHeader] = {
      if (remainingHeaders == 0) accum.reverse
      else {
        val nextHeader = blockHeader(accum.head.hash,accum.head.nBits).sample.get
        loop(remainingHeaders-1, nextHeader +: accum)
      }
    }
    loop(num-1, Seq(startHeader))
  }
}

object BlockchainElementsGenerator extends BlockchainElementsGenerator
