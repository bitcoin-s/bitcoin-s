package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.serializers.RawBitcoinSerializer

import scala.annotation.tailrec

/**
  * Created by chris on 5/20/16.
  * Responsible for serializing blocks in our blockchain
  * https://bitcoin.org/en/developer-reference#serialized-blocks
  */
trait RawBlockSerializer extends RawBitcoinSerializer[Block] {

  /**
    * Takes a list of bytes and converts it into a Block
    *
    * @param bytes the bytes to be converted to a block
    * @return the block object parsed from the list of bytes
    */
  def read(bytes : List[Byte]) : Block = {
    val blockHeader : BlockHeader = BlockHeader(bytes.take(80))
    val txCount : CompactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(bytes.slice(80, bytes.length))
    val txBytes : Seq[Byte] = bytes.slice((80 + txCount.size).toInt, bytes.size)
    val (transactions, remainingBytes) = parseBlockTransactions(txCount, txBytes)
    Block(blockHeader, txCount, transactions)
  }

  /**
    * Takes in a block and converts it to a hexadecimal string
    *
    * @param block the block that needs to be converted to a hexadecimal string
    * @return the hexadecimal string representing a block
    */
  def write(block : Block) : String = {
    val writtenHeader = block.blockHeader.hex
    val uInt = block.txCount.hex
    val writtenTransactions = block.transactions.map(_.hex).mkString
    writtenHeader + uInt + writtenTransactions
  }

  private def parseBlockTransactions(txCount : CompactSizeUInt, bytes : Seq[Byte]) : (Seq[Transaction], Seq[Byte]) = {
    @tailrec
    def loop(remainingTxs : BigInt, remainingBytes : Seq[Byte], accum : List[Transaction]) : (Seq[Transaction], Seq[Byte]) = {
      if (remainingTxs <= 0) {
        (accum.reverse, remainingBytes)
      } else {
        val transaction = Transaction(remainingBytes)
        val newRemainingBytes = remainingBytes.slice(transaction.size, remainingBytes.size)
        loop(remainingTxs - 1, newRemainingBytes, transaction :: accum)
      }
    }
    loop(txCount.num.toLong, bytes, List())
  }

}

object RawBlockSerializer extends RawBlockSerializer