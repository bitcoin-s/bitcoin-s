package org.scalacoin.marshallers.transaction

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.transaction.{TransactionImpl, Transaction}
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/14/16.
 */
trait RawTransactionParser extends RawBitcoinSerializer[Transaction] {

  def read(bytes : List[Byte]) = {
    val versionBytes = bytes.take(4)
    val version = Integer.parseInt(ScalacoinUtil.encodeHex(versionBytes.reverse),16)
    val txInputBytes = bytes.slice(4,bytes.size)
    val inputs = RawTransactionInputParser.read(txInputBytes)
    val txId = ""
    val outputs = Seq()
    val lockTime = -1

    TransactionImpl(txId,version,inputs,outputs,lockTime)
  }

  def write(tx : Transaction) : String = ???
}


object RawTransactionParser extends RawTransactionParser
