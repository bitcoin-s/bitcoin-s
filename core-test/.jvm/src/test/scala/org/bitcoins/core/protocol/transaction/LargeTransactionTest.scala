package org.bitcoins.core.protocol.transaction

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.io.Source

class LargeTransactionTest extends BitcoinSUnitTest {

  behavior of "LargeTransactionTest"

  it must "parse a536e7f60a493a258d9adc77d913f7798baf60c808c16898b04579d8c0652681" in {
    // from https://mempool.space/testnet/tx/a536e7f60a493a258d9adc77d913f7798baf60c808c16898b04579d8c0652681
    val fileName =
      "/a536e7f60a493a258d9adc77d913f7798baf60c808c16898b04579d8c0652681.txt"
    val lines =
      Source.fromURL(getClass.getResource(fileName)).mkString
    val tx = Transaction.fromHex(lines)
    assert(tx.hex == lines)
  }

  def timeBlockParsing[R](block: => R): Long = {
    val t0 = System.currentTimeMillis()
    val _ = block // call-by-name
    val t1 = System.currentTimeMillis()
    val time = t1 - t0
    time
  }
}
