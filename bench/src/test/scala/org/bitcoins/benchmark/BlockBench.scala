package org.bitcoins.benchmark

import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.testkit.util.BitcoinSUnitTest

import scala.concurrent.duration.DurationInt
import scala.io.Source

class BlockBench extends BitcoinSUnitTest {

  it must "parse a 1.5MB block" in {
    val fileName =
      "/00000000000000000008513c860373da0484f065983aeb063ebf81c172e81d48.txt"
    val lines = Source.fromURL(getClass.getResource(fileName)).mkString
    (0 until 10).foreach { _ =>
      val duration = BenchUtil.timeOperation(() => Block.fromHex(lines))
      assert(duration < 15.seconds)
    }
  }

  it must "parse a 2MB block" in {
    val fileName =
      "/000000000000000000050f70113ab1932c195442cb49bcc4ee4d7f426c8a3295.txt"
    val lines = Source.fromURL(getClass.getResource(fileName)).mkString

    (0 until 10).foreach { _ =>
      val duration = BenchUtil.timeOperation(() => Block.fromHex(lines))
      assert(duration < 15.seconds)
    }
  }

}
