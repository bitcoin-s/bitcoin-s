import org.bitcoins.core.protocol.blockchain.Block
import org.slf4j.LoggerFactory

import scala.io.Source

object BlockBench extends App {
  private def logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  private def timeBlockParsing[R](block: => R): Long = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    val time = t1 - t0
    logger.info("Elapsed time: " + (time) + "ms")
    time
  }

  def bench1(): Unit = {
    val fileName =
      "/00000000000000000008513c860373da0484f065983aeb063ebf81c172e81d48.txt"
    val lines = Source.fromURL(getClass.getResource(fileName)).mkString
    val time = timeBlockParsing(Block.fromHex(lines))
    require(time <= 15000)
  }

  def bench2(): Unit = {
    val fileName =
      "/000000000000000000050f70113ab1932c195442cb49bcc4ee4d7f426c8a3295.txt"
    val lines = Source.fromURL(getClass.getResource(fileName)).mkString
    val time = timeBlockParsing(Block.fromHex(lines))
    require(time <= 15000)
  }

  0.until(10).foreach(_ => bench1())

  //bench2()
}
