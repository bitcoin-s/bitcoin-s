package org.bitcoins.core

import org.bitcoins.core.protocol.blockchain.Block
import org.scalameter.api._
import scala.io.Source
import scodec.bits.ByteVector
import org.scalameter.History

object BlockBench extends Bench.OfflineReport {

  override def persistor: Persistor = GZIPJSONSerializationPersistor()

  private def readResource(resourceName: String): String = {
    Source.fromURL(getClass.getResource(resourceName)).mkString
  }

  val firstBlockHex = readResource(
    "/00000000000000000008513c860373da0484f065983aeb063ebf81c172e81d48.txt")

  val firstBlockBytes = ByteVector.fromValidHex(firstBlockHex)

  val secondBlockHex = readResource(
    "/000000000000000000050f70113ab1932c195442cb49bcc4ee4d7f426c8a3295.txt")

  val secondBlockBytes = ByteVector.fromValidHex(secondBlockHex)

  performance of "Block" in {
    measure method "fromHex" in {
      using(Gen.unit("Block 514 492 - 761KB")) in { _ =>
        Block.fromHex(firstBlockHex)
      }

      using(Gen.unit("Block 514 485 - 1030KB")) in { _ =>
        Block.fromHex(secondBlockHex)
      }
    }

    measure method "fromBytes" in {
      using(Gen.unit("Block 514 492 - 761KB")) in { _ =>
        Block.fromBytes(firstBlockBytes)
      }

      using(Gen.unit("Block 514 492 - 761KB")) in { _ =>
        Block.fromBytes(secondBlockBytes)
      }
    }
  }

}
