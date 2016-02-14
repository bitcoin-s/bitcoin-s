package org.scalacoin.protocol

import org.scalacoin.util.ScalacoinUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 7/26/15.
 */
class CommonStructuresTest extends FlatSpec with MustMatchers  {



  "VarInts" must "serialize a VarInt correctly" in {
    val varInt = VarIntImpl(253,3)
    varInt.hex must be ("fdfd00")
  }

  it must "serialize a VarInt with size 1 correctly" in {
    val varInt = VarIntImpl(139,1)
    varInt.hex must be ("8b")
  }
}
