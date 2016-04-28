package org.bitcoins.protocol

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 7/26/15.
 */
class CommonStructuresTest extends FlatSpec with MustMatchers  {



  "VarInts" must "serialize a VarInt correctly" in {
    val varInt = CompactSizeUIntImpl(253,3)
    varInt.hex must be ("fdfd00")
  }

  it must "serialize a VarInt with size 1 correctly" in {
    val varInt = CompactSizeUIntImpl(139,1)
    varInt.hex must be ("8b")
  }

  it must "serialize a VarInt with that is the number zero correctly" in {
    val varInt = CompactSizeUIntImpl(0,1)
    varInt.hex must be ("00")
  }
}
