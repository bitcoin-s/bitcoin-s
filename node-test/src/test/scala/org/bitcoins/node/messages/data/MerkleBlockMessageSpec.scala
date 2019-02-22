package org.bitcoins.node.messages.data

import org.bitcoins.testkit.gen.DataMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 8/26/16.
  */
class MerkleBlockMessageSpec extends Properties("MerkleBlockMessageSpec") {
  property("serialization symmetry") =
    Prop.forAll(DataMessageGenerator.merkleBlockMessage) {
      case merkleBlockMsg =>
        MerkleBlockMessage(merkleBlockMsg.hex) == merkleBlockMsg
    }
}
