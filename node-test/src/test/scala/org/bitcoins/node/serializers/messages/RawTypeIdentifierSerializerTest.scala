package org.bitcoins.node.serializers.messages

import org.bitcoins.node.messages.TypeIdentifier.{
  MsgBlock,
  MsgFilteredBlock,
  MsgTx
}
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 5/31/16.
  */
class RawTypeIdentifierSerializerTest extends FlatSpec with MustMatchers {
  val msgTxHex = "01000000"
  val msgBlockHex = "02000000"
  val msgFilteredBlockHex = "03000000"
  "RawTypeIdentifier" must "read/write a MsgTx" in {
    val msg = RawTypeIdentifierSerializer.read(msgTxHex)
    msg must be(MsgTx)
    RawTypeIdentifierSerializer.write(msg).toHex must be(msgTxHex)
  }

  it must "read/write a MsgBlock" in {
    val msg = RawTypeIdentifierSerializer.read(msgBlockHex)
    msg must be(MsgBlock)
    RawTypeIdentifierSerializer.write(msg).toHex must be(msgBlockHex)
  }

  it must "read/write a MsgFilteredBlock" in {
    val msg = RawTypeIdentifierSerializer.read(msgFilteredBlockHex)
    msg must be(MsgFilteredBlock)
    RawTypeIdentifierSerializer.write(msg).toHex must be(msgFilteredBlockHex)
  }
}
