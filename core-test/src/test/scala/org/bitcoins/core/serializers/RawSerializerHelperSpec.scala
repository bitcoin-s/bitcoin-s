package org.bitcoins.core.serializers

import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.scalacheck.{Prop, Properties}
import scodec.bits.ByteVector

class RawSerializerHelperSpec extends Properties("RawSerializerHelperSpec") {

  property("serialization symmetry of txs") = {
    Prop.forAll(TransactionGenerators.smallOutputs) {
      txs: Seq[TransactionOutput] =>
        val serialized = RawSerializerHelper.writeCmpctSizeUInt(txs, {
          tx: TransactionOutput =>
            tx.bytes
        })
        val (deserialized, remaining) = RawSerializerHelper
          .parseCmpctSizeUIntSeq(serialized, TransactionOutput(_: ByteVector))
        deserialized == txs && remaining == ByteVector.empty
    }
  }

}
