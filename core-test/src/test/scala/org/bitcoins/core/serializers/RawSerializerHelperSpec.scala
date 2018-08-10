package org.bitcoins.core.serializers

import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.protocol.transaction.{ Transaction, TransactionInput, TransactionOutput }
import org.scalacheck.{ Prop, Properties }
import scodec.bits.ByteVector

class RawSerializerHelperSpec extends Properties("RawSerializerHelperSpec") {

  property("serialization symmetry of txs") = {
    Prop.forAll(TransactionGenerators.smallOutputs) { txs: Seq[TransactionOutput] =>
      val serialized = RawSerializerHelper.writeCmpctSizeUInt(txs, { tx: TransactionOutput => tx.bytes })
      val (deserialized, remaining) = RawSerializerHelper.parseCmpctSizeUIntSeq(serialized, TransactionOutput(_: scodec.bits.ByteVector))
      deserialized == txs && remaining == ByteVector.empty
    }
  }

}
