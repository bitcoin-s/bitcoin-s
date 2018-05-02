package org.bitcoins.core.serializers

import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.protocol.transaction.Transaction
import org.scalacheck.{ Prop, Properties }

class RawSerializerHelperSpec extends Properties("RawSerializerHelperSpec") {

  property("serialization symmetry of txs") = {
    Prop.forAll(TransactionGenerators.smallTransactions) { txs: Seq[Transaction] =>
      val serialized = RawSerializerHelper.writeCmpctSizeUInt[Transaction](txs, { tx: Transaction => tx.bytes })
      val (deserialized, remaining) = RawSerializerHelper.parseCmpctSizeUIntSeq(serialized, Transaction(_: Seq[Byte]))
      deserialized == txs && remaining == Nil
    }
  }

}
