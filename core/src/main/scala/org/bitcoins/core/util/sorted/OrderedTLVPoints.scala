package org.bitcoins.core.util.sorted

import org.bitcoins.core.protocol.tlv.TLVPoint

case class OrderedTLVPoints(private val vec: Vector[TLVPoint])
    extends SortedVec[TLVPoint, TLVPoint](vec,
                                          org.bitcoins.core.tlvPointOrdering)

object OrderedTLVPoints extends SortedVecFactory[TLVPoint, OrderedTLVPoints] {

  override def apply(point: TLVPoint): OrderedTLVPoints = {
    OrderedTLVPoints(Vector(point))
  }

  override def fromUnsorted(vec: Vector[TLVPoint]): OrderedTLVPoints = {
    val sorted = vec.sorted(org.bitcoins.core.tlvPointOrdering)
    OrderedTLVPoints(sorted)
  }
}
