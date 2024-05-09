package org.bitcoins.core.util.sorted

import org.bitcoins.core.protocol.dlc.models.DLCPayoutCurvePiece

case class OrderedDLCPayoutCurvePieces(
    private val vec: Vector[DLCPayoutCurvePiece])
    extends SortedVec[DLCPayoutCurvePiece, DLCPayoutCurvePiece](
      vec,
      org.bitcoins.core.dlcPayoutCurvePieceOrdering)

object OrderedDLCPayoutCurvePieces
    extends SortedVecFactory[DLCPayoutCurvePiece, OrderedDLCPayoutCurvePieces] {

  override def apply(
      piece: DLCPayoutCurvePiece): OrderedDLCPayoutCurvePieces = {
    OrderedDLCPayoutCurvePieces(Vector(piece))
  }

  override def fromUnsorted(
      vec: Vector[DLCPayoutCurvePiece]): OrderedDLCPayoutCurvePieces = {
    val sorted = vec.sorted(org.bitcoins.core.dlcPayoutCurvePieceOrdering)
    OrderedDLCPayoutCurvePieces(sorted)
  }
}
