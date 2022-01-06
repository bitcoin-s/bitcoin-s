package org.bitcoins.core.util.sorted

import org.bitcoins.core.protocol.tlv._

/** Represents an ordered set of OracleAnnouncementTLVs
  * The ordering represents the ranked preference of the user
  */
case class OrderedAnnouncements(vec: Vector[OracleAnnouncementTLV])
    extends SortedVec[OracleAnnouncementTLV, OracleAnnouncementTLV](
      vec,
      SortedVec.forOrdered(vec)) {
  require(vec.nonEmpty, s"Cannot have empty OrderedAnnouncements")
}

/** Represents an ordered set of OracleAnnouncementV0TLV
  * The ordering represents the ranked preference of the user
  */
case class OrderedAnnouncementV0s(vec: Vector[OracleAnnouncementV0TLV])
    extends SortedVec[OracleAnnouncementV0TLV, OracleAnnouncementV0TLV](
      vec,
      SortedVec.forOrdered(vec))
