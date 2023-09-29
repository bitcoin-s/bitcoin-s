package org.bitcoins.core.util.sorted

import org.bitcoins.core.protocol.tlv._

/** Represents an ordered set of OracleAnnouncementTLVs
  * The ordering represents the ranked preference of the user
  */
case class OrderedAnnouncements(vec: Vector[BaseOracleAnnouncement])
    extends SortedVec[BaseOracleAnnouncement, BaseOracleAnnouncement](
      vec,
      SortedVec.forOrdered(vec)) {
  require(vec.nonEmpty, s"Cannot have empty OrderedAnnouncements")
}

/** Represents an ordered set of OracleAnnouncementV0TLV
  * The ordering represents the ranked preference of the user
  */
case class OrderedAnnouncementV0s(vec: Vector[BaseOracleAnnouncement])
    extends SortedVec[BaseOracleAnnouncement, BaseOracleAnnouncement](
      vec,
      SortedVec.forOrdered(vec))
