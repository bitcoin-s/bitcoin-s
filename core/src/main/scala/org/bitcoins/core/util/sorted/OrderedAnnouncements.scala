package org.bitcoins.core.util.sorted

import org.bitcoins.core.protocol.tlv._

/** Represents an ordered set of OracleAnnouncementTLVs */
case class OrderedAnnouncements(vec: Vector[OracleAnnouncementTLV])
    extends SortedVec[OracleAnnouncementTLV, OracleAnnouncementTLV](
      vec,
      SortedVec.forOrdered(vec))

/** Represents an ordered set of OracleAnnouncementV0TLV */
case class OrderedAnnouncementV0s(vec: Vector[OracleAnnouncementV0TLV])
    extends SortedVec[OracleAnnouncementV0TLV, OracleAnnouncementV0TLV](
      vec,
      SortedVec.forOrdered(vec))
