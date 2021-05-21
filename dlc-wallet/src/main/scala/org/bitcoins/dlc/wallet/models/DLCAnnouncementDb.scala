package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.Sha256Digest

/** This table is for mapping announcements to DLCs,
  * as well as, some contains DLC specific data about
  * the announcement.
  */
case class DLCAnnouncementDb(
    dlcId: Sha256Digest,
    announcementId: Long,
    index: Int,
    used: Boolean
)
