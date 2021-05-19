package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.Sha256Digest

case class DLCAnnouncementDb(
    dlcId: Sha256Digest,
    announcementId: Long,
    index: Int,
    used: Boolean
)
