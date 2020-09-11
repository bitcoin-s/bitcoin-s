package org.bitcoins.core.api.chain

import org.bitcoins.crypto.DoubleSha256Digest

case class FilterSyncMarker(startHeight: Int, stopBlockHash: DoubleSha256Digest)
