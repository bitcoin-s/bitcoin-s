package org.bitcoins.core.api.wallet

import org.bitcoins.crypto.DoubleSha256DigestBE

case class BlockSyncState(height: Int, blockHash: DoubleSha256DigestBE)
