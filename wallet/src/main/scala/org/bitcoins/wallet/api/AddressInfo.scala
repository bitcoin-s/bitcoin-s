package org.bitcoins.wallet.api

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.config.NetworkParameters

/**
  * This class represents the result of querying for address info
  * from our wallet
  */
case class AddressInfo(
    pubkey: ECPublicKey,
    network: NetworkParameters,
    path: HDPath)
