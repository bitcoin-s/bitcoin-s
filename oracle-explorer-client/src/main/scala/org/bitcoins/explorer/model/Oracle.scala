package org.bitcoins.explorer.model

import org.bitcoins.crypto.SchnorrPublicKey

case class Oracle(pubkey: SchnorrPublicKey, oracleName: String)
