package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.hd.{BIP32Path}

case class XPubHDPath(xpub: ExtPublicKey, bip32Path: BIP32Path)
