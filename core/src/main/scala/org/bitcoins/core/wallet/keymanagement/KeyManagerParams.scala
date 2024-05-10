package org.bitcoins.core.wallet.keymanagement

import java.nio.file.Path

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.hd.HDPurpose

case class KeyManagerParams(
    seedPath: Path,
    purpose: HDPurpose,
    network: NetworkParameters)
