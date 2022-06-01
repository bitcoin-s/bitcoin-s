package org.bitcoins.core.api.dlc.wallet.db

import org.bitcoins.crypto.Sha256Digest

import java.net.InetSocketAddress

case class DLCContactMapping(dlc: DLCDb, contact: Option[DLCContactDb]) {

  def dlcId: Sha256Digest = dlc.dlcId

  def toDLCContactMappingDb: Option[DLCContactMappingDb] =
    contact.map(c => DLCContactMappingDb(dlc.dlcId, c.address))
}

case class DLCContactMappingDb(
    dlcId: Sha256Digest,
    contactId: InetSocketAddress)
