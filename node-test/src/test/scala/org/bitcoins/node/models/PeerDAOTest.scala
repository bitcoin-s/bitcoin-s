package org.bitcoins.node.models

import org.bitcoins.core.p2p.AddrV2Message
import org.bitcoins.testkit.fixtures.NodeDAOFixture
import scodec.bits.ByteVector

import java.time.Instant

class PeerDAOTest extends NodeDAOFixture {

  behavior of "PeerDAO"

  it must "write peer bytes and read it back" in { daos =>
    val peerDAO = daos.peerDAO
    val bytes = ByteVector(Array[Byte](127, 0, 0, 1))
    val peer = PeerDb(address = bytes,
                      port = 8333,
                      lastSeen = Instant.now,
                      firstSeen = Instant.now,
                      networkId = AddrV2Message.IPV4_NETWORK_BYTE)

    for {
      created <- peerDAO.create(peer)
      read <- peerDAO.read(peer.address)
    } yield {
      assert(
        read.get.address == created.address &&
          read.get.port == created.port &&
          read.get.lastSeen.getEpochSecond == created.lastSeen.getEpochSecond
          && read.get.firstSeen.getEpochSecond == created.firstSeen.getEpochSecond
          && read.get.networkId == AddrV2Message.IPV4_NETWORK_BYTE
      )
    }
  }
}
