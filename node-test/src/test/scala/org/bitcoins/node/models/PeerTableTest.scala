package org.bitcoins.node.models

import org.bitcoins.testkit.fixtures.NodeDAOFixture

import java.time.Instant

class PeerDAOTest extends NodeDAOFixture {

  behavior of "PeerDAO"

  it must "write a peer and read it back" in { daos =>
    val peerDAO = daos.peerDAO
    val peer = PeerDB(address = "127.0.0.1", lastConnected = Instant.now)

    for {
      created <- peerDAO.create(peer)
      read <- peerDAO.read(peer.address)
    } yield {
      assert(
        read.get.address == created.address && read.get.lastConnected.getEpochSecond == created.lastConnected.getEpochSecond)
    }

  }
}
