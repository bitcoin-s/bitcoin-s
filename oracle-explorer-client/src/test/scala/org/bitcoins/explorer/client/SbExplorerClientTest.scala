package org.bitcoins.explorer.client

import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.explorer.env.ExplorerEnv
import org.bitcoins.explorer.model.ExplorerEvent
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.Future

class SbExplorerClientTest extends BitcoinSAsyncTest {

  behavior of "SbExplorerClient"

  val explorerClient = SbExplorerClient(ExplorerEnv.Test)

  it must "list events" in {
    val eventsF: Future[Vector[ExplorerEvent]] = explorerClient.listEvents()
    for {
      events <- eventsF
    } yield {
      assert(events.nonEmpty)
    }
  }

  it must "get an event" in {
    val hash = Sha256Digest.fromHex(
      "e0a5624edbc854120982165b0eef53f0777a49febd79a0c21bf75e5582021e33")
    val eventsF = explorerClient.getEvent(hash)
    for {
      event <- eventsF
    } yield {
      assert(event.announcement.sha256 == hash)
    }
  }
}
