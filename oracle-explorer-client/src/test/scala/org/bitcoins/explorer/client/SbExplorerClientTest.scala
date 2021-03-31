package org.bitcoins.explorer.client

import org.bitcoins.core.protocol.tlv.{
  OracleAnnouncementV0TLV,
  OracleAttestmentV0TLV
}
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.explorer.env.ExplorerEnv
import org.bitcoins.explorer.model.{
  CreateAnnouncementExplorer,
  CreateAttestations,
  SbAnnouncementEvent
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.Future

class SbExplorerClientTest extends BitcoinSAsyncTest {

  behavior of "SbExplorerClient"

  val explorerClient = SbExplorerClient(ExplorerEnv.Test)

  //https://test.oracle.suredbits.com/event/57505dcdfe8746d9adf3454df538244a425f302c07642d9dc4a4f635fbf08d30
  private val announcementHex: String =
    "fdd824b33cbc4081b947ea9d05e616b010b563bfdbc42a2d20effa6f169f8e4be732b10d5461fa84b5739876a0c8a7bdb717040b8ee5907fe7e60694199ba948ecd505b01d5dcdba2e64cb116cc0c375a0856298f0058b778f46bfe625ac6576204889e4fdd8224f0001efdf735567ae0a00a515e313d20029de5d7525da7b8367bc843d28b672d4db4d605bd280fdd80609000203594553024e4f1b323032312d30332d32342d73756e6e792d696e2d6368696361676f"

  private val announcement: OracleAnnouncementV0TLV =
    OracleAnnouncementV0TLV.fromHex(announcementHex)

  it must "list events" in {
    val eventsF: Future[Vector[SbAnnouncementEvent]] =
      explorerClient.listEvents()
    for {
      events <- eventsF
    } yield {
      assert(events.nonEmpty)
    }
  }

  it must "get an event" in {
    val hash = announcement.sha256
    val eventsF = explorerClient.getEvent(hash)
    for {
      event <- eventsF
    } yield {
      assert(event.announcement.sha256 == hash)
    }
  }

  it must "return failure from get an event if the event DNE" in {
    val hash = Sha256Digest.empty
    recoverToSucceededIf[RuntimeException] {
      explorerClient.getEvent(hash)
    }
  }

  it must "create an event on the oracle explorer and then get that event" ignore {

    val oracleName = "Chris_Stewart_5"
    val description = "2021-03-24-sunny-in-chicago"
    val uriOpt = Some("https://twitter.com/Chris_Stewart_5")

    val event =
      CreateAnnouncementExplorer(announcement, oracleName, description, uriOpt)

    val createdF = explorerClient.createAnnouncement(event)
    for {
      _ <- createdF
      event <- explorerClient.getEvent(event.oracleAnnouncementV0.sha256)
    } yield {
      assert(event.announcement == announcement)
      assert(event.attestations.isEmpty)
      assert(event.uri == uriOpt)
      assert(event.oracleName == oracleName)
      assert(event.description == description)
    }
  }

  it must "post attestations for an event to the oracle explorer" ignore {
    //the announcement is posted in the test case above
    //which means the test case above must be run before this test case
    val attestationsHex =
      "fdd868821b323032312d30332d32342d73756e6e792d696e2d6368696361676f1d5dcdba2e64cb116cc0c375a0856298f0058b778f46bfe625ac6576204889e40001efdf735567ae0a00a515e313d20029de5d7525da7b8367bc843d28b672d4db4db5de4dbff689f3b742be634a9c92c615dbcf2eadbdd470f514b1ac250a30db6d03594553"

    val attestations = OracleAttestmentV0TLV.fromHex(attestationsHex)

    val announcementHash = announcement.sha256
    val create = CreateAttestations(announcementHash, attestations)
    val createdF = explorerClient.createAttestations(create)

    for {
      _ <- createdF
      //now we must have the attesations
      event <- explorerClient.getEvent(announcementHash)
    } yield {
      assert(event.attestations.isDefined)
      assert(event.attestations.get == attestations)
    }
  }
}
