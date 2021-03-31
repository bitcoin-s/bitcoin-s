package org.bitcoins.explorer.model

import org.bitcoins.core.protocol.tlv.OracleAnnouncementV0TLV
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class SbOracleEventExplorerTest extends BitcoinSUnitTest {

  behavior of "SbOracleEvent"

  it must "encode the event to POST for a form" in {

    val announcementHex =
      "fdd824b33cbc4081b947ea9d05e616b010b563bfdbc42a2d20effa6f169f8e4be732b10d5461fa84b5739876a0c8a7bdb717040b8ee5907fe7e60694199ba948ecd505b01d5dcdba2e64cb116cc0c375a0856298f0058b778f46bfe625ac6576204889e4fdd8224f0001efdf735567ae0a00a515e313d20029de5d7525da7b8367bc843d28b672d4db4d605bd280fdd80609000203594553024e4f1b323032312d30332d32342d73756e6e792d696e2d6368696361676f"
    val announcement = OracleAnnouncementV0TLV.fromHex(announcementHex)
    val oracleName = "Chris_Stewart_5"
    val description = "2021-03-24-sunny-in-chicago"
    val uriOpt = Some("https://twitter.com/Chris_Stewart_5")

    val event =
      CreateAnnouncementExplorer(announcement, oracleName, description, uriOpt)

    val expected = s"oracleAnnouncementV0=${announcementHex}&" +
      s"description=$description&" +
      s"oracleName=${oracleName}&" +
      s"uri=${uriOpt.get}"

    assert(event.toString == expected)
  }
}
