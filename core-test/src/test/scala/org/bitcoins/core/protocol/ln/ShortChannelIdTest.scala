package org.bitcoins.core.protocol.ln

import org.scalatest.{FlatSpec, MustMatchers}

class ShortChannelIdTest extends FlatSpec with MustMatchers {

  it must "convert short channel id to and from human readable form" in {
    // BOLT example
    ShortChannelId.fromHumanReadableString("539268x845x1") must be (ShortChannelId.fromHex("83a8400034d0001"))
    ShortChannelId.fromHex("83a8400034d0001").toHumanReadableString must be ("539268x845x1")

    // min value
    ShortChannelId.fromHumanReadableString("0x0x0") must be (ShortChannelId.fromHex("0"))
    ShortChannelId.fromHex("0").toHumanReadableString must be ("0x0x0")

    // max value
    ShortChannelId.fromHumanReadableString("16777215x16777215x65535") must be (ShortChannelId.fromHex("ffffffffffffffff"))
    ShortChannelId.fromHex("ffffffffffffffff").toHumanReadableString must be ("16777215x16777215x65535")

    // overflow value
    ShortChannelId.fromHumanReadableString("16777216x16777216x65536") must be (ShortChannelId.fromHex("0"))
  }

}
