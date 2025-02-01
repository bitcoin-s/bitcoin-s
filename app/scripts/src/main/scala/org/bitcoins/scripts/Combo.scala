package org.bitcoins.scripts

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import play.api.libs.json.{Json, Reads, Writes}

case class Combo(num: Long, p2sh: String, addr: String, size: Int) {
  val spk: ScriptPubKey = {
    ScriptPubKey.fromAsmHex(p2sh.drop(3).dropRight(1))
  }

  val bitcoinAddress: BitcoinAddress = BitcoinAddress.fromString(addr)
}

object Combo {
  implicit val reads: Reads[Combo] = Json.reads[Combo]
  implicit val writes: Writes[Combo] = Json.writes[Combo]
}
