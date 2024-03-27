package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.number.{UInt5, UInt64}
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, ScriptPubKey}
import org.bitcoins.core.util.Bech32
import org.bitcoins.crypto.StringFactory

/** @see [[https://github.com/bitcoin/bitcoin/blob/master/doc/descriptors.md]]
  */
sealed abstract class OutputDescriptor {
  def scriptPubKey: ScriptPubKey
}

case class P2WPKHDescriptor(xPubHDPath: XPubHDPath) extends OutputDescriptor {
  val xpub = xPubHDPath.xpub
  val hdPath = xPubHDPath.bip32Path

  override val scriptPubKey: P2WPKHWitnessSPKV0 = {
    val pubKey = xpub.deriveChildPubKey(hdPath).get.key
    P2WPKHWitnessSPKV0(pubKey)
  }
}

object P2WPKHDescriptor extends StringFactory[P2WPKHDescriptor] {

  override def fromString(string: String): P2WPKHDescriptor = {
    val iter = DescriptorIterator(string)
    val t = iter.takeDescriptorType()
    if (t != DescriptorType.WPKH) {
      sys.error(s"Incorrect type for p2wpkh descriptor, got=$t")
    } else {
      val xPubHDPath = iter.takeXPubHDPath()
      P2WPKHDescriptor(xPubHDPath)
    }
  }
}

object OutputDescriptor extends StringFactory[OutputDescriptor] {

  final val charset: Vector[Char] = {
    ("0123456789()[],'/*abcdefgh@:$%{}" +
      "IJKLMNOPQRSTUVWXYZ&+-.;<=>?!^_|~" +
      "ijklmnopqrstuvwxyzABCDEFGH`#\"\\ ").toVector
  }

  private val map: Map[DescriptorType, StringFactory[OutputDescriptor]] = {
    Map(
      DescriptorType.WPKH -> P2WPKHDescriptor
    )
  }

  override def fromString(string: String): OutputDescriptor = {
    val iter = DescriptorIterator(string)
    val t = iter.takeDescriptorType()
    map
      .get(t)
      .map(_.fromString(string))
      .getOrElse(
        sys.error(s"Could not find descriptor for t=$t")
      )
  }

  def createChecksum(string: String): String = {
    require(!string.exists(_ == '#'),
            s"String already contains checksum, got=$string")
    var c = UInt64.one
    var cls = 0
    var clsCount = 0
    val u5s = Bech32.decodeStringToU5s(string, charset)
    u5s.zipWithIndex.foreach { case (u5, pos) =>
      c = polyMod(c, u5)
      cls = cls * 3 + (pos >> 5)
      clsCount += 1
      if (clsCount == 3) {
        // Emit an extra symbol representing the group numbers, for every 3 characters.
        c = polyMod(c, UInt5(cls))
        cls = 0
        clsCount = 0
      }
    }

    if (clsCount > 0) {
      c = polyMod(c, UInt5(cls))
    }

    1.until(8).foreach { _ =>
      c = polyMod(c, UInt5.zero)
    }

    //c = c ^ 1 // Prevent appending zeroes from not affecting the checksum.

    val builder = new StringBuilder()
    1.until(8).foreach { j =>
      //ret[j] = CHECKSUM_CHARSET[(c >> (5 * (7 - j))) & 31]
      val char = charset((c.toInt >> (5 * (7 - j))) & 31)
      builder.append(char)
    }

    builder.result()
  }

  def polyMod(c: UInt64, pos: UInt5): UInt64 = ???
}
