package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.number.{UInt64, UInt8}
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

  private val charsetWithIdx: Vector[(Char, Int)] = {
    charset.zipWithIndex
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

    string.foreach { case ch =>
      val pos = charsetWithIdx
        .find(_._1 == ch)
        .map(_._2)
        .get
      c = polyMod(c, pos)
      cls = cls * 3 + (pos >> 5)
      clsCount += 1
      if (clsCount == 3) {
        // Emit an extra symbol representing the group numbers, for every 3 characters.
        c = polyMod(c, cls)
        cls = 0
        clsCount = 0
      }
    }

    if (clsCount > 0) {
      c = polyMod(c, cls)
    }

    1.until(8).foreach { _ =>
      c = polyMod(c, 0)
    }

    //c = c ^ 1 // Prevent appending zeroes from not affecting the checksum.

    val builder = new StringBuilder()
    1.until(8).foreach { j =>
      //ret[j] = CHECKSUM_CHARSET[(c >> (5 * (7 - j))) & 31]
      val char = Bech32.charset((c.toInt >> (5 * (7 - j))) & 31)
      builder.append(char)
    }

    builder.result()
  }

  def polyMod(c: UInt64, idx: Int): UInt64 = {
    //uint64_t PolyMod(uint64_t c, int val)
    //{
    //    uint8_t c0 = c >> 35;
    //    c = ((c & 0x7ffffffff) << 5) ^ val;
    //    if (c0 & 1) c ^= 0xf5dee51989;
    //    if (c0 & 2) c ^= 0xa9fdca3312;
    //    if (c0 & 4) c ^= 0x1bab10e32d;
    //    if (c0 & 8) c ^= 0x3706b1677a;
    //    if (c0 & 16) c ^= 0x644d626ffd;
    //    return c;
    //}

    var res = c
    val c0: UInt8 = UInt8((c >> 35).toInt)
    res = (c & UInt64(0x7ffffffffL) << 5) ^ idx
    if ((c0 & UInt8.one) != UInt8.zero) {
      res = res ^ 0xf5dee51989L
    }

    if ((c0 & UInt8(2)) != UInt8.zero) {
      res = res ^ 0xa9fdca3312L
    }

    if ((c0 & UInt8(4)) != UInt8.zero) {
      res = res ^ 0x1bab10e32dL
    }

    if ((c0 & UInt8(8)) != UInt8.zero) {
      res = res ^ 0x3706b1677aL
    }

    if ((c0 & UInt8(16)) != UInt8.zero) {
      res = res ^ 0x644d626ffdL
    }

    res
  }
}
