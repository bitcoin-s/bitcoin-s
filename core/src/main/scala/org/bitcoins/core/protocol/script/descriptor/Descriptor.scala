package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.util.Bech32
import org.bitcoins.crypto.{ECPrivateKey, PublicKey, StringFactory}

import scala.util.Try

/** @see
  *   [[https://github.com/bitcoin/bitcoin/blob/master/doc/descriptors.md]]
  */
sealed abstract class Descriptor {

  def expression: DescriptorExpression
  def checksum: Option[String]
  def scriptPubKey: ScriptPubKey
  override def toString: String = {
    val checksumStr = checksum match {
      case Some(c) => "#" + c
      case None    => ""
    }
    expression.toString + checksumStr
  }
}

sealed abstract class ScriptDescriptor extends Descriptor {
  override def expression: ScriptExpression

  def descriptorType: DescriptorType = expression.descriptorType
  def scriptPubKey: ScriptPubKey
}

case class RawDescriptor(
    expression: RawScriptExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: RawScriptPubKey = expression.scriptPubKey
}

case class P2WPKHDescriptor(
    expression: P2WPKHExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2WPKHWitnessSPKV0 = expression.scriptPubKey

  def address(np: NetworkParameters): Bech32Address = {
    Bech32Address(scriptPubKey, np)
  }
}

case class P2WSHDescriptor(
    expression: P2WSHExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2WSHWitnessSPKV0 = expression.scriptPubKey
}

case class P2PKDescriptor[T <: PublicKey](
    expression: P2PKScriptExpression[T],
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2PKScriptPubKey = expression.scriptPubKey
}

case class P2PKHDescriptor(
    expression: P2PKHScriptExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2PKHScriptPubKey = expression.scriptPubKey
}

case class MultisigDescriptor(
    expression: MultisigExpression,
    checksum: Option[String])
    extends ScriptDescriptor {

  override val scriptPubKey: MultiSignatureScriptPubKey =
    expression.scriptPubKey
}

case class SortedMultisigDescriptor(
    expression: SortedMultisigExpression,
    checksum: Option[String])
    extends ScriptDescriptor {

  override val scriptPubKey: MultiSignatureScriptPubKey =
    expression.scriptPubKey
}

case class P2SHDescriptor(expression: P2SHExpression, checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2SHScriptPubKey = expression.scriptPubKey
}

case class TaprootDescriptor(
    expression: TreeExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: TaprootScriptPubKey = expression.scriptPubKey
}

sealed abstract class ComboDescriptor extends ScriptDescriptor {
  override def expression: ComboExpression

  override def scriptPubKey: ScriptPubKey = expression.scriptPubKey
  def p2pk: P2PKScriptPubKey = P2PKScriptPubKey(expression.source.pubKey)
  def p2pkh: P2PKHScriptPubKey = P2PKHScriptPubKey(expression.source.pubKey)
}

case class ComboDescriptorUncompressed(
    expression: ComboExpression,
    checksum: Option[String])
    extends ComboDescriptor

case class ComboDescriptorCompressed(
    expression: ComboExpression,
    checksum: Option[String])
    extends ComboDescriptor {
  require(
    expression.source.pubKey.isCompressed,
    s"ComboDescriptorCompressed must have compressed pubkey, got=$expression")

  val p2wpkh: P2WPKHWitnessSPKV0 =
    P2WPKHWitnessSPKV0(pubKey = expression.source.pubKey)
  val p2shp2wpkh: P2SHScriptPubKey = P2SHScriptPubKey(p2wpkh)
}

case class AddressDescriptor(
    expression: AddressExpression,
    checksum: Option[String])
    extends Descriptor {
  val address: BitcoinAddress = expression.address
  override val scriptPubKey: ScriptPubKey = address.scriptPubKey
}

sealed abstract class DescriptorFactory[
    T <: Descriptor,
    E <: DescriptorExpression,
    U <: DescriptorType]
    extends StringFactory[T] {
  def descriptorType: U

  override def fromString(string: String): T = {
    val iter = DescriptorIterator(string)
    val t = iter.takeDescriptorType()
    if (t != descriptorType) {
      sys.error(
        s"Incorrect type for descriptor, got=$t expected=$descriptorType")
    } else {
      val (payload, checksum) = iter.current.span(_ != '#')
      val expressionIter = DescriptorIterator(payload.dropRight(1)) // drop ')'
      val expression = parseValidExpression(expressionIter)
      // now check for a valid checksum
      val checksumOpt =
        if (checksum.nonEmpty) Some(checksum.tail) else None // drop '#'
      val isValidChecksum = Descriptor.isValidChecksum(
        createDescriptor(expression, None),
        checksumOpt)
      if (isValidChecksum) {
        createDescriptor(expression, checksumOpt)
      } else {
        sys.error(s"Invalid descriptor checksum given for string=$string")
      }
    }
  }

  protected def parseValidExpression(iter: DescriptorIterator): E

  protected def createDescriptor(e: E, checksum: Option[String]): T
}

object RawDescriptor
    extends DescriptorFactory[RawDescriptor,
                              RawScriptExpression,
                              ScriptDescriptorType.Raw.type] {

  override val descriptorType: ScriptDescriptorType.Raw.type =
    ScriptDescriptorType.Raw

  override protected def parseValidExpression(
      iter: DescriptorIterator): RawScriptExpression = {
    val raw = RawScriptPubKey.fromAsmHex(iter.current)
    RawScriptExpression(raw)
  }

  override protected def createDescriptor(
      e: RawScriptExpression,
      checksum: Option[String]): RawDescriptor = {
    RawDescriptor(e, checksum)
  }
}

object P2WPKHDescriptor
    extends DescriptorFactory[P2WPKHDescriptor,
                              P2WPKHExpression,
                              ScriptDescriptorType.WPKH.type] {
  override val descriptorType: ScriptDescriptorType.WPKH.type =
    ScriptDescriptorType.WPKH

  override def parseValidExpression(
      iter: DescriptorIterator): P2WPKHExpression = {
    val keyExpression = iter.takeSingleECKeyExpression()
    P2WPKHExpression(keyExpression)
  }

  override protected def createDescriptor(
      e: P2WPKHExpression,
      checksum: Option[String]): P2WPKHDescriptor = {
    P2WPKHDescriptor(e, checksum)
  }

  def apply(
      privKey: ECPrivateKey,
      network: NetworkParameters): P2WPKHDescriptor = {
    val keyExpression = RawPrivateECPublicKeyExpression(
      key = privKey.toPrivateKeyBytes(),
      network = network,
      originOpt = None)
    val p2wpkhExpression = P2WPKHExpression(keyExpression)
    val noChecksum = P2WPKHDescriptor(p2wpkhExpression, None)
    val checksum = Descriptor.createChecksum(noChecksum)
    P2WPKHDescriptor(p2wpkhExpression, Some(checksum))
  }

  def apply(keyExpression: ExtECPublicKeyExpression): P2WPKHDescriptor = {
    val p2wpkhExpression = P2WPKHExpression(keyExpression)
    val noChecksum = P2WPKHDescriptor(p2wpkhExpression, None)
    val checksum = Descriptor.createChecksum(noChecksum)
    P2WPKHDescriptor(p2wpkhExpression, Some(checksum))
  }
}

object P2WSHDescriptor
    extends DescriptorFactory[P2WSHDescriptor,
                              P2WSHExpression,
                              ScriptDescriptorType.WSH.type] {
  override val descriptorType: ScriptDescriptorType.WSH.type =
    ScriptDescriptorType.WSH

  override protected def parseValidExpression(
      iter: DescriptorIterator): P2WSHExpression = {
    val scriptExpression = iter.takeRawSPKScriptExpression()
    P2WSHExpression(scriptExpression)
  }

  override protected def createDescriptor(
      e: P2WSHExpression,
      checksum: Option[String]): P2WSHDescriptor = {
    P2WSHDescriptor(e, checksum)
  }
}

object P2PKDescriptor
    extends DescriptorFactory[P2PKDescriptor[PublicKey],
                              P2PKScriptExpression[PublicKey],
                              ScriptDescriptorType.PK.type] {
  override val descriptorType: ScriptDescriptorType.PK.type =
    ScriptDescriptorType.PK

  override protected def parseValidExpression(
      iter: DescriptorIterator): P2PKScriptExpression[PublicKey] = {
    val keyExpression = iter.takeSingleKeyExpression()
    P2PKScriptExpression[PublicKey](keyExpression)
  }

  override protected def createDescriptor(
      e: P2PKScriptExpression[PublicKey],
      checksum: Option[String]): P2PKDescriptor[PublicKey] = {
    P2PKDescriptor(e, checksum)
  }
}

object P2PKHDescriptor
    extends DescriptorFactory[P2PKHDescriptor,
                              P2PKHScriptExpression,
                              ScriptDescriptorType.PKH.type] {
  override val descriptorType: ScriptDescriptorType.PKH.type =
    ScriptDescriptorType.PKH

  override protected def parseValidExpression(
      iter: DescriptorIterator): P2PKHScriptExpression = {
    val keyExpression = iter.takeSingleECKeyExpression()
    P2PKHScriptExpression(keyExpression)
  }

  override protected def createDescriptor(
      e: P2PKHScriptExpression,
      checksum: Option[String]): P2PKHDescriptor = {
    P2PKHDescriptor(e, checksum)
  }
}

object MultisigDescriptor
    extends DescriptorFactory[MultisigDescriptor,
                              MultisigExpression,
                              ScriptDescriptorType.Multi.type] {
  override val descriptorType: ScriptDescriptorType.Multi.type =
    ScriptDescriptorType.Multi

  override protected def parseValidExpression(
      iter: DescriptorIterator): MultisigExpression = {
    val keyExpression = iter.takeMultisigKeyExpression()
    MultisigExpression(keyExpression)
  }

  override protected def createDescriptor(
      e: MultisigExpression,
      checksum: Option[String]): MultisigDescriptor = {
    MultisigDescriptor(e, checksum)
  }
}

object SortedMultisigDescriptor
    extends DescriptorFactory[SortedMultisigDescriptor,
                              SortedMultisigExpression,
                              ScriptDescriptorType.SortedMulti.type] {

  override val descriptorType: ScriptDescriptorType.SortedMulti.type =
    ScriptDescriptorType.SortedMulti

  override protected def parseValidExpression(
      iter: DescriptorIterator): SortedMultisigExpression = {
    val expr = iter.takeMultisigKeyExpression()
    SortedMultisigExpression(expr)
  }

  override protected def createDescriptor(
      e: SortedMultisigExpression,
      checksum: Option[String]): SortedMultisigDescriptor = {
    SortedMultisigDescriptor(e, checksum)
  }
}

object P2SHDescriptor
    extends DescriptorFactory[P2SHDescriptor,
                              P2SHExpression,
                              ScriptDescriptorType.SH.type] {
  override val descriptorType: ScriptDescriptorType.SH.type =
    ScriptDescriptorType.SH

  override protected def parseValidExpression(
      iter: DescriptorIterator): P2SHExpression = {
    val scriptExpression = iter.takeScriptExpressionECKey()
    require(
      !scriptExpression.isInstanceOf[ComboExpression],
      s"Cannot have ComboExpression in P2SHDescriptor, got=$scriptExpression")
    P2SHExpression(scriptExpression)
  }

  override protected def createDescriptor(
      e: P2SHExpression,
      checksum: Option[String]): P2SHDescriptor = {
    P2SHDescriptor(e, checksum)
  }

  def apply(spk: RawScriptPubKey): P2SHDescriptor = {
    val raw = RawScriptExpression(spk)
    val p2shExpr = P2SHExpression(raw)
    val checksum = Descriptor.createChecksum(createDescriptor(p2shExpr, None))
    createDescriptor(p2shExpr, Some(checksum))
  }
}

object ComboDescriptor
    extends DescriptorFactory[ComboDescriptor,
                              ComboExpression,
                              ScriptDescriptorType.Combo.type] {
  override val descriptorType: ScriptDescriptorType.Combo.type =
    ScriptDescriptorType.Combo

  override protected def parseValidExpression(
      iter: DescriptorIterator): ComboExpression = {
    val keyExpr = iter.takeSingleECKeyExpression()
    ComboExpression(keyExpr)
  }

  override protected def createDescriptor(
      e: ComboExpression,
      checksum: Option[String]): ComboDescriptor = {
    if (e.source.pubKey.isCompressed) ComboDescriptorCompressed(e, checksum)
    else ComboDescriptorUncompressed(e, checksum)
  }
}

object TaprootDescriptor
    extends DescriptorFactory[TaprootDescriptor,
                              TreeExpression,
                              ScriptDescriptorType.TR.type] {
  override val descriptorType: ScriptDescriptorType.TR.type =
    ScriptDescriptorType.TR

  override protected def parseValidExpression(
      iter: DescriptorIterator): TreeExpression = {
    val treeExpression = iter.takeTreeExpression()
    treeExpression
  }

  override protected def createDescriptor(
      e: TreeExpression,
      checksum: Option[String]): TaprootDescriptor = {
    TaprootDescriptor(e, checksum)
  }
}

object ScriptDescriptor extends StringFactory[ScriptDescriptor] {

  private val map: Map[DescriptorType,
                       DescriptorFactory[? <: ScriptDescriptor,
                                         ? <: ScriptExpression,
                                         ? <: DescriptorType]] = {
    Map(
      ScriptDescriptorType.Raw -> RawDescriptor,
      ScriptDescriptorType.WPKH -> P2WPKHDescriptor,
      ScriptDescriptorType.WSH -> P2WSHDescriptor,
      ScriptDescriptorType.PK -> P2PKDescriptor,
      ScriptDescriptorType.SH -> P2SHDescriptor,
      ScriptDescriptorType.PKH -> P2PKHDescriptor,
      ScriptDescriptorType.Multi -> MultisigDescriptor,
      ScriptDescriptorType.SortedMulti -> SortedMultisigDescriptor,
      ScriptDescriptorType.Combo -> ComboDescriptor,
      ScriptDescriptorType.TR -> TaprootDescriptor
    )
  }

  override def fromString(string: String): ScriptDescriptor = {
    val iter = DescriptorIterator(string)
    val t = iter.takeScriptDescriptorType()
    t match {
      case s: ScriptDescriptorType =>
        map
          .get(s)
          .map(_.fromString(string))
          .getOrElse(
            sys.error(s"Cannot find parse t=$t to ScriptDescriptor s=$s"))
    }
  }
}

object AddressDescriptor
    extends DescriptorFactory[AddressDescriptor,
                              AddressExpression,
                              DescriptorType.Addr.type] {

  override def descriptorType: DescriptorType.Addr.type = DescriptorType.Addr

  override protected def parseValidExpression(
      iter: DescriptorIterator): AddressExpression = {
    iter.takeAddressExpression()
  }

  override protected def createDescriptor(
      e: AddressExpression,
      checksum: Option[String]): AddressDescriptor = {
    AddressDescriptor(e, checksum)
  }
}

object Descriptor extends StringFactory[Descriptor] {

  final val charset: Vector[Char] = {
    ("0123456789()[],'/*abcdefgh@:$%{}" +
      "IJKLMNOPQRSTUVWXYZ&+-.;<=>?!^_|~" +
      "ijklmnopqrstuvwxyzABCDEFGH`#\"\\ ").toVector
  }

  private val charsetWithIdx: Vector[(Char, Int)] = {
    charset.zipWithIndex
  }

  override def fromString(string: String): Descriptor = {
    val iter = DescriptorIterator(string)
    val t = iter.takeDescriptorType()
    t match {
      case _: ScriptDescriptorType =>
        ScriptDescriptor.fromString(string)
      case DescriptorType.Addr =>
        AddressDescriptor.fromString(string)
    }
  }

  /** Implements checksum algorithm specified by BIP380 for descriptors
    * @see
    *   [[https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki#checksum]]
    */
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
        .getOrElse {
          sys.error(s"Invalid character=$ch in descriptor string=$string")
        }
      c = polyMod(c, pos & 31)
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

    0.until(8).foreach { _ =>
      c = polyMod(c, 0)
    }

    c = c ^ 1 // Prevent appending zeroes from not affecting the checksum.

    val builder = new StringBuilder(8)
    0.until(8).foreach { j =>
      // ret[j] = CHECKSUM_CHARSET[(c >> (5 * (7 - j))) & 31]
      val idx = (c.toLong >> (5 * (7 - j))) & 31
      val char = Bech32.charset(idx.toInt)
      builder.append(char)
    }

    builder.result()
  }

  def createChecksum(descriptor: Descriptor): String = {
    createChecksum(descriptor.toString)
  }

  def isValidChecksum(
      descriptor: Descriptor,
      checksumOpt: Option[String]): Boolean = {
    checksumOpt match {
      case None => true // trivially true if we have no checksum
      case Some(checksum) =>
        val t = Try(createChecksum(descriptor.toString))
        if (t.isFailure) false
        else t.get == checksum
    }
  }

  /** Implement polynomial algorithm for descriptors
    * @see
    *   [[https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki#checksum]]
    * @see
    *   [[https://github.com/bitcoin/bitcoin/blob/d1e9a02126634f9e2ca0b916b69b173a8646524d/src/script/descriptor.cpp#L90]]
    */
  private def polyMod(c: UInt64, idx: Int): UInt64 = {
    var res = c
    val c0: UInt8 = UInt8((c >> 35).toInt)
    res = (c & UInt64(0x7ffffffffL)) << 5 ^ idx
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
