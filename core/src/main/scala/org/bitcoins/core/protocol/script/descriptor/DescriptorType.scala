package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.StringFactory

/** @see
  *   [[https://github.com/bitcoin/bitcoin/blob/master/doc/descriptors.md#features]]
  */
sealed abstract class DescriptorType

sealed abstract class ScriptDescriptorType extends DescriptorType {
  def scriptType: ScriptType
}

object ScriptDescriptorType extends StringFactory[ScriptDescriptorType] {
  private[descriptor] val all: Vector[ScriptDescriptorType] = Vector(
    PK,
    PKH,
    WPKH,
    WSH,
    TR,
    Multi,
    SortedMulti,
    Raw,
    SH,
    Combo
  )
  case object PK extends ScriptDescriptorType {
    override val scriptType: org.bitcoins.core.script.ScriptType.PUBKEY.type =
      ScriptType.PUBKEY
    override val toString: String = "pk"
  }

  case object PKH extends ScriptDescriptorType {
    override val scriptType
        : org.bitcoins.core.script.ScriptType.PUBKEYHASH.type =
      ScriptType.PUBKEYHASH
    override val toString: String = "pkh"
  }

  case object SH extends ScriptDescriptorType {
    override val scriptType
        : org.bitcoins.core.script.ScriptType.SCRIPTHASH.type =
      ScriptType.SCRIPTHASH
    override val toString: String = "sh"
  }

  case object WPKH extends ScriptDescriptorType {
    override val scriptType
        : org.bitcoins.core.script.ScriptType.WITNESS_V0_KEYHASH.type =
      ScriptType.WITNESS_V0_KEYHASH
    override val toString: String = "wpkh"
  }

  case object WSH extends ScriptDescriptorType {
    override val scriptType
        : org.bitcoins.core.script.ScriptType.WITNESS_V0_SCRIPTHASH.type =
      ScriptType.WITNESS_V0_SCRIPTHASH
    override val toString: String = "wsh"
  }

  case object TR extends ScriptDescriptorType {
    override val scriptType
        : org.bitcoins.core.script.ScriptType.WITNESS_V1_TAPROOT.type =
      ScriptType.WITNESS_V1_TAPROOT
    override val toString: String = "tr"
  }

  case object Multi extends ScriptDescriptorType {
    override val scriptType: org.bitcoins.core.script.ScriptType.MULTISIG.type =
      ScriptType.MULTISIG
    override val toString: String = "multi"
  }

  case object SortedMulti extends ScriptDescriptorType {
    override val scriptType: org.bitcoins.core.script.ScriptType.MULTISIG.type =
      ScriptType.MULTISIG
    override val toString: String = "sortedmulti"
  }

  case object Raw extends ScriptDescriptorType {
    override val scriptType: ScriptType = ScriptType.NONSTANDARD
    override val toString: String = s"raw"
  }

  case object Combo extends ScriptDescriptorType {
    // this is wrong, combo doesn't have a specific script type?
    override val scriptType: ScriptType = ScriptType.PUBKEYHASH
    override val toString: String = "combo"
  }

  override def fromStringOpt(string: String): Option[ScriptDescriptorType] = {
    val (dType, _) = string.span(_ != '(')
    all.find(d => dType == d.toString)
  }

  override def fromString(string: String): ScriptDescriptorType = {
    fromStringOpt(string) match {
      case Some(d) => d
      case None =>
        sys.error(s"Could not find descriptor type for string=$string")
    }
  }
}

object DescriptorType extends StringFactory[DescriptorType] {

  case object Addr extends DescriptorType {
    override val toString: String = "addr"
  }

  private val all: Vector[DescriptorType] = ScriptDescriptorType.all ++ Vector(
    Addr
  )

  override def fromStringOpt(string: String): Option[DescriptorType] = {
    val (dType, _) = string.span(_ != '(')
    all.find(d => dType == d.toString)
  }

  override def fromString(string: String): DescriptorType = {
    fromStringOpt(string) match {
      case Some(d) => d
      case None =>
        sys.error(s"Could not find descriptor type for string=$string")
    }
  }
}
