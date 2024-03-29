package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.StringFactory

/** @see [[https://github.com/bitcoin/bitcoin/blob/master/doc/descriptors.md#features]] */
sealed abstract class DescriptorType

sealed abstract class ScriptDescriptorType extends DescriptorType {
  def scriptType: ScriptType
}

object DescriptorType extends StringFactory[ScriptDescriptorType] {

  case object PK extends ScriptDescriptorType {
    override val scriptType = ScriptType.PUBKEY
    override val toString: String = "pk"
  }

  case object PKH extends ScriptDescriptorType {
    override val scriptType = ScriptType.PUBKEYHASH
    override val toString: String = "pkh"
  }

  case object SH extends ScriptDescriptorType {
    override val scriptType = ScriptType.SCRIPTHASH
    override val toString: String = "sh"
  }

  case object WPKH extends ScriptDescriptorType {
    override val scriptType = ScriptType.WITNESS_V0_KEYHASH
    override val toString: String = "wpkh"
  }

  case object WSH extends ScriptDescriptorType {
    override val scriptType = ScriptType.WITNESS_V0_SCRIPTHASH
    override val toString: String = "wsh"
  }

  case object TR extends ScriptDescriptorType {
    override val scriptType = ScriptType.WITNESS_V1_TAPROOT
    override val toString: String = "tr"
  }

  case object Multi extends ScriptDescriptorType {
    override val scriptType = ScriptType.MULTISIG
    override val toString: String = "multi"
  }

  case object SortedMulti extends ScriptDescriptorType {
    override val scriptType = ScriptType.MULTISIG
    override val toString: String = "sortedmulti"
  }

  case object Raw extends ScriptDescriptorType {
    override val scriptType: ScriptType = ScriptType.NONSTANDARD
    override val toString: String = s"raw"
  }

  private val all: Vector[ScriptDescriptorType] = Vector(
    PK,
    PKH,
    WPKH,
    WSH,
    TR,
    Multi,
    SortedMulti,
    Raw
  )

  override def fromStringOpt(string: String): Option[ScriptDescriptorType] = {
    all.find(d => string.startsWith(d.toString))
  }

  override def fromString(string: String): ScriptDescriptorType = {
    fromStringOpt(string) match {
      case Some(d) => d
      case None =>
        sys.error(s"Could not find descriptor type for string=$string")
    }
  }
}
