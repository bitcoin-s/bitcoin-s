package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.StringFactory

/** @see [[https://github.com/bitcoin/bitcoin/blob/master/doc/descriptors.md#features]] */
sealed abstract class DescriptorType {
  def scriptType: ScriptType
}

object DescriptorType extends StringFactory[DescriptorType] {

  case object PK extends DescriptorType {
    override val scriptType = ScriptType.PUBKEY
    override val toString: String = "pk"
  }

  case object PKH extends DescriptorType {
    override val scriptType = ScriptType.PUBKEYHASH
    override val toString: String = "pkh"
  }

  case object SH extends DescriptorType {
    override val scriptType = ScriptType.SCRIPTHASH
    override val toString: String = "sh"
  }

  case object WPKH extends DescriptorType {
    override val scriptType = ScriptType.WITNESS_V0_KEYHASH
    override val toString: String = "wpkh"
  }

  case object WSH extends DescriptorType {
    override val scriptType = ScriptType.WITNESS_V0_SCRIPTHASH
    override val toString: String = "wsh"
  }

  case object TR extends DescriptorType {
    override val scriptType = ScriptType.WITNESS_V1_TAPROOT
    override val toString: String = "tr"
  }

  case object Multi extends DescriptorType {
    override val scriptType = ScriptType.MULTISIG
    override val toString: String = "multi"
  }

  case object SortedMulti extends DescriptorType {
    override val scriptType = ScriptType.MULTISIG
    override val toString: String = "sortedmulti"
  }

  private val all: Vector[DescriptorType] = Vector(
    PK,
    PKH,
    WPKH,
    WSH,
    TR,
    Multi,
    SortedMulti
  )

  override def fromStringOpt(string: String): Option[DescriptorType] = {
    all.find(d => string.startsWith(d.toString))
  }

  override def fromString(string: String): DescriptorType = {
    fromStringOpt(string) match {
      case Some(d) => d
      case None =>
        sys.error(s"Could not find descriptor type for string=$string")
    }
  }
}
