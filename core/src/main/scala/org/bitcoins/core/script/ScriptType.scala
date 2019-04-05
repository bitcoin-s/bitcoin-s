package org.bitcoins.core.script

/**
  * The different Bitcoin Script type variations
  *
  * @see [[https://github.com/bitcoin/bitcoin/blob/fa6180188b8ab89af97860e6497716405a48bab6/src/script/standard.h#L56 standard.h]]
  *     and [[https://github.com/bitcoin/bitcoin/blob/03732f8644a449af34f4df1bb3b8915fb15ef22c/src/script/standard.cpp#L27 standarc.cpp]]
  *     from Bitcoin Core
  */
sealed abstract class ScriptType {
  import org.bitcoins.core.script.ScriptType._
  override def toString = this match {
    case NONSTANDARD           => "nonstandard"
    case PUBKEY                => "pubkey"
    case PUBKEYHASH            => "pubkeyhash"
    case SCRIPTHASH            => "scripthash"
    case MULTISIG              => "multisig"
    case NULLDATA              => "nulldata"
    case WITNESS_V0_KEYHASH    => "witness_v0_keyhash"
    case WITNESS_V0_SCRIPTHASH => "witness_v0_scripthash"
    case WITNESS_UNKNOWN       => "witness_unknown"
  }
}

/**
  * The different Bitcoin Script type variations
  *
  * @see [[https://github.com/bitcoin/bitcoin/blob/fa6180188b8ab89af97860e6497716405a48bab6/src/script/standard.h#L56 standard.h]]
  *     and [[https://github.com/bitcoin/bitcoin/blob/03732f8644a449af34f4df1bb3b8915fb15ef22c/src/script/standard.cpp#L27 standarc.cpp]]
  *     from Bitcoin Core
  */
object ScriptType {
  private val all: Seq[ScriptType] = Vector(NONSTANDARD,
                                            PUBKEY,
                                            PUBKEYHASH,
                                            SCRIPTHASH,
                                            MULTISIG,
                                            NULLDATA,
                                            WITNESS_V0_KEYHASH,
                                            WITNESS_V0_SCRIPTHASH,
                                            WITNESS_UNKNOWN)

  def fromString(string: String): Option[ScriptType] =
    all.find(_.toString == string)

  /** Throws if given string is invalid */
  def fromStringExn(string: String): ScriptType =
    fromString(string)
      .getOrElse(
        throw new IllegalArgumentException(
          s"$string is not a valid script type!"))

  final case object NONSTANDARD extends ScriptType

  // ╔ "standard" transaction/script types
  // V
  final case object PUBKEY extends ScriptType
  final case object PUBKEYHASH extends ScriptType
  final case object SCRIPTHASH extends ScriptType
  final case object MULTISIG extends ScriptType

  /** unspendable OP_RETURN script that carries data */
  final case object NULLDATA extends ScriptType
  final case object WITNESS_V0_KEYHASH extends ScriptType
  final case object WITNESS_V0_SCRIPTHASH extends ScriptType

  /** Only for Witness versions not already defined */
  final case object WITNESS_UNKNOWN extends ScriptType
}
