package org.bitcoins.commons.jsonmodels.wallet

import org.bitcoins.crypto.{DoubleSha256DigestBE, StringFactory}

sealed abstract class WalletStateDescriptorType

object WalletStateDescriptorType
    extends StringFactory[WalletStateDescriptorType] {

  final case object SyncHeight extends WalletStateDescriptorType

  val all: Vector[WalletStateDescriptorType] = Vector(SyncHeight)

  override def fromStringOpt(str: String): Option[WalletStateDescriptorType] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }

  override def fromString(string: String): WalletStateDescriptorType = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(
          s"Could not find WalletStateDescriptorType for string=$string")
    }
  }
}

sealed abstract class WalletStateDescriptor {
  def descriptorType: WalletStateDescriptorType
}

sealed trait WalletStateDescriptorFactory[T <: WalletStateDescriptor]
    extends StringFactory[T] {
  def tpe: WalletStateDescriptorType
}

object WalletStateDescriptor extends StringFactory[WalletStateDescriptor] {

  val all: Vector[StringFactory[WalletStateDescriptor]] = Vector(
    SyncHeightDescriptor)

  override def fromString(string: String): WalletStateDescriptor = {
    all.find(f => f.fromStringT(string).isSuccess) match {
      case Some(factory) => factory.fromString(string)
      case None =>
        sys.error(s"Could not find WalletStateDescriptor for string=$string")
    }
  }
}

case class SyncHeightDescriptor(bestHash: DoubleSha256DigestBE, height: Int)
    extends WalletStateDescriptor {

  override val descriptorType: WalletStateDescriptorType =
    WalletStateDescriptorType.SyncHeight
  override val toString: String = s"${bestHash.hex} $height"
}

object SyncHeightDescriptor
    extends WalletStateDescriptorFactory[SyncHeightDescriptor] {

  override val tpe: WalletStateDescriptorType =
    WalletStateDescriptorType.SyncHeight

  override def fromString(string: String): SyncHeightDescriptor = {
    val arr = string.split(' ').take(2)

    val hash = DoubleSha256DigestBE(arr.head)
    val height = arr.last.toInt

    SyncHeightDescriptor(hash, height)
  }
}
