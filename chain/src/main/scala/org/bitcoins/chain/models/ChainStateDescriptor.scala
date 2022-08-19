package org.bitcoins.chain.models

import org.bitcoins.crypto.StringFactory

sealed abstract class ChainStateDescriptorType

object ChainStateDescriptorType
    extends StringFactory[ChainStateDescriptorType] {

  final case object Syncing extends ChainStateDescriptorType

  final case object IsInitialBlockDownload extends ChainStateDescriptorType

  val all: Vector[ChainStateDescriptorType] =
    Vector(IsInitialBlockDownload, Syncing)

  override def fromStringOpt(str: String): Option[ChainStateDescriptorType] = {
    val result =
      all.find(state => str.toLowerCase() == state.toString.toLowerCase)
    result
  }

  override def fromString(string: String): ChainStateDescriptorType = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find ChainStateDescriptorType for string=$string")
    }
  }
}

sealed abstract class ChainStateDescriptor {
  def descriptorType: ChainStateDescriptorType
}

sealed trait ChainStateDescriptorFactory[T <: ChainStateDescriptor]
    extends StringFactory[T] {
  def tpe: ChainStateDescriptorType
}

object ChainStateDescriptor extends StringFactory[ChainStateDescriptor] {

  val all: Vector[StringFactory[ChainStateDescriptor]] =
    Vector(IsInitialBlockDownload, SyncDescriptor)

  override def fromString(string: String): ChainStateDescriptor = {
    all.find(f => f.fromStringT(string).isSuccess) match {
      case Some(factory) => factory.fromString(string)
      case None =>
        sys.error(s"Could not find ChainStateDescriptor for string=$string")
    }
  }
}

case class SyncDescriptor(syncing: Boolean) extends ChainStateDescriptor {

  override val descriptorType: ChainStateDescriptorType =
    ChainStateDescriptorType.Syncing

  override val toString: String = syncing.toString
}

object SyncDescriptor extends ChainStateDescriptorFactory[SyncDescriptor] {

  override val tpe: ChainStateDescriptorType =
    ChainStateDescriptorType.Syncing

  override def fromString(string: String): SyncDescriptor = {
    val rescanning = java.lang.Boolean.parseBoolean(string)
    SyncDescriptor(rescanning)
  }
}

case class IsInitialBlockDownload(isIBDRunning: Boolean)
    extends ChainStateDescriptor {

  override val descriptorType: ChainStateDescriptorType =
    ChainStateDescriptorType.IsInitialBlockDownload
  override val toString = s"${IsInitialBlockDownload.prefix} $isIBDRunning"
}

object IsInitialBlockDownload
    extends ChainStateDescriptorFactory[IsInitialBlockDownload] {
  val prefix: String = "IsInitialBlockDownload".toLowerCase()

  override val tpe: ChainStateDescriptorType =
    ChainStateDescriptorType.IsInitialBlockDownload

  override def fromString(string: String): IsInitialBlockDownload = {
    fromStringOpt(string) match {
      case Some(ibd) => ibd
      case None =>
        sys.error(s"$string could not be parsed to InitialBlockDownload")
    }
  }

  override def fromStringOpt(string: String): Option[IsInitialBlockDownload] = {
    val arr = string.split(' ').take(2)

    if (arr(0).toLowerCase == prefix) {
      val isBool: Boolean = java.lang.Boolean.parseBoolean(arr(1))
      val result = IsInitialBlockDownload(isBool)
      Some(result)
    } else {
      None
    }
  }
}
