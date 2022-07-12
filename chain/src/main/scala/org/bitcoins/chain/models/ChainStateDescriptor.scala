package org.bitcoins.chain.models

import org.bitcoins.crypto.StringFactory

sealed abstract class ChainStateDescriptorType

object ChainStateDescriptorType
    extends StringFactory[ChainStateDescriptorType] {

  final case object Syncing extends ChainStateDescriptorType

  val all: Vector[ChainStateDescriptorType] = Vector(Syncing)

  override def fromStringOpt(str: String): Option[ChainStateDescriptorType] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
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
    Vector(SyncDescriptor)

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
