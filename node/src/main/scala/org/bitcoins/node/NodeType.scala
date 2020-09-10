package org.bitcoins.node

import org.bitcoins.crypto.StringFactory

sealed abstract class NodeType {
  def shortName: String
}

object NodeType extends StringFactory[NodeType] {

  final case object FullNode extends NodeType {
    override def shortName: String = "full"
  }

  final case object NeutrinoNode extends NodeType {
    override def shortName: String = "neutrino"
  }

  final case object SpvNode extends NodeType {
    override def shortName: String = "spv"
  }

  val all: Vector[NodeType] = Vector(FullNode, NeutrinoNode, SpvNode)

  override def fromStringOpt(str: String): Option[NodeType] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase) match {
      case Some(value) => Some(value)
      case None =>
        all.find(state => str.toLowerCase() == state.shortName.toLowerCase)
    }
  }

  override def fromString(string: String): NodeType = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find a NodeType for string=$string")
    }
  }
}
