package org.bitcoins.sbclient

sealed trait RequestType {
  def requestString: String
}

object RequestType {

  case object RValue extends RequestType {
    override def requestString: String = "rvalue"
  }

  case object LastSig extends RequestType {
    override def requestString: String = "lastsig"
  }

  val all: Vector[RequestType] = Vector(RValue, LastSig)
}
