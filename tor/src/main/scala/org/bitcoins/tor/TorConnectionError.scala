package org.bitcoins.tor

sealed trait TorConnectionError {
  def prefix: Byte

  def exn: RuntimeException = new RuntimeException(toString)
}

object TorConnectionError {

  val connectErrors: Map[Byte, String] = Map[Byte, String](
    (0x00, "Request granted"),
    (0x01, "General failure"),
    (0x02, "Connection not allowed by ruleset"),
    (0x03, "Network unreachable"),
    (0x04, "Host unreachable"),
    (0x05, "Connection refused by destination host"),
    (0x06, "TTL expired"),
    (0x07, "Command not supported / protocol error"),
    (0x08, "Address type not supported")
  )

  private val all: Vector[TorConnectionError] = Vector(
    RequestGranted,
    GeneralFailure,
    ConnectionNotAllowed,
    NetworkUnreachable,
    HostUnreachable,
    ConnectionRefusedByDestination,
    TTLExpired,
    CommandNotSupportedOrProtocolError,
    AddressTypeNotSupported
  )

  def fromByte(byte: Byte): TorConnectionError = {
    fromByteOpt(byte) match {
      case Some(err) => err
      case None =>
        sys.error(s"Could not find tor connection error by prefix=$byte")
    }
  }

  def fromByteOpt(byte: Byte): Option[TorConnectionError] = {
    all.find(_.prefix == byte)
  }

  case object RequestGranted extends TorConnectionError {
    override val prefix: Byte = 0.toByte
  }

  case object GeneralFailure extends TorConnectionError {
    override val prefix: Byte = 1.toByte

    override def toString: String =
      s"General failure, this likely means tor timed out when trying to connect. Try using telnet to reproduce error."
  }

  case object ConnectionNotAllowed extends TorConnectionError {
    override val prefix: Byte = 0x02.toByte
    override val toString: String = "Connection not allowed by ruleset"
  }

  case object NetworkUnreachable extends TorConnectionError {
    override val prefix: Byte = 0x03.toByte
  }

  case object HostUnreachable extends TorConnectionError {
    override val prefix: Byte = 0x04.toByte
  }

  case object ConnectionRefusedByDestination extends TorConnectionError {
    override val prefix: Byte = 0x05.toByte
  }

  case object TTLExpired extends TorConnectionError {
    override val prefix: Byte = 0x06.toByte
  }

  case object CommandNotSupportedOrProtocolError extends TorConnectionError {
    override val prefix: Byte = 0x07.toByte
  }

  case object AddressTypeNotSupported extends TorConnectionError {
    override val prefix: Byte = 0x08.toByte
  }
}
