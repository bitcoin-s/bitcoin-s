package org.bitcoins.server

import upickle.default._
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.currency.Bitcoins
import ujson.Value

import scala.util.Failure
import scala.util.Try
import scala.util.Success

// TODO ID?
case class ServerCommand(method: String, params: ujson.Arr)

object ServerCommand {
  implicit val rw: ReadWriter[ServerCommand] = macroRW
}

case class Rescan(
    addresses: Vector[BitcoinAddress],
    startBlock: Option[BlockStamp],
    endBlock: Option[BlockStamp])

object Rescan {

  def fromJsArr(jsArr: ujson.Arr): Try[Rescan] = {

    def parseAddresses(addrsJs: Value) = {
      addrsJs.arr.toVector.map(js => BitcoinAddress.fromStringExn(js.str))
    }

    jsArr.arr.toList match {
      case addrsJs :: startJs :: endJs :: Nil =>
        try {
          val addresses = parseAddresses(addrsJs)
          val start = BlockStamp.fromString(startJs.str).get
          val end = BlockStamp.fromString(endJs.str).get
          Success(
            Rescan(addresses = addresses,
                   startBlock = Option(start),
                   endBlock = Option(end)))
        } catch {
          case e: Throwable => Failure(e)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing address and amount argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }

}

case class SendToAddress(address: BitcoinAddress, amount: Bitcoins)

object SendToAddress {

  /// TODO do this in a more coherent fashion
  // custom akka-http directive?
  def fromJsArr(jsArr: ujson.Arr): Try[SendToAddress] = {
    jsArr.arr.toList match {
      case addrJs :: bitcoinsJs :: Nil =>
        try {
          val address = BitcoinAddress.fromStringExn(addrJs.str)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          Success(SendToAddress(address, bitcoins))
        } catch {
          case e: Throwable => Failure(e)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing address and amount argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }

}
