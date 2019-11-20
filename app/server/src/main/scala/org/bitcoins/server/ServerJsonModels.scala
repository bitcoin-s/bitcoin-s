package org.bitcoins.server

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import ujson.{Null, Num, Str, Value}
import upickle.default._

import scala.util.{Failure, Try}

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

    def parseAddresses(addrsJs: Value): Vector[BitcoinAddress] = {
      addrsJs.arr.toVector.map(js => BitcoinAddress.fromStringExn(js.str))
    }

    def nullToOpt(value: Value): Option[Value] = value match {
      case Null => None
      case _    => Some(value)
    }

    def parseBlockStamp(value: Value): Option[BlockStamp] =
      nullToOpt(value).map {
        case Str(value) => BlockStamp.fromString(value).get
        case Num(value) =>
          val int = value.toInt
          if (int >= 0 && int <= Int.MaxValue)
            BlockHeight(int)
          else throw Value.InvalidData(value, "Expected a positive integer")
        case _: Value =>
          throw Value.InvalidData(value, "Expected a Num or a Str")
      }

    jsArr.arr.toList match {
      case addrsJs :: startJs :: endJs :: Nil =>
        Try {
          val addresses = parseAddresses(addrsJs)
          val start = parseBlockStamp(startJs)
          val end = parseBlockStamp(endJs)
          Rescan(addresses = addresses, startBlock = start, endBlock = end)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing addresses"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
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
        Try {
          val address = BitcoinAddress.fromStringExn(addrJs.str)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          SendToAddress(address, bitcoins)
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
