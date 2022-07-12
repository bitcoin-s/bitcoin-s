package org.bitcoins.commons.rpc

import org.bitcoins.core.api.dlc.wallet.db.DLCContactDb
import org.bitcoins.crypto.{AesPassword, Sha256Digest}

import java.net.{InetSocketAddress, URI}
import scala.util.{Failure, Try}

sealed trait CommandRpc

sealed trait CliCommand {
  def defaultPort: Int
}

trait ServerlessCliCommand extends CliCommand {
  override def defaultPort: Int = 9999
}

trait AppServerCliCommand extends CliCommand {
  override def defaultPort: Int = 9999
}

trait OracleServerCliCommand extends CliCommand {
  override def defaultPort: Int = 9998
}

object CliCommand {

  case object NoCommand extends CliCommand {
    override val defaultPort: Int = 9999
  }
}

case class ContactAdd(alias: String, address: InetSocketAddress, memo: String)
    extends CommandRpc
    with AppServerCliCommand {
  def toDLCContactDb: DLCContactDb = DLCContactDb(alias, address, memo)
}

object ContactAdd {

  val empty: ContactAdd =
    ContactAdd("", InetSocketAddress.createUnresolved("127.0.0.1", 9999), "")

  def fromJsArr(arr: ujson.Arr): Try[ContactAdd] = {
    arr.arr.toList match {
      case aliasJs :: addressJs :: memoJs :: Nil =>
        Try {
          val address = {
            val uri = new URI(s"tcp://${addressJs.str}")
            InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
          }
          ContactAdd(aliasJs.str, address, memoJs.str)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to contact-add, got=${other.length} expected=3")
        Failure(exn)
    }
  }
}

case object ContactsList extends CommandRpc with AppServerCliCommand

case class ContactRemove(address: InetSocketAddress)
    extends CommandRpc
    with AppServerCliCommand

object ContactRemove {

  def fromJsArr(arr: ujson.Arr): Try[ContactRemove] = {
    arr.arr.toList match {
      case addressJs :: Nil =>
        Try {
          val address = {
            val uri = new URI(s"tcp://${addressJs.str}")
            InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
          }
          ContactRemove(address)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to contact-remove, got=${other.length} expected=1")
        Failure(exn)
    }
  }
}

case class DLCContactAdd(dlcId: Sha256Digest, address: InetSocketAddress)
    extends CommandRpc
    with AppServerCliCommand

object DLCContactAdd {

  val empty: DLCContactAdd =
    DLCContactAdd(Sha256Digest.empty,
                  InetSocketAddress.createUnresolved("127.0.0.1", 9999))

  def fromJsArr(arr: ujson.Arr): Try[DLCContactAdd] = {
    arr.arr.toList match {
      case dlcIdJs :: addressJs :: Nil =>
        Try {
          val dlcId = Sha256Digest.fromHex(dlcIdJs.str)
          val address = {
            val uri = new URI(s"tcp://${addressJs.str}")
            InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
          }
          DLCContactAdd(dlcId, address)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to dlc-contact-add, got=${other.length} expected=2")
        Failure(exn)
    }
  }
}

case class DLCContactRemove(dlcId: Sha256Digest)
    extends CommandRpc
    with AppServerCliCommand

object DLCContactRemove {

  def fromJsArr(arr: ujson.Arr): Try[DLCContactRemove] = {
    arr.arr.toList match {
      case dlcIdJs :: Nil =>
        Try {
          val dlcId = Sha256Digest.fromHex(dlcIdJs.str)
          DLCContactRemove(dlcId)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to contact-remove, got=${other.length} expected=1")
        Failure(exn)
    }
  }
}

case class LoadWallet(
    walletName: Option[String],
    password: Option[AesPassword],
    bip39Password: Option[String])
    extends CommandRpc
    with AppServerCliCommand

object LoadWallet {

  val empty: LoadWallet =
    LoadWallet(walletName = None, password = None, bip39Password = None)

  def fromJsArr(arr: ujson.Arr): Try[LoadWallet] = Try {
    arr.arr.toList match {
      case _ :: _ :: bip39PasswordJs :: Nil =>
        val (walletNameOpt, passwordOpt) =
          JsonRpcUtil.jsToWalletNameAndPassword(arr)
        LoadWallet(walletNameOpt,
                   passwordOpt,
                   JsonRpcUtil.nullToOpt(bip39PasswordJs).map(_.str))
      case _ :: _ :: Nil =>
        val (walletNameOpt, passwordOpt) =
          JsonRpcUtil.jsToWalletNameAndPassword(arr)
        LoadWallet(walletNameOpt, passwordOpt, None)
      case walletNameJs :: Nil =>
        LoadWallet(JsonRpcUtil.jsToStringOpt(walletNameJs), None, None)
      case Nil =>
        LoadWallet(None, None, None)
      case other =>
        throw new IllegalArgumentException(
          s"Bad number of arguments: ${other.length}. Expected: 3")
    }
  }
}
