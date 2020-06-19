package org.bitcoins.wallet.util

import org.bitcoins.core.hd._
import play.api.libs.json._
import scala.sys.process._

/** This program connects to a running Trezor, and gets
  * xpubs and addresses from legacy, segwit and nested
  * segwit accounts for mainnet. The intention here was
  * to also do this for testnet, but for some reason
  * my emulator refuses to work when specifying a
  * testnet BIP32 path. Something to look into in the
  * future.
  *
  * To replicate this:
  * 1) Boot up a Trezor emulator (https://github.com/trezor/trezor-firmware/blob/master/core/docs/emulator.md)
  * 2) Run Trezor bridge (instructions in link above)
  * 3) Go to trezor.io/start, restore wallet from seed
  * 4) Leave emulator open
  * 5) Run this program. Result gets printed to stdout.
  */
object GetAddresses extends App {

  import scala.language.implicitConversions
  implicit def string2Json(str: String): JsString = JsString(str)
  implicit def int2Json(int: Int): JsNumber = JsNumber(int)

  implicit def seq2Json[T](xs: Seq[T])(implicit conv: T => JsValue): JsArray =
    JsArray(xs.map(conv))

  def printerr(x: Any): Unit = System.err.println(x.toString())

  val accountInfo = for {
    constant <- HDPurposes.all
    coin <- List(HDCoinType.Bitcoin /*, HDCoinType.Testnet*/ )
    accountIndex <- 0 until 3
  } yield {
    val accountPath = BIP32Path(
      BIP32Node(constant.constant, hardened = true),
      BIP32Node(coin.toInt, hardened = true),
      BIP32Node(accountIndex, hardened = true)
    )

    val pathType =
      constant match {
        case HDPurposes.Legacy       => "legacy"
        case HDPurposes.NestedSegWit => "p2sh-segwit"
        case HDPurposes.SegWit       => "segwit"
        case other                   => throw new RuntimeException(s"Unexpected purpose $other")
      }

    val trezorPathType =
      constant match {
        case HDPurposes.Legacy       => "address"
        case HDPurposes.NestedSegWit => "p2shsegwit"
        case HDPurposes.SegWit       => "segwit"
        case other                   => throw new RuntimeException(s"Unexpected purpose $other")
      }

    val xpubCmd =
      s"""trezorctl get-public-node -n $accountPath -t $trezorPathType"""
    printerr(s"Executing cmd: $xpubCmd")
    val xpub = xpubCmd.!!.split("\n").last.split(": ").last

    val addresses = for {
      chainType <- List[HDChainType](HDChainType.Change, HDChainType.External)
      addressIndex <- 0 until 3
    } yield {
      val path = BIP32Path(
        BIP32Node(constant.constant, hardened = true),
        BIP32Node(coin.toInt, hardened = true),
        BIP32Node(accountIndex, hardened = true),
        BIP32Node(chainType.index, hardened = false),
        BIP32Node(addressIndex, hardened = false)
      )

      val addressCmd = s"trezorctl get-address -n $path -t $trezorPathType"
      printerr(s"Executing cmd: $addressCmd")
      val address = addressCmd.!!.split("\n").head

      val json = Json.toJson(
        Map[String, JsValue](
          "path" -> path.toString,
          "chain" -> chainType.toString,
          "addressIndex" -> addressIndex,
          "address" -> address
        )
      )
      json
    }

    val json = JsObject(
      Map[String, JsValue](
        "coin" -> coin.toString,
        "pathType" -> pathType,
        "account" -> accountIndex,
        "xpub" -> xpub,
        "addresses" -> addresses
      )
    )
    json
  }

  println(Json.stringify(JsArray(accountInfo)))
}
