package org.bitcoins.rpc.serializers

import java.net.InetAddress

import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.{Address, BitcoinAddress, P2PKHAddress, P2SHAddress}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction._
import play.api.libs.json._

import scala.util.{Failure, Success}

object JsonReaders {
  implicit object DoubleSha256DigestReads extends Reads[DoubleSha256Digest] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(DoubleSha256Digest.fromHex(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object BitcoinsReads extends Reads[Bitcoins] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) => JsSuccess(Bitcoins(n))
      case err => JsError(s"error.expected.jsnumber, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object BlockHeaderReads extends Reads[BlockHeader] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(BlockHeader.fromHex(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object Int32Reads extends Reads[Int32] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.toBigIntExact() match {
        case Some(num) => JsSuccess(Int32(num))
        case None => JsError(s"error.expected.Int32, got $n")
      }
      case JsString(s) => JsSuccess(Int32.fromHex(s))
      case err => JsError(s"error.expected.jsnumber, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object UInt32Reads extends Reads[UInt32] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.toBigIntExact() match {
        case Some(num) =>
          if (num >= 0)
            JsSuccess(UInt32(num))
          else
            JsError(s"error.expected.positive_value, got $num")
        case None => JsError(s"error.expected.UInt32, got $n")
      }
      case JsString(s) => JsSuccess(UInt32.fromHex(s))
      case err => JsError(s"error.expected.jsnumber, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object AddressReads extends Reads[Address] {
    def reads(json: JsValue) = json match {
      case JsString(s) => Address.fromString(s) match {
        case Success(address) => JsSuccess(address)
        case Failure(err) => JsError(s"error.expected.address, got ${err.toString}")
      }
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object UnitReads extends Reads[Unit] {
    def reads(json: JsValue) = null
  }

  implicit object InetAddressReads extends Reads[InetAddress] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(InetAddress.getByName(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object ScriptPubKeyReads extends Reads[ScriptPubKey] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(ScriptPubKey.fromHex(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object BlockReads extends Reads[Block] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(Block.fromHex(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object Sha256Hash160DigestReads extends Reads[Sha256Hash160Digest] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(Sha256Hash160Digest.fromHex(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object ECPublicKeyReads extends Reads[ECPublicKey] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(ECPublicKey.fromHex(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object P2PKHAddressReads extends Reads[P2PKHAddress] {
    def reads(json: JsValue) = json match {
      case JsString(s) => P2PKHAddress.fromString(s) match {
        case Success(address) => JsSuccess(address)
        case Failure(err) => JsError(s"error.expected.p2pkhaddress, got ${err.toString}")
      }
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object P2SHAddressReads extends Reads[P2SHAddress] {
    def reads(json: JsValue) = json match {
      case JsString(s) => P2SHAddress.fromString(s) match {
        case Success(address) => JsSuccess(address)
        case Failure(err) => JsError(s"error.expected.p2shaddress, got ${err.toString}")
      }
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object TransactionInputReads extends Reads[TransactionInput] {
    def reads(json: JsValue): JsResult[TransactionInput] = {
      val sequence = (json \ "sequence").validate[UInt32] match {
        case s: JsSuccess[UInt32] => s.value
        case _ => return JsError("error.expected.vin.sequence")
      }
      (json \ "coinbase").validate[String] match {
        case s: JsSuccess[String] => JsSuccess(CoinbaseInput(ScriptSignature.fromAsmHex(s.value)))
        case _ =>
          val txid = (json \ "txid").validate[DoubleSha256Digest] match {
            case id: JsSuccess[DoubleSha256Digest] => id.value
            case _ => return JsError("error.expected.vin.txid")
          }
          val vout = (json \ "vout").validate[UInt32] match {
            case ind: JsSuccess[UInt32] => ind.value
            case _ => return JsError("error.expected.vin.vout")
          }
          val scriptSig = (json \ "scriptSig" \ "hex").validate[String] match {
            case s: JsSuccess[String] => ScriptSignature.fromAsmHex(s.value)
            case _ => return JsError("error.expected.vin.scriptSig")
          }

          JsSuccess(TransactionInput(TransactionOutPoint(txid, vout),scriptSig, sequence))
      }
    }
  }

  implicit object BitcoinAddressReads extends Reads[BitcoinAddress] {
    def reads(json: JsValue) = json match {
      case JsString(s) => BitcoinAddress.fromString(s) match {
        case Success(address) => JsSuccess(address)
        case Failure(err) => JsError(s"error.expected.address, got ${err.toString}")
      }
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object MerkleBlockReads extends Reads[MerkleBlock] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(MerkleBlock.fromHex(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object TransactionReads extends Reads[Transaction] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(Transaction.fromHex(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }
}