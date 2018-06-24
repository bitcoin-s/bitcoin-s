package org.bitcoins.rpc.serializers

import java.io.File
import java.net.{InetAddress, URI}

import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  ECPublicKey,
  Sha256Hash160Digest
}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.{Int32, Int64, UInt32, UInt64}
import org.bitcoins.core.protocol.{
  Address,
  BitcoinAddress,
  P2PKHAddress,
  P2SHAddress
}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.{BitcoinFeeUnit, SatoshisPerByte}
import org.bitcoins.rpc.jsonmodels.RpcAddress
import play.api.libs.json._

import scala.util.{Failure, Success}

object JsonReaders {
  // For use in implementing reads method of Reads[T] where T is constructed from a JsNumber via numFunc
  private def processJsNumber[T](numFunc: BigDecimal => T)(
      json: JsValue): JsResult[T] = json match {
    case JsNumber(n) => JsSuccess(numFunc(n))
    case err @ (JsNull | _: JsBoolean | _: JsString | _: JsArray |
        _: JsObject) =>
      JsError(s"error.expected.jsnumber, got ${Json.toJson(err).toString()}")
  }

  // For use in implementing reads method of Reads[T] where T is constructed from a JsString via strFunc
  private def processJsString[T](strFunc: String => T)(
      json: JsValue): JsResult[T] = json match {
    case JsString(s) => JsSuccess(strFunc(s))
    case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
        _: JsObject) =>
      JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
  }

  implicit object BigIntReads extends Reads[BigInt] {
    override def reads(json: JsValue): JsResult[BigInt] =
      processJsNumber[BigInt](_.toBigInt())(json)
  }

  implicit object DoubleSha256DigestReads extends Reads[DoubleSha256Digest] {
    override def reads(json: JsValue): JsResult[DoubleSha256Digest] =
      processJsString[DoubleSha256Digest](DoubleSha256Digest.fromHex)(json)
  }

  implicit object BitcoinsReads extends Reads[Bitcoins] {
    override def reads(json: JsValue): JsResult[Bitcoins] =
      processJsNumber[Bitcoins](Bitcoins(_))(json)
  }

  implicit object SatoshisReads extends Reads[Satoshis] {
    override def reads(json: JsValue): JsResult[Satoshis] =
      processJsNumber[Satoshis](num => Satoshis(Int64(num.toBigInt)))(json)
  }

  implicit object BlockHeaderReads extends Reads[BlockHeader] {
    override def reads(json: JsValue): JsResult[BlockHeader] =
      processJsString[BlockHeader](BlockHeader.fromHex)(json)
  }

  implicit object Int32Reads extends Reads[Int32] {
    override def reads(json: JsValue): JsResult[Int32] = json match {
      case JsNumber(n) =>
        n.toBigIntExact() match {
          case Some(num) => JsSuccess(Int32(num))
          case None      => JsError(s"error.expected.Int32, got $n")
        }
      case JsString(s) => JsSuccess(Int32.fromHex(s))
      case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
        JsError(s"error.expected.jsnumber, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object UInt32Reads extends Reads[UInt32] {
    override def reads(json: JsValue): JsResult[UInt32] = json match {
      case JsNumber(n) =>
        n.toBigIntExact() match {
          case Some(num) =>
            if (num >= 0) {
              JsSuccess(UInt32(num))
            } else {
              JsError(s"error.expected.positive_value, got $num")
            }
          case None => JsError(s"error.expected.UInt32, got $n")
        }
      case JsString(s) => JsSuccess(UInt32.fromHex(s))
      case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
        JsError(s"error.expected.jsnumber, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object UInt64Reads extends Reads[UInt64] {
    override def reads(json: JsValue): JsResult[UInt64] = json match {
      case JsNumber(n) =>
        n.toBigIntExact() match {
          case Some(num) =>
            if (num >= 0) {
              JsSuccess(UInt64(num))
            } else {
              JsError(s"error.expected.positive_value, got $num")
            }
          case None => JsError(s"error.expected.UInt32, got $n")
        }
      case JsString(s) => JsSuccess(UInt64.fromHex(s))
      case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
        JsError(s"error.expected.jsnumber, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object AddressReads extends Reads[Address] {
    override def reads(json: JsValue): JsResult[Address] = json match {
      case JsString(s) =>
        Address.fromString(s) match {
          case Success(address) => JsSuccess(address)
          case Failure(err) =>
            JsError(s"error.expected.address, got ${err.toString}")
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
          _: JsObject) =>
        JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  // Errors for Unit return types are caught in RpcClient::parseResult
  implicit object UnitReads extends Reads[Unit] {
    override def reads(json: JsValue): JsResult[Unit] = JsSuccess(Unit)
  }

  implicit object InetAddressReads extends Reads[InetAddress] {
    override def reads(json: JsValue): JsResult[InetAddress] =
      processJsString[InetAddress](InetAddress.getByName)(json)
  }

  implicit object ScriptPubKeyReads extends Reads[ScriptPubKey] {
    override def reads(json: JsValue): JsResult[ScriptPubKey] =
      processJsString[ScriptPubKey](ScriptPubKey.fromAsmHex)(json)
  }

  implicit object BlockReads extends Reads[Block] {
    override def reads(json: JsValue): JsResult[Block] =
      processJsString[Block](Block.fromHex)(json)
  }

  implicit object Sha256Hash160DigestReads extends Reads[Sha256Hash160Digest] {
    override def reads(json: JsValue): JsResult[Sha256Hash160Digest] =
      processJsString[Sha256Hash160Digest](Sha256Hash160Digest.fromHex)(json)
  }

  implicit object ECPublicKeyReads extends Reads[ECPublicKey] {
    override def reads(json: JsValue): JsResult[ECPublicKey] =
      processJsString[ECPublicKey](ECPublicKey.fromHex)(json)
  }

  implicit object P2PKHAddressReads extends Reads[P2PKHAddress] {
    override def reads(json: JsValue): JsResult[P2PKHAddress] = json match {
      case JsString(s) =>
        P2PKHAddress.fromString(s) match {
          case Success(address) => JsSuccess(address)
          case Failure(err) =>
            JsError(s"error.expected.p2pkhaddress, got ${err.toString}")
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
          _: JsObject) =>
        JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object P2SHAddressReads extends Reads[P2SHAddress] {
    override def reads(json: JsValue): JsResult[P2SHAddress] = json match {
      case JsString(s) =>
        P2SHAddress.fromString(s) match {
          case Success(address) => JsSuccess(address)
          case Failure(err) =>
            JsError(s"error.expected.p2shaddress, got ${err.toString}")
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
          _: JsObject) =>
        JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object TransactionInputReads extends Reads[TransactionInput] {
    override def reads(json: JsValue): JsResult[TransactionInput] = {
      val sequence = (json \ "sequence").validate[UInt32] match {
        case s: JsSuccess[UInt32] => s.value
        case _                    => return JsError("error.expected.vin.sequence")
      }
      (json \ "coinbase").validate[String] match {
        case s: JsSuccess[String] =>
          JsSuccess(
            CoinbaseInput(ScriptSignature.fromAsmHex(s.value), sequence))
        case _ =>
          val txid = (json \ "txid").validate[DoubleSha256Digest] match {
            case id: JsSuccess[DoubleSha256Digest] => id.value
            case _                                 => return JsError("error.expected.vin.txid")
          }
          val vout = (json \ "vout").validate[UInt32] match {
            case ind: JsSuccess[UInt32] => ind.value
            case _                      => return JsError("error.expected.vin.vout")
          }
          val scriptSig = (json \ "scriptSig" \ "hex").validate[String] match {
            case s: JsSuccess[String] => ScriptSignature.fromAsmHex(s.value)
            case _                    => return JsError("error.expected.vin.scriptSig")
          }

          JsSuccess(
            TransactionInput(TransactionOutPoint(txid, vout),
                             scriptSig,
                             sequence))
      }
    }
  }

  implicit object BitcoinAddressReads extends Reads[BitcoinAddress] {
    override def reads(json: JsValue): JsResult[BitcoinAddress] = json match {
      case JsString(s) =>
        BitcoinAddress.fromString(s) match {
          case Success(address) => JsSuccess(address)
          case Failure(err) =>
            JsError(s"error.expected.address, got ${err.toString}")
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
          _: JsObject) =>
        JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object MerkleBlockReads extends Reads[MerkleBlock] {
    override def reads(json: JsValue): JsResult[MerkleBlock] =
      processJsString[MerkleBlock](MerkleBlock.fromHex)(json)
  }

  implicit object TransactionReads extends Reads[Transaction] {
    override def reads(json: JsValue): JsResult[Transaction] =
      processJsString[Transaction](Transaction.fromHex)(json)
  }

  implicit object TransactionOutPointReads extends Reads[TransactionOutPoint] {
    private case class OutPoint(txid: DoubleSha256Digest, vout: UInt32)
    override def reads(json: JsValue): JsResult[TransactionOutPoint] = {
      implicit val outPointReads: Reads[OutPoint] = Json.reads[OutPoint]
      json.validate[OutPoint] match {
        case JsSuccess(op, _) =>
          JsSuccess(TransactionOutPoint(op.txid, op.vout))
        case JsError(err) =>
          JsError(s"Could not parse TransactionOutPoint, got ${err.toString()}")
      }
    }
  }

  // This still needs cleanup
  implicit object RpcAddressReads extends Reads[RpcAddress] {
    override def reads(json: JsValue): JsResult[RpcAddress] = json match {
      case array: JsArray =>
        val balance = array.value.find(_.isInstanceOf[JsNumber]) match {
          case Some(JsNumber(n)) => Bitcoins(n)
          case Some(
              err @ (JsNull | _: JsBoolean | _: JsString | _: JsArray |
              _: JsObject)) =>
            return JsError(
              s"error.expected.jsnumber, got ${Json.toJson(err).toString()}")
          case None => return JsError("error.expected.balance")
        }
        val jsStrings: IndexedSeq[JsString] = array.value
          .filter(_.isInstanceOf[JsString])
          .map(_.asInstanceOf[JsString])
        val address =
          jsStrings.find(s => BitcoinAddress.isValid(s.value)) match {
            case Some(JsString(s)) =>
              BitcoinAddress.fromString(s) match {
                case Success(a) => a
                case Failure(err) =>
                  return JsError(s"error.expected.address, got ${err.toString}")
              }
            case None => return JsError("error.expected.address")
          }
        jsStrings.find(s => !BitcoinAddress.isValid(s.value)) match {
          case Some(JsString(s)) =>
            JsSuccess(RpcAddress(address, balance, Some(s)))
          case None => JsSuccess(RpcAddress(address, balance, None))
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsString |
          _: JsObject) =>
        JsError(s"error.expected.jsarray, got ${Json.toJson(err).toString()}")
    }
  }

  // Currently takes in BTC/kB
  implicit object BitcoinFeeUnitReads extends Reads[BitcoinFeeUnit] {
    override def reads(json: JsValue): JsResult[BitcoinFeeUnit] =
      processJsNumber[BitcoinFeeUnit](num =>
        SatoshisPerByte(Satoshis(Int64((num * 100000).toBigInt()))))(json)
  }

  implicit object FileReads extends Reads[File] {
    override def reads(json: JsValue): JsResult[File] =
      processJsString[File](new File(_))(json)
  }

  implicit object URIReads extends Reads[URI] {
    override def reads(json: JsValue): JsResult[URI] =
      processJsString[URI](str => new URI("http://" + str))(json)
  }
}
