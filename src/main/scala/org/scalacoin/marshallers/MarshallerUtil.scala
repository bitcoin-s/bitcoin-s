package org.scalacoin.marshallers

import org.scalacoin.marshallers.blockchain.softforks.EnforcementProgressMarshaller.EnforcementProgressFormatter
import org.scalacoin.marshallers.blockchain.softforks.RejectionProgressMarshaller.RejectionProgressFormatter
import org.scalacoin.marshallers.blockchain.softforks.SoftForkMarshaller.SoftForkFormatter
import org.scalacoin.marshallers.networking.NetworkConnectionsMarshaller.NetworkConnectionsFormatter
import org.scalacoin.marshallers.transaction.TransactionInputMarshaller.TransactionInputFormatter
import org.scalacoin.marshallers.transaction.TransactionOutputMarshaller.TransactionOutputFormatter
import org.scalacoin.protocol.BitcoinAddress
import org.scalacoin.protocol.blockchain.softforks.{SoftForks, RejectionProgress, EnforcementProgress}
import org.scalacoin.protocol.networking.NetworkConnections
import org.scalacoin.protocol.transaction.{TransactionOutput, TransactionInput}
import spray.json.{JsonWriter, JsArray, DefaultJsonProtocol, JsValue}
import scala.collection.breakOut

/**
 * Created by chris on 12/27/15.
 */
trait MarshallerUtil {
  def convertToAddressList(value : JsValue) : Seq[BitcoinAddress] = {
    import DefaultJsonProtocol._
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => BitcoinAddress(e.convertTo[String]))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing a list of bitcoin addresses")
    }
  }

  def convertToJsArray[T](seq : Seq[T])(implicit formatter : JsonWriter[T]) : JsArray  = {
    JsArray(seq.map(p =>
      formatter.write(p))(breakOut): Vector[JsValue])
  }


  def convertToTransactionInputList(value : JsValue) : Seq[TransactionInput] = {
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => TransactionInputFormatter.read(e))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing a list of transaction inputs")
    }
  }

  def convertToTransactionOutputList(value : JsValue) : Seq[TransactionOutput] = {
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => TransactionOutputFormatter.read(e))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing a list of transaction outputs")
    }
  }

  def convertToNetworkConnectionList(value : JsValue) : Seq[NetworkConnections] = {
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => NetworkConnectionsFormatter.read(e))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing a list of network connections")
    }
  }
  def convertToSoftForksList(value : JsValue) : Seq[SoftForks] = {
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => SoftForkFormatter.read(e))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing softforks info")
    }
  }
}
