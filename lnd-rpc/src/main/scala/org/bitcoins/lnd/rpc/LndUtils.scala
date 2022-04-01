package org.bitcoins.lnd.rpc

import com.google.protobuf.ByteString
import lnrpc.ChannelPoint.FundingTxid.FundingTxidBytes
import lnrpc.{ChannelPoint, OutPoint}
import org.bitcoins.commons.jsonmodels.lnd.TxDetails
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto._
import scodec.bits._
import signrpc.TxOut

import scala.language.implicitConversions

trait LndUtils {

  implicit def byteVecToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toArray)

  implicit def byteStringToByteVec(byteString: ByteString): ByteVector =
    ByteVector(byteString.toByteArray)

  implicit def outputToTxOut(output: TransactionOutput): TxOut =
    TxOut(output.value.satoshis.toLong, output.scriptPubKey.asmBytes)

  implicit def txOutToTxOutput(txOut: TxOut): TransactionOutput =
    TransactionOutput(Satoshis(txOut.value),
                      ScriptPubKey.fromAsmBytes(txOut.pkScript))

  implicit def outpointToTxOutPoint(op: OutPoint): TransactionOutPoint =
    TransactionOutPoint(DoubleSha256DigestBE(op.txidStr),
                        UInt32(op.outputIndex))

  implicit def txOutpointToOutpoint(outpoint: TransactionOutPoint): OutPoint =
    OutPoint(outpoint.txId.bytes, outpoint.txIdBE.hex, outpoint.vout.toInt)

  // If other kinds of Iterables are needed, there's a fancy thing to do
  // that is done all over the Seq code using params and an implicit CanBuildFrom
  implicit def outputVecToTxOuts(
      outputs: Vector[TransactionOutput]): Vector[TxOut] =
    outputs.map(outputToTxOut)

  implicit def outpointVecToTxOutPointVec(
      ops: Vector[OutPoint]): Vector[TransactionOutPoint] =
    ops.map(outpointToTxOutPoint)

  implicit def txOutpointToOutpointVec(
      ops: Vector[TransactionOutPoint]): Vector[OutPoint] =
    ops.map(txOutpointToOutpoint)

  implicit def byteStringVecToByteVecs(
      byteStrings: Vector[ByteString]): Vector[ByteVector] =
    byteStrings.map(byteStringToByteVec)

  implicit def channelPointToOutpoint(
      channelPoint: ChannelPoint): TransactionOutPoint = {
    val txIdBytes = channelPoint.fundingTxid.fundingTxidBytes.get
    TransactionOutPoint(DoubleSha256Digest(txIdBytes),
                        UInt32(channelPoint.outputIndex))
  }

  implicit def outPointToChannelPoint(
      outPoint: TransactionOutPoint): ChannelPoint = {
    val txId = FundingTxidBytes(outPoint.txId.bytes)
    ChannelPoint(txId, outPoint.vout.toInt)
  }

  implicit def LndTransactionToTxDetails(
      details: lnrpc.Transaction): TxDetails = {
    val blockHashOpt = if (details.blockHash.isEmpty) {
      None
    } else Some(DoubleSha256DigestBE(details.blockHash))

    val addrs =
      details.destAddresses.map(BitcoinAddress.fromString).toVector

    TxDetails(
      txId = DoubleSha256DigestBE(details.txHash),
      amount = Satoshis(details.amount),
      numConfirmations = details.numConfirmations,
      blockHashOpt = blockHashOpt,
      blockHeight = details.blockHeight,
      timeStamp = details.timeStamp,
      totalFees = Satoshis(details.totalFees),
      destAddresses = addrs,
      tx = Transaction(details.rawTxHex),
      label = details.label
    )
  }
}

object LndUtils extends LndUtils
