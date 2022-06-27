package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.number.UInt16
import org.bitcoins.crypto.NetworkElement
import scodec.bits.ByteVector

sealed trait OracleParamsTLV extends NetworkElement

case class OracleParamsV0TLV(
    maxErrorExp: Int,
    minFailExp: Int,
    maximizeCoverage: Boolean)
    extends OracleParamsTLV {

  override val bytes: ByteVector = {
    UInt16(maxErrorExp).bytes ++
      UInt16(minFailExp).bytes ++
      TLVUtil.boolBytes(maximizeCoverage)
  }
}

object OracleParamsV0TLV extends FactoryOptionTLV[OracleParamsV0TLV] {

  override def fromBytes(
      bytes: ByteVector): OptionDLCType[OracleParamsV0TLV] = {
    if (bytes.head == 0) {
      NoneDLCType
    } else {
      require(
        bytes.head == 1,
        s"OracleParamsV0TLV has wrong optional subtype, got=${bytes.head}")
      val iter = ValueIterator(bytes.drop(1))

      val maxErrorExp = iter.takeU16().toInt
      val minFailExp = iter.takeU16().toInt
      val maximizeCoverage = iter.takeBoolean()

      val p = OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage)
      SomeDLCType(p)
    }
  }
}
