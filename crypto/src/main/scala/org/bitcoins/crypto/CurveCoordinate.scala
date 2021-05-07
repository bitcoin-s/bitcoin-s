package org.bitcoins.crypto

import scodec.bits.ByteVector

case class CurveCoordinate(bytes: ByteVector)
    extends FiniteFieldMember[CurveCoordinate](CryptoParams.getCurvePrime, 32) {

  override def fieldObj: FiniteFieldObject[CurveCoordinate] = CurveCoordinate
}

object CurveCoordinate
    extends FiniteFieldObject[CurveCoordinate](CryptoParams.getCurvePrime, 32) {

  override def fieldMemberConstructor(bytes: ByteVector): CurveCoordinate = {
    new CurveCoordinate(bytes)
  }
}
