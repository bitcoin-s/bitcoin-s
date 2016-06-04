package org.bitcoins.core.number

/**
  * Created by chris on 6/4/16.
  */
sealed trait NumberType {
  def + (num : NumberType) : NumberType
  def - (num : NumberType) : NumberType
  def * (num : NumberType) : NumberType
}

sealed trait SignedNumberType extends NumberType

sealed trait UnsignedNumberType extends NumberType

sealed trait UInt32 extends UnsignedNumberType

sealed trait UInt64 extends UnsignedNumberType

sealed trait Int32 extends SignedNumberType

sealed trait Int64 extends SignedNumberType
