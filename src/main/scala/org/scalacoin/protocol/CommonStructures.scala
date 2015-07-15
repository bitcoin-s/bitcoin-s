package org.scalacoin.protocol

/**
 * Created by chris on 7/14/15.
 */

/**
 * Variable length integer
 * Integer can be encoded depending on the represented value to save space.
 * Variable length integers always precede an array/vector of a type of data that may vary in length.
 * Longer numbers are encoded in little endian.
 * @param value
 */
case class VarInt( value : String)
