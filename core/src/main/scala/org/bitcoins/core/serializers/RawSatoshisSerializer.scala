package org.bitcoins.core.serializers

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.Int64

/**
 * Created by chris on 6/23/16.
 */
trait RawSatoshisSerializer extends RawBitcoinSerializer[Satoshis] {

  def read(bytes: scodec.bits.ByteVector): Satoshis = Satoshis(Int64(bytes.reverse))

  def write(satoshis: Satoshis): scodec.bits.ByteVector = {
    Int64(satoshis.toLong).bytes.reverse
  }

}

object RawSatoshisSerializer extends RawSatoshisSerializer
