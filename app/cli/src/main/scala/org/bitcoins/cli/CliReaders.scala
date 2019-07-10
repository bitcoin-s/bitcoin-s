package org.bitcoins.cli

import scopt._
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol._
import org.bitcoins.core.currency._
import org.bitcoins.core.config.Networks
import scala.util.Failure
import scala.util.Success

/** scopt readers for parsing CLI params and options */
object CliReaders {

  implicit val npReads: Read[NetworkParameters] =
    new Read[NetworkParameters] {
      val arity: Int = 1

      val reads: String => NetworkParameters = str =>
        Networks.knownNetworks
          .find(_.toString.toLowerCase == str.toLowerCase)
          .getOrElse {
            val networks =
              Networks.knownNetworks
                .map(_.toString.toLowerCase)
                .mkString(", ")
            val msg =
              s"$str is not a valid network! Valid networks: $networks"
            sys.error(msg)
        }
    }

  implicit val bitcoinAddressReads: Read[BitcoinAddress] =
    new Read[BitcoinAddress] {
      val arity: Int = 1

      val reads: String => BitcoinAddress = BitcoinAddress.fromStringExn
    }

  implicit val bitcoinsReads: Read[Bitcoins] =
    new Read[Bitcoins] {
      val arity: Int = 1
      val reads: String => Bitcoins = str => Bitcoins(BigDecimal(str))
    }
}
