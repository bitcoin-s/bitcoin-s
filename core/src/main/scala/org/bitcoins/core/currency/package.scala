package org.bitcoins.core

// We extend AnyVal to avoid runtime allocation of new
// objects. See the Scala documentation on value classes
// and universal traits for more:
// https://docs.scala-lang.org/overviews/core/value-classes.html
package object currency {

  /** Provides natural language syntax for bitcoins */
  implicit class BitcoinsInt(private val i: Int) extends AnyVal {
    def bitcoins: Bitcoins = Bitcoins(i)
    def bitcoin: Bitcoins = bitcoins
    def BTC: Bitcoins = bitcoins
  }

  /** Provides natural language syntax for bitcoins */
  implicit class BitcoinsLong(private val i: Long) extends AnyVal {
    def bitcoins: Bitcoins = Bitcoins(i)
    def bitcoin: Bitcoins = bitcoins
    def BTC: Bitcoins = bitcoins
  }

  /** Provides natural language syntax for satoshis */
  implicit class SatoshisInt(private val i: Int) extends AnyVal {
    def satoshis: Satoshis = Satoshis(i)
    def satoshi: Satoshis = satoshis
    def sats: Satoshis = satoshis
    def sat: Satoshis = satoshis
  }

  /** Provides natural language syntax for satoshis */
  implicit class SatoshisLong(private val i: Long) extends AnyVal {
    def satoshis: Satoshis = Satoshis(i)
    def satoshi: Satoshis = satoshis
    def sats: Satoshis = satoshis
    def sat: Satoshis = satoshis
  }
}
