package org.bitcoins.core.protocol.ln

// We extend AnyVal to avoid runtime allocation of new
// objects. See the Scala documentation on value classes
// and universal traits for more:
// https://docs.scala-lang.org/overviews/core/value-classes.html
package object currency {

  /** Provides natural language syntax for millisatoshis */
  implicit class MilliSatoshisInt(private val i: Int) extends AnyVal {
    def millisatoshis: MilliSatoshis = MilliSatoshis(i)
    def millisatoshi: MilliSatoshis = millisatoshis
    def msats: MilliSatoshis = millisatoshis
    def msat: MilliSatoshis = millisatoshis
  }

  /** Provides natural language syntax for millisatoshis */
  implicit class MilliSatoshisLong(private val i: Long) extends AnyVal {
    def millisatoshis: MilliSatoshis = MilliSatoshis(i)
    def millisatoshi: MilliSatoshis = millisatoshis
    def msats: MilliSatoshis = millisatoshis
    def msat: MilliSatoshis = millisatoshis
  }

  /** Provides natural language syntax for millibitcoins */
  implicit class MilliBitcoinsInt(private val i: Int) extends AnyVal {
    def millibitcoins: MilliBitcoins = MilliBitcoins(i)
    def millibitcoin: MilliBitcoins = millibitcoins
    def mBTC: MilliBitcoins = millibitcoins
  }

  /** Provides natural language syntax for millibitcoins */
  implicit class MilliBitcoinsLong(private val i: Long) extends AnyVal {
    def millibitcoins: MilliBitcoins = MilliBitcoins(i)
    def millibitcoin: MilliBitcoins = millibitcoins
    def mBTC: MilliBitcoins = millibitcoins
  }

  /** Provides natural language syntax for microbitcoins */
  implicit class MicroBitcoinsInt(private val i: Int) extends AnyVal {
    def microbitcoins: MicroBitcoins = MicroBitcoins(i)
    def microbitcoin: MicroBitcoins = microbitcoins
    def uBTC: MicroBitcoins = microbitcoins
  }

  /** Provides natural language syntax for microbitcoins */
  implicit class MicroBitcoinsLong(private val i: Long) extends AnyVal {
    def microbitcoins: MicroBitcoins = MicroBitcoins(i)
    def microbitcoin: MicroBitcoins = microbitcoins
    def uBTC: MicroBitcoins = microbitcoins
  }

  /** Provides natural language syntax for nanobitcoins */
  implicit class NanoBitcoinsInt(private val i: Int) extends AnyVal {
    def nanobitcoins: NanoBitcoins = NanoBitcoins(i)
    def nanobitcoin: NanoBitcoins = nanobitcoins
    def nBTC: NanoBitcoins = nanobitcoins
  }

  /** Provides natural language syntax for nanobitcoins */
  implicit class NanoBitcoinsLong(private val i: Long) extends AnyVal {
    def nanobitcoins: NanoBitcoins = NanoBitcoins(i)
    def nanobitcoin: NanoBitcoins = nanobitcoins
    def nBTC: NanoBitcoins = nanobitcoins
  }

  /** Provides natural language syntax for picobitcoins */
  implicit class PicoitcoinsInt(private val i: Int) extends AnyVal {
    def picobitcoins: PicoBitcoins = PicoBitcoins(i)
    def picobitcoin: PicoBitcoins = picobitcoins
    def pBTC: PicoBitcoins = picobitcoins
  }

  /** Provides natural language syntax for picobitcoins */
  implicit class PicoitcoinsLong(private val i: Long) extends AnyVal {
    def picobitcoins: PicoBitcoins = PicoBitcoins(i)
    def picobitcoin: PicoBitcoins = picobitcoins
    def pBTC: PicoBitcoins = picobitcoins
  }

}
