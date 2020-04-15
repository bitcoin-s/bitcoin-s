package org.bitcoins.sbclient

sealed trait Exchange {

  def pairs: Vector[TradingPair]

  /**
    * The unique string representation of of this exchange.
    * Used to differentiate between spot and futures exchanges,
    * e.g. Kraken vs KrakenFut etc
    */
  def toLongString: String

  /** The unique string representation of this exchange where
    * bitmex is not bitmexfut.
    */
  def toGeneralString: String = {
    if (this == Exchange.Bitmex) {
      "bitmex"
    } else {
      this.toLongString
    }
  }
}

sealed trait SpotExchange extends Exchange

sealed trait FuturesExchange extends Exchange

object Exchange {

  /** Spot exchanges located in US, this is useful for log filtering */
  val allUSSpot: Vector[SpotExchange] = Vector(Coinbase, Gemini, Kraken)

  /** International spot exchanges, this is useful for log filtering */
  val allInternationalSpot: Vector[SpotExchange] =
    Vector(Bitfinex, Binance, Bitstamp)

  val allSpot: Vector[SpotExchange] =
    allUSSpot ++ allInternationalSpot

  val allFut: Vector[FuturesExchange] = Vector(KrakenFut, Bitmex)

  val all: Vector[Exchange] = allSpot ++ allFut

  // Bitmex is special because we want to accept both bitmexfut and bitmex
  val acceptedHistoricalNames: Vector[String] =
    all.map(_.toLongString).:+("bitmex")

  case object Bitfinex extends SpotExchange {
    override def toString: String = "bitfinex"
    override def toLongString: String = "bitfinex"
    override def pairs: Vector[TradingPair] = BitfinexTradingPair.all
  }

  case object Binance extends SpotExchange {
    override def toString: String = "binance"
    override def toLongString: String = "binance"
    override def pairs: Vector[TradingPair] = BinanceTradingPair.all
  }

  case object Coinbase extends SpotExchange {
    override def toString: String = "coinbase"
    override def toLongString: String = "coinbase"
    override def pairs: Vector[TradingPair] = CoinbaseTradingPair.all
  }

  case object Bitstamp extends SpotExchange {
    override def toString: String = "bitstamp"
    override def toLongString: String = "bitstamp"
    override def pairs: Vector[TradingPair] = BitstampTradingPair.all
  }

  case object Bitmex extends FuturesExchange {
    override def toString: String = "bitmex"
    override def toLongString: String = "bitmexfut"
    override def pairs: Vector[TradingPair] = BitmexTradingPair.all
  }

  case object Gemini extends SpotExchange {
    override def toString: String = "gemini"
    override def toLongString: String = "gemini"
    override def pairs: Vector[TradingPair] = GeminiTradingPair.all
  }

  case object Kraken extends SpotExchange {
    override def toString: String = "kraken"
    override def toLongString: String = "kraken"
    override def pairs: Vector[TradingPair] = KrakenTradingPair.all
  }

  case object KrakenFut extends FuturesExchange {
    override def toString: String = "kraken"
    override def toLongString: String = "krakenfut"
    override def pairs: Vector[TradingPair] = KrakenFutTradingPair.all
  }
}

object SpotExchange {

  def fromString(exchange: String): Option[SpotExchange] = {
    exchange.toLowerCase match {
      case "bitfinex" => Some(Exchange.Bitfinex)
      case "binance"  => Some(Exchange.Binance)
      case "coinbase" => Some(Exchange.Coinbase)
      case "bitstamp" => Some(Exchange.Bitstamp)
      case "gemini"   => Some(Exchange.Gemini)
      case "kraken"   => Some(Exchange.Kraken)
      case _: String  => None
    }
  }

  override def toString: String = "spotexchange"
}

object FuturesExchange {

  def fromString(exchange: String): Option[FuturesExchange] = {
    exchange.toLowerCase match {
      case "bitmex"    => Some(Exchange.Bitmex)
      case "bitmexfut" => Some(Exchange.Bitmex)
      case "kraken"    => Some(Exchange.KrakenFut)
      case "krakenfut" => Some(Exchange.KrakenFut)
      case _: String   => None
    }
  }

  override def toString: String = "futuresexchange"
}
