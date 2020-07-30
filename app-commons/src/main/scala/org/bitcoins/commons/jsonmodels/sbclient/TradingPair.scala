package org.bitcoins.commons.jsonmodels.sbclient

import org.bitcoins.commons.jsonmodels.sbclient.Asset._
import org.bitcoins.commons.jsonmodels.sbclient.TradingPair.UnsupportedTradingPair

sealed abstract class TradingPair(left: Asset, right: Asset)
    extends Serializable {
  def toLowerString: String = this.toString.toLowerCase
  def toUpperString: String = this.toString.toUpperCase
  def getLeft: Asset = left
  def getRight: Asset = right

  def isSupportedBy(exchange: Exchange): Boolean = {
    exchange match {
      case Exchange.Bitfinex  => this.isInstanceOf[BitfinexTradingPair]
      case Exchange.Binance   => this.isInstanceOf[BinanceTradingPair]
      case Exchange.Coinbase  => this.isInstanceOf[CoinbaseTradingPair]
      case Exchange.Bitstamp  => this.isInstanceOf[BitstampTradingPair]
      case Exchange.Gemini    => this.isInstanceOf[GeminiTradingPair]
      case Exchange.Kraken    => this.isInstanceOf[KrakenTradingPair]
      case Exchange.KrakenFut => this.isInstanceOf[KrakenFutTradingPair]
      case Exchange.Bitmex    => this.isInstanceOf[BitmexTradingPair]
    }
  }
}

sealed trait BitfinexTradingPair extends TradingPair
sealed trait BinanceTradingPair extends TradingPair
sealed trait CoinbaseTradingPair extends TradingPair
sealed trait BitstampTradingPair extends TradingPair
sealed trait GeminiTradingPair extends TradingPair
sealed trait KrakenTradingPair extends TradingPair
sealed trait KrakenFutTradingPair extends TradingPair
sealed trait BitmexTradingPair extends TradingPair

object TradingPair {

  private val aliases = Vector("XBT", "XDG", "XLM", "BCHABC", "BAB")

  private def mapAlias(alias: String): String = {
    alias match {
      case "XBT"            => "BTC"
      case "XDG"            => "DOGE"
      case "XLM"            => "STR"
      case "BCHABC" | "BAB" => "BCH"
      case notAlias: String => notAlias
    }
  }

  private def handleAliasOfSize(size: Int, pair: String): String = {
    val safeToCheck = pair.length >= size
    lazy val leftIsAlias = aliases.contains(pair.take(size).toUpperCase)
    lazy val rightIsAlias = aliases.contains(pair.takeRight(size).toUpperCase)

    if (safeToCheck && leftIsAlias) {
      mapAlias(pair.take(size).toUpperCase) + pair.drop(size)
    } else if (safeToCheck && rightIsAlias) {
      pair.dropRight(size) + mapAlias(pair.takeRight(size).toUpperCase)
    } else {
      pair
    }
  }

  private def handleAlias(pair: String): String = {
    val handled3 = handleAliasOfSize(size = 3, pair = pair)

    handleAliasOfSize(size = 6, pair = handled3)
  }

  /** Reads a [[TradingPair trading pair]] from a string
    * returns None if it DNE
    */
  def fromStringOpt(pair: String): Option[TradingPair] = {
    val translatedPair = handleAlias(pair)

    val pairOpt =
      all.find(tradingPair =>
        tradingPair.toUpperString == translatedPair.toUpperCase)

    pairOpt
  }

  /** Reads a [[TradingPair trading pair]] from a string
    * returns a [[UnsupportedTradingPair unsupported traiding pair]]
    * that contains the given string if it does not exist.
    */
  def fromString(pair: String): TradingPair = {
    val translatedPair = handleAlias(pair)
    val pairOpt = fromStringOpt(pair)
    lazy val unsupported =
      UnsupportedTradingPair.fromString(translatedPair)
    pairOpt.getOrElse(unsupported)
  }

  case object BTCUSD
      extends TradingPair(BTC, USD)
      with BitfinexTradingPair
      with CoinbaseTradingPair
      with BitstampTradingPair
      with GeminiTradingPair
      with KrakenTradingPair
      with KrakenFutTradingPair
      with BitmexTradingPair

  case object ETHUSD
      extends TradingPair(ETH, USD)
      with BitfinexTradingPair
      with CoinbaseTradingPair
      with BitstampTradingPair
      with GeminiTradingPair
      with KrakenTradingPair
      with KrakenFutTradingPair
      with BitmexTradingPair

  case object ETHBTC
      extends TradingPair(ETH, BTC)
      with BitfinexTradingPair
      with BinanceTradingPair
      with CoinbaseTradingPair
      with BitstampTradingPair
      with GeminiTradingPair
      with KrakenTradingPair
      with BitmexTradingPair

  case object LTCUSD
      extends TradingPair(LTC, USD)
      with BitfinexTradingPair
      with CoinbaseTradingPair
      with BitstampTradingPair
      with GeminiTradingPair
      with KrakenTradingPair
      with KrakenFutTradingPair

  case object LTCBTC
      extends TradingPair(LTC, BTC)
      with BitfinexTradingPair
      with BinanceTradingPair
      with CoinbaseTradingPair
      with BitstampTradingPair
      with GeminiTradingPair
      with KrakenTradingPair
      with BitmexTradingPair

  case object LTCETH
      extends TradingPair(LTC, ETH)
      with BinanceTradingPair
      with GeminiTradingPair

  case object BCHUSD
      extends TradingPair(BCH, USD)
      with BitfinexTradingPair
      with CoinbaseTradingPair
      with BitstampTradingPair
      with GeminiTradingPair
      with KrakenTradingPair
      with KrakenFutTradingPair

  case object BCHBTC
      extends TradingPair(BCH, BTC)
      with BitfinexTradingPair
      with BinanceTradingPair
      with CoinbaseTradingPair
      with BitstampTradingPair
      with GeminiTradingPair
      with KrakenTradingPair
      with BitmexTradingPair

  case object BCHETH extends TradingPair(BCH, ETH) with GeminiTradingPair

  case object LTCBCH extends TradingPair(LTC, BCH) with GeminiTradingPair

  case object XRPUSD
      extends TradingPair(XRP, USD)
      with BitfinexTradingPair
      with BitstampTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair
      with KrakenFutTradingPair

  case object XRPBTC
      extends TradingPair(XRP, BTC)
      with BinanceTradingPair
      with BitfinexTradingPair
      with BitstampTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair
      with BitmexTradingPair
      with KrakenFutTradingPair

  case object XRPETH extends TradingPair(XRP, ETH) with BinanceTradingPair

  case object EOSUSD
      extends TradingPair(EOS, USD)
      with BitfinexTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair

  case object EOSBTC
      extends TradingPair(EOS, BTC)
      with BitfinexTradingPair
      with BinanceTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair
      with BitmexTradingPair

  case object EOSETH
      extends TradingPair(EOS, ETH)
      with BitfinexTradingPair
      with BinanceTradingPair
      with KrakenTradingPair

  case object BTCUSDT extends TradingPair(BTC, USDT) with BinanceTradingPair

  case object ETHUSDT extends TradingPair(ETH, USDT) with BinanceTradingPair

  case object LTCUSDT extends TradingPair(LTC, USDT) with BinanceTradingPair

  case object BCHUSDT extends TradingPair(BCH, USDT) with BinanceTradingPair

  case object XRPUSDT extends TradingPair(XRP, USDT) with BinanceTradingPair

  case object EOSUSDT extends TradingPair(EOS, USDT) with BinanceTradingPair

  case object EURUSD extends TradingPair(EUR, USD) with BitstampTradingPair

  case object BTCEUR
      extends TradingPair(BTC, EUR)
      with BitfinexTradingPair
      with BitstampTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair

  case object ETHEUR
      extends TradingPair(ETH, EUR)
      with BitfinexTradingPair
      with BitstampTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair

  case object LTCEUR
      extends TradingPair(LTC, EUR)
      with BitstampTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair

  case object BCHEUR
      extends TradingPair(BCH, EUR)
      with BitstampTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair

  case object XRPEUR
      extends TradingPair(XRP, EUR)
      with BitstampTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair

  case object EOSEUR
      extends TradingPair(EOS, EUR)
      with BitfinexTradingPair
      with CoinbaseTradingPair
      with KrakenTradingPair

  case class UnsupportedTradingPair(left: Asset, right: Asset)
      extends TradingPair(left, right)
      with BitfinexTradingPair
      with BinanceTradingPair
      with CoinbaseTradingPair
      with BitstampTradingPair
      with GeminiTradingPair
      with KrakenTradingPair
      with KrakenFutTradingPair
      with BitmexTradingPair {

    override def toLowerString: String = {
      s"$left$right".toLowerCase
    }

    override def toUpperString: String = {
      s"$left$right".toUpperCase
    }
  }

  object UnsupportedTradingPair {

    def fromString(pair: String): UnsupportedTradingPair = {
      val lowerPair = pair.toLowerCase
      val cut = Math.min(3, pair.length)

      UnsupportedTradingPair(
        Asset.fromLowerString(lowerPair.substring(0, cut)),
        Asset.fromLowerString(lowerPair.substring(cut))
      )
    }
  }

  val all: Vector[TradingPair] =
    Vector(
      BTCUSD,
      ETHUSD,
      ETHBTC,
      BTCUSDT,
      ETHUSDT,
      LTCUSD,
      LTCBTC,
      LTCETH,
      LTCUSDT,
      BCHUSD,
      BCHBTC,
      BCHETH,
      LTCBCH,
      BCHUSDT,
      XRPUSD,
      XRPBTC,
      XRPETH,
      XRPUSDT,
      EOSUSD,
      EOSBTC,
      EOSETH,
      EOSUSDT,
      EURUSD,
      BTCEUR,
      ETHEUR,
      BCHEUR,
      LTCEUR,
      XRPEUR,
      EOSEUR
    )

  def allForExchange(exchange: Exchange): Vector[TradingPair] = {
    exchange match {
      case Exchange.Binance   => BinanceTradingPair.all
      case Exchange.Bitfinex  => BitfinexTradingPair.all
      case Exchange.Bitstamp  => BitstampTradingPair.all
      case Exchange.Coinbase  => CoinbaseTradingPair.all
      case Exchange.Gemini    => GeminiTradingPair.all
      case Exchange.Kraken    => KrakenTradingPair.all
      case Exchange.Bitmex    => BitmexTradingPair.all
      case Exchange.KrakenFut => KrakenFutTradingPair.all
    }
  }
}

/** This trait is to avoid a bunch of copying code in MyExchangeTradingPair companion object */
trait TradingPairObject[PairType >: UnsupportedTradingPair <: TradingPair] {
  def all: Vector[PairType]

  def fromString(pair: String): PairType = {
    val tradingPair: TradingPair = TradingPair.fromString(pair)
    fromTradingPair(tradingPair)
  }

  def fromTradingPair(pair: TradingPair): PairType = {
    all
      .find(_ == pair)
      .getOrElse(UnsupportedTradingPair(pair.getLeft, pair.getRight))
  }
}

object BitfinexTradingPair extends TradingPairObject[BitfinexTradingPair] {

  def all: Vector[BitfinexTradingPair] =
    Vector(
      TradingPair.BTCUSD,
      TradingPair.ETHUSD,
      TradingPair.ETHBTC,
      TradingPair.LTCUSD,
      TradingPair.LTCBTC,
      TradingPair.BCHUSD,
      TradingPair.BCHBTC,
      TradingPair.XRPUSD,
      TradingPair.XRPBTC,
      TradingPair.EOSUSD,
      TradingPair.EOSBTC,
      TradingPair.EOSETH,
      TradingPair.BTCEUR,
      TradingPair.ETHEUR,
      TradingPair.EOSEUR
    )
}

object BinanceTradingPair extends TradingPairObject[BinanceTradingPair] {

  def all: Vector[BinanceTradingPair] =
    Vector(
      TradingPair.BTCUSDT,
      TradingPair.ETHUSDT,
      TradingPair.ETHBTC,
      TradingPair.LTCUSDT,
      TradingPair.LTCBTC,
      TradingPair.LTCETH,
      TradingPair.BCHUSDT,
      TradingPair.BCHBTC,
      TradingPair.XRPUSDT,
      TradingPair.XRPBTC,
      TradingPair.XRPETH,
      TradingPair.EOSUSDT,
      TradingPair.EOSBTC,
      TradingPair.EOSETH
    )
}

object CoinbaseTradingPair extends TradingPairObject[CoinbaseTradingPair] {

  def fromCoinbaseString(pair: String): CoinbaseTradingPair = {
    val noHyphenPair = pair.filter(_ != '-')
    CoinbaseTradingPair.fromString(noHyphenPair)
  }

  def all: Vector[CoinbaseTradingPair] =
    Vector(
      TradingPair.BTCUSD,
      TradingPair.ETHUSD,
      TradingPair.ETHBTC,
      TradingPair.LTCUSD,
      TradingPair.LTCBTC,
      TradingPair.BCHUSD,
      TradingPair.BCHBTC,
      TradingPair.XRPUSD,
      TradingPair.XRPBTC,
      TradingPair.EOSUSD,
      TradingPair.EOSBTC,
      TradingPair.BTCEUR,
      TradingPair.ETHEUR,
      TradingPair.LTCEUR,
      TradingPair.BCHEUR,
      TradingPair.XRPEUR,
      TradingPair.EOSEUR
    )
}

object BitstampTradingPair extends TradingPairObject[BitstampTradingPair] {

  def fromBitstampChannelName(channel: String): BitstampTradingPair = {
    val pieces = channel.split("_")

    // Bitstamp omits a currency pair for btcusd
    // (e.g. live_trades = live_trades_btcusd)
    if (pieces.length == 2) {
      TradingPair.BTCUSD
    } else {
      fromString(pieces.last)
    }
  }

  def all: Vector[BitstampTradingPair] =
    Vector(
      TradingPair.BTCUSD,
      TradingPair.ETHUSD,
      TradingPair.ETHBTC,
      TradingPair.LTCUSD,
      TradingPair.LTCBTC,
      TradingPair.BCHUSD,
      TradingPair.BCHBTC,
      TradingPair.XRPUSD,
      TradingPair.XRPBTC,
      TradingPair.EURUSD,
      TradingPair.BTCEUR,
      TradingPair.ETHEUR,
      TradingPair.LTCEUR,
      TradingPair.BCHEUR,
      TradingPair.XRPEUR
    )
}

object GeminiTradingPair extends TradingPairObject[GeminiTradingPair] {

  def all: Vector[GeminiTradingPair] =
    Vector(
      TradingPair.BTCUSD,
      TradingPair.ETHUSD,
      TradingPair.ETHBTC,
      TradingPair.LTCUSD,
      TradingPair.LTCBTC,
      TradingPair.LTCETH,
      TradingPair.BCHUSD,
      TradingPair.BCHBTC,
      TradingPair.BCHETH,
      TradingPair.LTCBCH
    )
}

object KrakenTradingPair extends TradingPairObject[KrakenTradingPair] {

  def fromKrakenString(pair: String): KrakenTradingPair = {
    val pairNoSeparators = pair.filter(_ != '/')
    KrakenTradingPair.fromString(pairNoSeparators)
  }

  def all: Vector[KrakenTradingPair] =
    Vector(
      TradingPair.BTCUSD,
      TradingPair.ETHUSD,
      TradingPair.ETHBTC,
      TradingPair.LTCUSD,
      TradingPair.LTCBTC,
      TradingPair.BCHUSD,
      TradingPair.BCHBTC,
      TradingPair.XRPUSD,
      TradingPair.XRPBTC,
      TradingPair.EOSUSD,
      TradingPair.EOSBTC,
      TradingPair.EOSETH,
      TradingPair.BTCEUR,
      TradingPair.ETHEUR,
      TradingPair.LTCEUR,
      TradingPair.BCHEUR,
      TradingPair.XRPEUR,
      TradingPair.EOSEUR
    )
}

object KrakenFutTradingPair extends TradingPairObject[KrakenFutTradingPair] {

  def all: Vector[KrakenFutTradingPair] =
    Vector(
      TradingPair.BTCUSD,
      TradingPair.ETHUSD,
      TradingPair.LTCUSD,
      TradingPair.BCHUSD,
      TradingPair.XRPUSD,
      TradingPair.XRPBTC
    )
}

object BitmexTradingPair extends TradingPairObject[BitmexTradingPair] {

  def all: Vector[BitmexTradingPair] =
    Vector(
      TradingPair.BTCUSD,
      TradingPair.ETHUSD,
      TradingPair.ETHBTC,
      TradingPair.LTCBTC,
      TradingPair.BCHBTC,
      TradingPair.XRPBTC,
      TradingPair.EOSBTC
    )
}
