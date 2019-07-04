package org.bitcoins.core

import org.scalameter.api._
import org.bitcoins.core.number.UInt8
import scala.util.Try
import org.bitcoins.core.number.UInt32

object NumberBench extends Bench.OfflineReport {

  val UInt8RangeGen = Gen.exponential("UInt8 underlying")(from = 1,
                                                          until =
                                                            UInt8.max.toInt,
                                                          factor = 2)

  val SingleUInt8Gen = for {
    int <- UInt8RangeGen
  } yield UInt8(int)

  val PairUInt8Gen: Gen[(UInt8, UInt8)] = for {
    first <- SingleUInt8Gen
    second <- SingleUInt8Gen
  } yield {
    (first, second)
  }

  performance of "UInt8" in {
    measure method "addition" in {
      using(PairUInt8Gen) in {
        case (first, second) =>
          Try { first + second }
      }
    }

    measure method "subtraction" in {
      using(PairUInt8Gen) in {
        case (first, second) =>
          Try { first - second }
      }
    }

    measure method "multiplication" in {
      using(PairUInt8Gen) in {
        case (first, second) =>
          Try { first * second }
      }
    }

    measure method "bitwise AND" in {
      using(PairUInt8Gen) in {
        case (first, second) =>
          first & second
      }
    }

    measure method "bitwise OR" in {
      using(PairUInt8Gen) in {
        case (first, second) =>
          first | second
      }
    }

    measure method "bitshift <<" in {
      using(PairUInt8Gen) in {
        case (first, second) =>
          first << second
      }
    }

    measure method "bitshift >>" in {
      using(PairUInt8Gen) in {
        case (first, second) =>
          first >> second

      }
    }
  }

  val UInt32RangeGen = Gen.exponential("UInt32 underlying")(
    from = 1,
    until = Int.MaxValue / 2,
    factor = 2)

  val SingleUInt32Gen = for {
    int <- UInt32RangeGen
  } yield UInt32(int)

  val PairUInt32Gen: Gen[(UInt32, UInt32)] = for {
    first <- SingleUInt32Gen
    second <- SingleUInt32Gen
  } yield {
    (first, second)
  }

  performance of "UInt32" in {
    measure method "addition" in {
      using(PairUInt32Gen) in {
        case (first, second) =>
          first + second
      }
    }

    measure method "subtraction" in {
      using(PairUInt32Gen) in {
        case (first, second) =>
          first - second
      }
    }

    measure method "multiplication" in {
      using(PairUInt32Gen) in {
        case (first, second) =>
          Try { first * second }
      }
    }

    measure method "bitwise AND" in {
      using(PairUInt32Gen) in {
        case (first, second) =>
          first & second
      }
    }

    measure method "bitwise OR" in {
      using(PairUInt32Gen) in {
        case (first, second) =>
          first | second
      }
    }

    measure method "bitshift <<" in {
      using(PairUInt32Gen) in {
        case (first, second) =>
          first << second
      }
    }

    measure method "bitshift >>" in {
      using(PairUInt32Gen) in {
        case (first, second) =>
          first >> second
      }
    }
  }
}
