package org.bitcoins.lnurl

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.util.{Failure, Success}

class LnURLTest extends BitcoinSUnitTest {

  it must "correctly encode" in {
    val url =
      "https://service.com/api?q=3fc3645b439ce8e7f2553a69e5267081d96dcd340693afabe04be7b0ccd178df"

    val expected =
      "LNURL1DP68GURN8GHJ7UM9WFMXJCM99E3K7MF0V9CXJ0M385EKVCENXC6R2C35XVUKXEFCV5MKVV34X5EKZD3EV56NYD3HXQURZEPEXEJXXEPNXSCRVWFNV9NXZCN9XQ6XYEFHVGCXXCMYXYMNSERXFQ5FNS"

    assert(LnURL.fromURL(url).toString.toUpperCase == expected)
  }

  it must "correctly decode" in {
    val str =
      "LNURL1DP68GURN8GHJ7UM9WFMXJCM99E3K7MF0V9CXJ0M385EKVCENXC6R2C35XVUKXEFCV5MKVV34X5EKZD3EV56NYD3HXQURZEPEXEJXXEPNXSCRVWFNV9NXZCN9XQ6XYEFHVGCXXCMYXYMNSERXFQ5FNS"

    val expected =
      "https://service.com/api?q=3fc3645b439ce8e7f2553a69e5267081d96dcd340693afabe04be7b0ccd178df"

    LnURL.decode(str) match {
      case Failure(exception) => fail(exception)
      case Success(value)     => assert(value == expected)
    }
  }
}
