package org.bitcoins.tor

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.settings.{
  ClientConnectionSettings,
  ConnectionPoolSettings
}
import akka.util.ByteString
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import java.net.InetSocketAddress
import scala.util.Properties

class Socks5ClientTransportSpec extends BitcoinSAsyncTest {

  val torEnabled = Properties
    .envOrNone("TOR")
    .isDefined

  implicit val ec = system.dispatcher

  val proxyParams = Socks5ProxyParams(
    address = InetSocketAddress.createUnresolved("127.0.0.1", 9050),
    credentialsOpt = None,
    randomizeCredentials = true)

  val socks5ClientTransport = new Socks5ClientTransport(proxyParams)

  val clientConnectionSettings =
    ClientConnectionSettings(system).withTransport(socks5ClientTransport)

  val settings = ConnectionPoolSettings(system).withConnectionSettings(
    clientConnectionSettings)

  it should "handle clear net addresses" in {
    assume(torEnabled, "TOR environment variable is not set")
    for {
      response <- Http().singleRequest(
        HttpRequest(uri = "http://blockstream.info/"),
        settings = settings)
    } yield {
      // should redirect to the HTTPS endpoint
      assert(response.status.intValue() == 301)
      assert(
        response.headers
          .find(_.lowercaseName() == "location")
          .map(_.value())
          .contains("https://blockstream.info/"))
    }
  }

  it should "handle onion addresses" in {
    assume(torEnabled, "TOR environment variable is not set")
    for {
      response <- Http().singleRequest(
        HttpRequest(uri =
          "http://explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion/"),
        settings = settings)
      html <- response.entity.dataBytes
        .runFold(ByteString(""))(_ ++ _)
        .map(x => x.utf8String)
    } yield {
      assert(response.status.intValue() == 200)
      assert(html.contains("Bitcoin Explorer"))
    }
  }

}
