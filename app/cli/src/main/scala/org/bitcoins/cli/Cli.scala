package org.bitcoins.cli

import scopt.OParser
import org.bitcoins.core.config.NetworkParameters

import CliReaders._
import org.bitcoins.core.protocol._
import org.bitcoins.core.currency._

case class Config(
    wallet: WalletConfig = WalletConfig(),
    chain: ChainConfig = ChainConfig(),
    node: NodeConfig = NodeConfig(),
    network: Option[NetworkParameters] = None,
    debug: Boolean = false
)

case class WalletConfig(
    balance: Boolean = false,
    newAddress: Boolean = false,
    sendToAddress: Boolean = false,
    address: Option[BitcoinAddress] = None,
    amount: Option[Bitcoins] = None
)

case class ChainConfig(
    height: Boolean = false,
    bestHash: Boolean = false,
)

case class NodeConfig(
    peers: Boolean = false
)

object Cli extends App {

  val builder = OParser.builder[Config]

  val parser = {
    import builder._
    OParser.sequence(
      programName("bitcoin-s-cli"),
      opt[NetworkParameters]('n', "network")
        .action((np, conf) => conf.copy(network = Some(np)))
        .text("Select the active network."),
      opt[Unit]("debug")
        .action((_, conf) => conf.copy(debug = true))
        .text("Print debugging information"),
      cmd("getblockcount")
        .action((_, conf) => conf.copy(chain = conf.chain.copy(height = true)))
        .text(s"Get the block height"),
      cmd("getbestblockhash")
        .action(
          (_, conf) => conf.copy(chain = conf.chain.copy(bestHash = true)))
        .text(s"Get the best block hash"),
      cmd("getbalance")
        .action((_, conf) =>
          conf.copy(wallet = conf.wallet.copy(balance = true)))
        .text("Get the wallet balance"),
      cmd("getnewaddress")
        .action((_, conf) =>
          conf.copy(wallet = conf.wallet.copy(newAddress = true)))
        .text("Get a new address"),
      cmd("sendtoaddress")
        .action((_, conf) =>
          conf.copy(wallet = conf.wallet.copy(sendToAddress = true)))
        .text("Send money to the given address")
        .children(
          opt[BitcoinAddress]("address").action((addr, conf) =>
            conf.copy(wallet = conf.wallet.copy(address = Some(addr)))),
          opt[Bitcoins]("amount").action((btc, conf) =>
            conf.copy(wallet = conf.wallet.copy(amount = Some(btc))))
        ),
      cmd("getpeers")
        .action((_, conf) => conf.copy(node = conf.node.copy(peers = true)))
        .text(s"List the connected peers"),
      help('h', "help").text("Display this help message and exit"),
      checkConfig {
        case Config(WalletConfig(false, false, false, _, _),
                    ChainConfig(false, false),
                    NodeConfig(false),
                    _,
                    _) =>
          failure("You need to provide a command!")

        case Config(WalletConfig(_, _, true, None, _), _, _, _, _) =>
          failure("You need to provide an address!")
        case Config(WalletConfig(_, _, true, _, None), _, _, _, _) =>
          failure("You need to provide an amount!!")
        case _ => success
      }
    )
  }

  // TODO make this dynamic
  val port = 9999
  val host = "localhost"

  val config: Config = OParser.parse(parser, args, Config()) match {
    case None       => sys.exit(1)
    case Some(conf) => conf
  }

  /** Prints the given message to stderr */
  def debug(message: Any): Unit = {
    if (config.debug) {
      System.err.println(s"DEBUG: $message")
    }
  }

  // TODO: http://eed3si9n.com/scopt4 composing command line parsers
  // to make this sane
  val path: String = config match {
    // balance
    case Config(WalletConfig(true, _, _, _, _), _, _, _, _) =>
      "wallet/getbalance"
    // new address
    case Config(WalletConfig(_, true, _, _, _), _, _, _, _) =>
      "wallet/getnewaddress"
    // sendtoaddress
    case Config(WalletConfig(_, _, true, addrOpt, bitcoinsOpt), _, _, _, _) =>
      (addrOpt, bitcoinsOpt) match {
        case (Some(addr), Some(bitcoins)) =>
          s"wallet/sendtoaddress/${addr.value}/${bitcoins.toBigDecimal.toString}"
        case _ =>
          sys.error(
            "PANIC: You must provide both address and bitcoin amount. This should have been picked up earlier...")
      }
    // height
    case Config(_, ChainConfig(true, _), _, _, _) => "chain/getblockcount"
    // besthash
    case Config(_, ChainConfig(_, true), _, _, _) => "chain/getbestblockhash"
    // peers
    case Config(_, _, NodeConfig(true), _, _) => "node/getpeers"
  }

  import com.softwaremill.sttp._
  implicit val backend = HttpURLConnectionBackend()
  val javaURI = new java.net.URI(s"http://$host:$port/$path")
  // TODO sttp URI provides a bunch of handy features, use these properly
  val sttpURI = Uri(javaURI)
  val request = sttp.get(sttpURI)
  debug(s"HTTP request: $request")
  val response = request.send()
  // TODO check status code, handle errors, etc
  debug(s"HTTP response:")
  debug(response)
  print(response.unsafeBody)
}
