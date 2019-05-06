import akka.actor.ActorSystem
import org.bitcoins.core.config._
import org.bitcoins.rpc.config._
import org.bitcoins.rpc.client.common._
import java.net.URI

//this script shows you have to connect to a remote bitcoind
//instance via an ssh tunnel with bitcoin-s
//first we need to create the ssh tunnel

//$ ssh -L 8332:localhost:8332 my-cool-user@my-cool-website.com

//note: the port number '8332' is for mainnet, if you want to
//conect to a testnet rpc client you will need to do '18332'

//now we have a secure connection between our remote bitcoind
//node that is running on 'my-cool-website.com' under the
//username 'my-cool-user'

//it should be noted, that the steps above can be skipped and you
//can connect to a running local bitcoind instance as well

val username = "FILL_ME_IN" //this username comes from 'rpcuser' in your bitcoin.conf file
val password = "FILL_ME_IN" //this password comes from your 'rpcpassword' in your bitcoin.conf file
val rpcPort = 8332 //this is default port for mainnet, 18332 for testnet/regtest


val authCredentials = BitcoindAuthCredentials(
  username = username,
  password = password,
  rpcPort = rpcPort
)

val bitcoindInstance = {
  BitcoindInstance (
    network = MainNet,
    uri = new URI(s"http://localhost:${authCredentials.rpcPort + 1}"),
    rpcUri = new URI(s"http://localhost:${authCredentials.rpcPort}"),
    authCredentials = authCredentials
  )
}

implicit val system = ActorSystem(s"connnect-bitcoind-ssh-${System.currentTimeMillis()}")
implicit val ec = system.dispatcher
val rpcCli = new BitcoindRpcClient(bitcoindInstance)

rpcCli.getBalance.onComplete { case balance =>
  println(s"Wallet balance=${balance}")
  system.terminate()
}
