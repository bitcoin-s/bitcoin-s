import java.io.File

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
/**
* This is for example purposes only!
  * This shows how to peer a bitcoin-s wallet
  * with a bitcoind instance that is relaying
  * information about what is happening on the blockchain
  * to the bitcoin-s wallet.
  *
  * This is useful if you want more flexible signing
  * procedures in the JVM ecosystem and more
  * granular control over your utxos with
  * popular databases like postgres, sqlite etc
  */


val time = System.currentTimeMillis()
//boiler plate config
implicit val system = ActorSystem(s"wallet-scala-sheet-${time}")
import system.dispatcher

//bitcoind config
val username = "USER_NAME"
val pass = "ENTER_RANDOM_PASSWORD_HERE"
val rpcPort = 5345
val zmqPort = 29000

val datadir = s"/tmp/bitcoin-${time}/bitcoin.conf"
val datadirFile = new File(datadir)
datadirFile.mkdirs()
datadirFile.createNewFile()

val configMap = Map(
  "rpcuser" -> username,
  "rpcpassword" -> pass,
  "rpcport" -> rpcPort,
  "daemon" -> "1",
  "server" -> "1",
  "debug" -> "1",
  "regtest" -> "1",
  "zmqpubhashtx" -> s"tcp://127.0.0.1:$zmqPort",
  "zmqpubhashblock" -> s"tcp://127.0.0.1:$zmqPort",
  "zmqpubrawtx" -> s"tcp://127.0.0.1:$zmqPort",
  "zmqpubrawblock" -> s"tcp://127.0.0.1:$zmqPort",

  //where bitcoind blocks etc is written to
  "datadir" -> datadir
).asJava


val config = ConfigFactory.parseMap(configMap)

//helper method to convert from typesafe config style
//to the bitcoin.conf config style and writes to the bitcoin.conf
BitcoindRpcTestUtil.writeConfigToFile(config)

//construct bitcoind
val instance = BitcoindInstance.fromConfig(config = config)
val bitcoind = new BitcoindV17RpcClient(instance = instance)

//start bitcoind, this may take a little while
val bitcoindF = bitcoind.start().map(_ => bitcoind)
val blockCountF = bitcoindF.flatMap(_.getBlockCount)
Await.result(blockCountF, 10.seconds)
//print block count as sanity check
blockCountF.onComplete(count => println(s"blockCount=${count}"))

//let's create a bitcoin-s wallet

//first generate a mnemonic
val entropy = MnemonicCode.getEntropy256Bits
val mnemonic = MnemonicCode.fromEntropy(entropy = entropy)


//clean everything up
val stoppedBitcoindF = bitcoindF.flatMap(_.stop())
val systemTermF = stoppedBitcoindF.flatMap { _ =>
  datadirFile.delete()
  system.terminate()
}