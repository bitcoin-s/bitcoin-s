import sbt._
import java.nio.file.Paths

object Projects {
  val core = project in file("..") / "core"
  val eclairRpc = project in file("..") / "eclair-rpc"
  val bitcoindRpc = project in file("..") / "bitcoind-rpc"
  val secp256k1jni = project in file("..") / "secp256k1jni "
}
