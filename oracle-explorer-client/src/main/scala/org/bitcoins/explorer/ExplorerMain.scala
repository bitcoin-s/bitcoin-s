package org.bitcoins.explorer

import akka.actor.ActorSystem
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.explorer.client.SbExplorerClient
import org.bitcoins.explorer.env.ExplorerEnv

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

object ExplorerMain extends App {

  implicit val system: ActorSystem = ActorSystem()
  val env = ExplorerEnv.Production
  val explorerApi = new SbExplorerClient(env)

  val hash = Sha256Digest.fromHex(
    "78b04cca31f575164f8d591edba3838dcf802eccdbd7ed91a380a5593cb210c4")
  val eventsF = explorerApi.getEvent(hash)
  val events = Await.result(eventsF, 30.seconds)

  println(s"e=$events")
}
