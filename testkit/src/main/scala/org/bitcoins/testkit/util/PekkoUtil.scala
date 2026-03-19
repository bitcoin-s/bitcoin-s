package org.bitcoins.testkit.util

import org.apache.pekko.actor.ActorSystem

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.FiniteDuration

trait PekkoUtil {

  /** Returns a future that will sleep until the given duration has passed */
  def nonBlockingSleep(
      duration: FiniteDuration
  )(implicit system: ActorSystem): Future[Unit] = {
    val p = Promise[Unit]()
    system.scheduler
      .scheduleOnce(duration)(p.success(()))(using system.dispatcher)
    p.future
  }

}

object PekkoUtil extends PekkoUtil
