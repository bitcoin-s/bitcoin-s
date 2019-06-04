package org.bitcoins.core.util

import scala.concurrent.Future

object FutureUtil {

  val unit: Future[Unit] = Future.successful(())
}
