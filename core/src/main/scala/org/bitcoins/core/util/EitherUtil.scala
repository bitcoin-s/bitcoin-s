package org.bitcoins.core.util

import scala.concurrent.Future

object EitherUtil {

  def flattenFutureE[L, R](
      e: Either[L, Future[Either[L, R]]]): Future[Either[L, R]] = {

    def ifLeft(left: L): Future[Either[L, R]] = Future.successful(Left(left))
    def ifRight(rightF: Future[Either[L, R]]): Future[Either[L, R]] = rightF

    e.fold(ifLeft, ifRight)
  }
}
