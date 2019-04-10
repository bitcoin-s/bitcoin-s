package org.bitcoins.core.util

import scala.concurrent.Future

object EitherUtil {

  /**
    * Flattens a nested `Either[Foo, Future[Foo, Bar]]` into
    * a `Future[Either[Foo, Bar]]`. This is useful for situtations
    * where the right hand side of an either is asynchronous.
    */
  def flattenFutureE[L, R](
      e: Either[L, Future[Either[L, R]]]): Future[Either[L, R]] = {

    def ifLeft(left: L): Future[Either[L, R]] = Future.successful(Left(left))
    def ifRight(rightF: Future[Either[L, R]]): Future[Either[L, R]] = rightF

    e.fold(ifLeft, ifRight)
  }
}
