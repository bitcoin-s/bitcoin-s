package org.bitcoins.core.util

import scala.concurrent.{ExecutionContext, Future}

/**
  * @define liftBiasedFut Given a [[scala.Either Either]] that contains a
  *                       [[scala.concurrent.Future Future[L | R] ]] only on one side,
  *                       transforms it into a future [[scala.Either Either[L, R] ]]
  */
object EitherUtil {

  /**
    * Flattens a nested `Either[Foo, Future[Foo, Bar]]` into
    * a `Future[Either[Foo, Bar]]`. This is useful for situtations
    * where the right hand side of an either is asynchronous.
    */
  def flattenFutureE[L, R](
      either: Either[L, Future[Either[L, R]]]): Future[Either[L, R]] = {

    def ifLeft(left: L): Future[Either[L, R]] = Future.successful(Left(left))
    def ifRight(rightF: Future[Either[L, R]]): Future[Either[L, R]] = rightF

    either.fold(ifLeft, ifRight)
  }

  /** $liftBiasedFut */
  def liftRightBiasedFutureE[L, R](either: Either[L, Future[R]])(
      implicit ec: ExecutionContext): Future[Either[L, R]] =
    either match {
      case Right(fut) => fut.map(Right(_))
      case Left(l)    => Future.successful(Left(l))
    }

  /** $liftBiasedFut */
  def listLeftBiasedFutureE[L, R](either: Either[Future[L], R])(
      implicit ec: ExecutionContext): Future[Either[L, R]] =
    either match {
      case Left(fut) => fut.map(Left(_))
      case Right(l)  => Future.successful(Right(l))
    }

}
