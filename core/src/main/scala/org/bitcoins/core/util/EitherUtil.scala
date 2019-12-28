package org.bitcoins.core.util

import org.bitcoins.core.compat.{CompatEither, CompatLeft, CompatRight}

import scala.concurrent.{ExecutionContext, Future}

/**
  * @define liftBiasedFut Given a [[scala.Either Either]] that contains a
  *                       [[scala.concurrent.Future Future[L | R] ]] only on one side,
  *                       transforms it into a future [[scala.Either Either[L, R] ]]
  */
object EitherUtil {

  /**
    * Flattens a nested `Either[Foo, Future[Foo, Bar]]` into
    * a `Future[Either[Foo, Bar]]`. This is EitherUtiluseful for situtations
    * where the right hand side of an either is asynchronous.
    */
  def flattenFutureE[L, R](
      either: CompatEither[L, Future[CompatEither[L, R]]]
  ): Future[CompatEither[L, R]] = {

    def ifLeft(left: L): Future[CompatEither[L, R]] =
      Future.successful(CompatLeft(left))
    def ifRight(
        rightF: Future[CompatEither[L, R]]): Future[CompatEither[L, R]] =
      rightF

    either.fold(ifLeft, ifRight)
  }

  /** $liftBiasedFut */
  def liftRightBiasedFutureE[L, R](
      either: CompatEither[L, Future[R]]
  )(implicit ec: ExecutionContext): Future[CompatEither[L, R]] =
    either match {
      case CompatRight(fut) => fut.map(elem => CompatRight(elem))
      case CompatLeft(l)    => Future.successful(CompatLeft(l))
    }

  /** $liftBiasedFut */
  def listLeftBiasedFutureE[L, R](
      either: Either[Future[L], R]
  )(implicit ec: ExecutionContext): Future[Either[L, R]] =
    either match {
      case Left(fut) => fut.map(Left(_))
      case Right(l)  => Future.successful(Right(l))
    }

}
