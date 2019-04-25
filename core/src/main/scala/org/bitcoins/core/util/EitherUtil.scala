package org.bitcoins.core.util

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Success, Failure}

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
      either: Either[L, Future[Either[L, R]]]
  ): Future[Either[L, R]] = {

    def ifLeft(left: L): Future[Either[L, R]] = Future.successful(Left(left))
    def ifRight(rightF: Future[Either[L, R]]): Future[Either[L, R]] = rightF

    either.fold(ifLeft, ifRight)
  }

  /** $liftBiasedFut */
  def liftRightBiasedFutureE[L, R](
      either: Either[L, Future[R]]
  )(implicit ec: ExecutionContext): Future[Either[L, R]] =
    either match {
      case Right(fut) => fut.map(Right(_))
      case Left(l)    => Future.successful(Left(l))
    }

  /** $liftBiasedFut */
  def listLeftBiasedFutureE[L, R](
      either: Either[Future[L], R]
  )(implicit ec: ExecutionContext): Future[Either[L, R]] =
    either match {
      case Left(fut) => fut.map(Left(_))
      case Right(l)  => Future.successful(Right(l))
    }

  object EitherOps {
    import scala.language.implicitConversions
    implicit def either2EnhancedEither[A, B](
        either: Either[A, B]
    ): EnchancedEither[A, B] = EnchancedEither(either)

    implicit def enchancedEither2Either[A, B](
        enhanced: EnchancedEither[A, B]): Either[A, B] = enhanced.underlying
  }

  /** The methods here are copied directly from the 2.12 stdlib */
  case class EnchancedEither[A, B](
      private[EitherUtil] val underlying: Either[A, B]) {

    /** The given function is applied if this is a `Right`.
      *
      *  {{{
      *  Right(12).map(x => "flower") // Result: Right("flower")
      *  Left(12).map(x => "flower")  // Result: Left(12)
      *  }}}
      */
    def map[B1](f: B => B1): EnchancedEither[A, B1] = underlying match {
      case Right(b) => EnchancedEither(Right(f(b)))
      case _        => EnchancedEither(this.asInstanceOf[Either[A, B1]])
    }

    /** Binds the given function across `Right`.
      *
      *  @param f The function to bind across `Right`.
      */
    def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): EnchancedEither[A1, B1] =
      underlying match {
        case Right(b) => EnchancedEither(f(b))
        case _        => EnchancedEither(underlying.asInstanceOf[Either[A1, B1]])
      }

    def toTry(implicit ev: A <:< Throwable): Try[B] = underlying match {
      case Right(b) => Success(b)
      case Left(a)  => Failure(a)
    }
  }

}
