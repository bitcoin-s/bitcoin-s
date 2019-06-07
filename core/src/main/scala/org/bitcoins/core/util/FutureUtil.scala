package org.bitcoins.core.util

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object FutureUtil {

  /**
    * Executes a series of futures sequentially
    *
    * @param items The elements we want to transform into futures
    * @param fun A function that transforms each element into a future
    * @return The processed elements
    */
  def sequentially[T, U](items: Seq[T])(fun: T => Future[U])(
      implicit ec: ExecutionContext): Future[List[U]] = {
    val init = Future.successful(List.empty[U])
    items.foldLeft(init) { (f, item) =>
      f.flatMap { x =>
        fun(item).map(_ :: x)
      }
    } map (_.reverse)
  }

  val unit: Future[Unit] = Future.successful(())
}
