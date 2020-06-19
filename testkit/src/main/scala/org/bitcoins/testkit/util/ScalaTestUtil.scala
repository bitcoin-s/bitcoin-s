package org.bitcoins.testkit.util

import org.scalatest.{Assertion, Assertions}

import scala.concurrent.{ExecutionContext, Future}

/** Helper methods for working with the scalatest testing framewrok
  * @see [[http://www.scalatest.org/user_guide/using_assertions scalatest documentation]]
  */
object ScalaTestUtil {

  def toAssertF(vecFut: Vector[Future[Assertion]])(implicit
      ec: ExecutionContext): Future[Assertion] = {
    val futVec = Future.sequence(vecFut)
    futVec.map(_.foldLeft(Assertions.succeed) {
      case (_, next) =>
        next
    })
  }

}
