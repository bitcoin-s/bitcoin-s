package org.bitcoins.core.util.testprotocol

import spray.json._

import scala.annotation.tailrec

/**
  * Created by tom on 6/20/16.
  */
object Base58InvalidTestCaseProtocol extends DefaultJsonProtocol {

  implicit object Base58InvalidTestCaseFormatter
      extends RootJsonFormat[Seq[Base58InvalidTestCase]] {

    override def read(value: JsValue): Seq[Base58InvalidTestCase] = {
      val jsArray: JsArray = value match {
        case array: JsArray => array
        case _: JsValue =>
          throw new RuntimeException("TestCase must be in format of jsArray")
      }
      val elements: Vector[JsValue] = jsArray.elements
      parseInvalidTestCases(elements)
    }
    override def write(testCase: Seq[Base58InvalidTestCase]): JsValue = ???
  }

  /**
    * Function to parse the JSON values into a Seq[Base58InvalidTestCase]
    * @param elements
    * @return
    */
  private def parseInvalidTestCases(
      elements: Seq[JsValue]): Seq[Base58InvalidTestCase] = {
    @tailrec
    def loop(
        remainingElements: List[JsValue],
        accum: List[Base58InvalidTestCase]): Seq[Base58InvalidTestCase] = {
      remainingElements match {
        case h :: t =>
          val base58TestCase = elementToBase58TestCase(h)
          loop(t, base58TestCase :: accum)
        case Nil => accum.reverse
      }
    }
    loop(elements.toList, List())
  }

  /**
    * Helper function to parser
    * @param jsValue
    * @return
    */
  def elementToBase58TestCase(jsValue: JsValue): Base58InvalidTestCase =
    jsValue match {
      case array: JsArray =>
        val str = array.elements.head
        Base58InvalidTestCaseImpl(str.convertTo[String])
      case error: JsValue =>
        throw new RuntimeException("Expected array. Got: " + error)
    }
}
