package org.bitcoins.core.util.testprotocol

import org.bitcoins.core.util.BitcoinSLogger
import spray.json._

import scala.annotation.tailrec

/**
  * Created by tom on 6/20/16.
  */
object Base58InvalidTestCaseProtocol extends DefaultJsonProtocol {
  implicit object Base58InvalidTestCaseFormatter extends RootJsonFormat[Seq[Base58InvalidTestCase]] {
    override def read(value: JsValue) : Seq[Base58InvalidTestCase] = {
      val jsArray : JsArray = value match {
        case array : JsArray => array
        case _ : JsValue => throw new RuntimeException("TestCase must be in format of jsArray")
      }
      val elements : Vector[JsValue] = jsArray.elements
      parser(elements)
    }
    override def write (testCase : Seq[Base58InvalidTestCase]) : JsValue = ???
  }

  /**
    * Function to parse the JSON values into a Seq[Base58InvalidTestCase]
    * @param elements
    * @return
    */
  def parser(elements : Seq[JsValue]) : Seq[Base58InvalidTestCase] = {
    @tailrec
    def loop(remainingElements : List[JsValue], accum: List[Base58InvalidTestCase]) : Seq[Base58InvalidTestCase] = {
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
  def elementToBase58TestCase (jsValue : JsValue) : Base58InvalidTestCase =  jsValue match {
    case array : JsArray =>
      val str = array.elements.head
      Base58InvalidTestCaseImpl(str.convertTo[String])
    case error : JsValue => throw new RuntimeException("Expected array. Got: " + error)
  }
}
/*
  def elementToBase58TestCase (jsValue : Option[JsValue]) : Base58InvalidTestCase =  jsValue match {
    case Some(str : JsString) =>
      val base58TestCase = Base58InvalidTestCaseImpl(str.convertTo[String])
      base58TestCase
    case None => Base58InvalidTestCaseImpl("")
    case error : JsValue => throw new RuntimeException("Expected array. Got: " + error)
  }


    def elementToBase58TestCase (jsValue : JsValue) : Base58InvalidTestCase =  jsValue match {
    case str : JsString =>
      val base58TestCase = Base58InvalidTestCaseImpl(str.convertTo[String])
      base58TestCase
    case error : JsValue => throw new RuntimeException("Expected array. Got: " + error)
 */