package org.bitcoins.dlc.testgen

import java.io.{File, PrintWriter}

import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success, Try}

object DLCTestVectorGen {
  import DLCTxGen._

  implicit private val ec: ExecutionContext = ExecutionContext.global

  val defaultTestFile: File = new File(
    "dlc/src/main/scala/org/bitcoins/dlc/testgen/dlc_test.json")

  private def writeToFile(json: JsValue, outFile: File): Unit = {
    val writer = new PrintWriter(outFile)
    writer.print(Json.prettyPrint(json))
    writer.close()
  }

  def writeTestVectorsToFile(
      vecs: Vector[DLCTestVector],
      file: File = defaultTestFile): Unit = {
    val arr = JsArray(vecs.map(_.toJson))
    writeToFile(arr, file)
  }

  def readFromDefaultTestFile(): JsResult[Vector[DLCTestVector]] = {
    val source = Source.fromFile(defaultTestFile)
    val str = source.getLines().reduce(_ ++ _)
    source.close()

    Json.parse(str).validate[JsArray].flatMap { arr =>
      arr.value
        .foldLeft[JsResult[Vector[DLCTestVector]]](JsSuccess(Vector.empty)) {
          case (jsResultAccum, json) =>
            jsResultAccum.flatMap { accum =>
              DLCTestVector.fromJson(json).map { testVec =>
                accum :+ testVec
              }
            }
        }
    }
  }

  def readInputsFromDefaultTestFile(): JsResult[Vector[ValidTestInputs]] = {
    val source = Source.fromFile(defaultTestFile)
    val str = source.getLines().reduce(_ ++ _)
    source.close()

    Json.parse(str).validate[JsArray].flatMap { arr =>
      arr.value
        .foldLeft[JsResult[Vector[ValidTestInputs]]](JsSuccess(Vector.empty)) {
          case (jsResultAccum, json) =>
            jsResultAccum.flatMap { accum =>
              ValidTestInputs.fromJson((json \ "testInputs").get).map {
                testVec =>
                  accum :+ testVec
              }
            }
        }
    }
  }

  /** Returns true if anything has changed, false otherwise */
  def regenerateTestFile(): Future[Boolean] = {
    val testVecInputs = readInputsFromDefaultTestFile()
    val testVecResultT = Try(readFromDefaultTestFile())

    testVecInputs match {
      case JsSuccess(inputs, _) =>
        val newTestVecsF =
          Future.sequence(inputs.map(input => successTestVector(input)))
        newTestVecsF.flatMap { newTestVecs =>
          val noChange = testVecResultT match {
            case Failure(_) | Success(JsError(_)) => false
            case Success(JsSuccess(testVecs, _)) =>
              newTestVecs.zip(testVecs).foldLeft(true) {
                case (sameSoFar, (oldVec, newVec)) =>
                  sameSoFar && (oldVec == newVec)
              }
          }

          if (noChange) {
            Future.successful(false)
          } else {
            val successfulDelete = defaultTestFile.delete()
            if (successfulDelete) {
              writeTestVectorsToFile(newTestVecs)
              Future.successful(true)
            } else {
              Future.failed(
                new RuntimeException(
                  s"Was unable to delete ${defaultTestFile.getAbsolutePath}"))
            }
          }
        }
      case JsError(err) =>
        Future.failed(
          new IllegalArgumentException(s"Could not read json from file: $err"))
    }
  }

  def generateTestVectors(): Future[Vector[DLCTestVector]] = {
    // Happy Path
    Future.sequence(Vector(2, 3, 5, 8, 100).map(randomSuccessTestVector))
  }
}
