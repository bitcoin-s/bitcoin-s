package org.bitcoins.dlc.testgen

import play.api.libs.json._

import java.io.{File, PrintWriter}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success, Try}

trait TestVectorGen[T <: TestVector, Input] {

  def testVectorParser: TestVectorParser[T]

  def inputFromJson: JsValue => JsResult[Input]

  def inputStr: String

  def generateFromInput: Input => Future[T]

  implicit def ec: ExecutionContext = ExecutionContext.global

  def defaultTestFile: File

  private def writeToFile(json: JsValue, outFile: File): Unit = {
    val writer = new PrintWriter(outFile)
    writer.print(Json.prettyPrint(json))
    writer.close()
  }

  def writeTestVectorsToFile(
      vecs: Vector[T],
      file: File = defaultTestFile): Unit = {
    val arr = JsArray(vecs.map(_.toJson))
    writeToFile(arr, file)
  }

  def readFromDefaultTestFile(): JsResult[Vector[T]] = {
    // If we are missing the test file, generate it
    if (!defaultTestFile.canRead) {
      val gen = generateAndWriteTestVectors()
      Await.result(gen, 60.seconds)
    }

    val source = Source.fromFile(defaultTestFile)
    val str = source.getLines().reduce(_ ++ _)
    source.close()

    Json.parse(str).validate[JsArray].flatMap { arr =>
      arr.value
        .foldLeft[JsResult[Vector[T]]](JsSuccess(Vector.empty)) {
          case (jsResultAccum, json) =>
            jsResultAccum.flatMap { accum =>
              testVectorParser.fromJson(json).map { testVec =>
                accum :+ testVec
              }
            }
        }
    }
  }

  def readInputsFromDefaultTestFile(): JsResult[Vector[Input]] = {
    val source = Source.fromFile(defaultTestFile)
    val str = source.getLines().reduce(_ ++ _)
    source.close()

    Json.parse(str).validate[JsArray].flatMap { arr =>
      arr.value
        .foldLeft[JsResult[Vector[Input]]](JsSuccess(Vector.empty)) {
          case (jsResultAccum, json) =>
            jsResultAccum.flatMap { accum =>
              inputFromJson((json \ inputStr).get).map { testVec =>
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
          Future.sequence(inputs.map(input => generateFromInput(input)))
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

  def generateTestVectors(): Future[Vector[T]]

  def generateAndWriteTestVectors(
      file: File = defaultTestFile): Future[Unit] = {
    generateTestVectors().map { testVectors =>
      writeTestVectorsToFile(testVectors, file)
    }
  }
}
