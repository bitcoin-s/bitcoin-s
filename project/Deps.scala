import sbt._

object Deps {
  object V {
    val bouncyCastle = "1.55"
    val logback = "1.0.13"
    val scalacheck = "1.13.4"
    val scalaTest = "3.0.5"
    val slf4j = "1.7.5"
    val spray = "1.3.2"
    val zeromq = "0.4.3"
  }

  object Compile {
    val bouncycastle = "org.bouncycastle" % "bcprov-jdk15on" % V.bouncyCastle
    val slf4j = "org.slf4j" % "slf4j-api" % V.slf4j % "provided"
    val zeromq = "org.zeromq" % "jeromq" % V.zeromq
  }

  object Test {
    val bitcoinj = ("org.bitcoinj" % "bitcoinj-core" % "0.14.4" % "test").exclude("org.slf4j", "slf4j-api")
    val junitInterface = "com.novocode" % "junit-interface" % "0.10" % "test"
    val logback = "ch.qos.logback" % "logback-classic" % V.logback % "test"
    val scalacheck = "org.scalacheck" %% "scalacheck" % V.scalacheck % "test" withSources() withJavadoc()
    val scalaTest = "org.scalatest" %% "scalatest" % V.scalaTest % "test"
    val spray = "io.spray" %% "spray-json" % V.spray  % "test"
  }

  val core = List(
    Compile.bouncycastle,
    Compile.slf4j
  )

  val coreGen = List(
    Compile.slf4j,
    Test.scalacheck
  )

  val coreTest = List(
    Test.bitcoinj,
    Test.junitInterface,
    Test.logback,
    Test.scalaTest,
    Test.spray
  )

  val zmq = List(
    Compile.zeromq,
    Compile.slf4j,
    Test.logback,
    Test.scalacheck,
    Test.scalaTest
  )
}
