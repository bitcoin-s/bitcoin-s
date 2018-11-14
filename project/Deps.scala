import sbt._

object Deps {
  object V {
    val bouncyCastle = "1.55"
    val logback = "1.0.13"
    val scalacheck = "1.14.0"
    val scalaTest = "3.0.5"
    val slf4j = "1.7.5"
    val spray = "1.3.2"
    val zeromq = "0.4.3"
    val akkav = "10.1.5"
    val akkaStreamv = "2.5.17"
    val playv = "2.6.10"
    val scodecV = "1.1.6"
  }

  object Compile {
    val bouncycastle = "org.bouncycastle" % "bcprov-jdk15on" % V.bouncyCastle withSources() withJavadoc()
    val scodec = "org.scodec" %% "scodec-bits" % V.scodecV withSources() withJavadoc()
    val slf4j = "org.slf4j" % "slf4j-api" % V.slf4j % "provided" withSources() withJavadoc()
    val zeromq = "org.zeromq" % "jeromq" % V.zeromq withSources() withJavadoc()
    val akkaHttp = "com.typesafe.akka" %% "akka-http" % V.akkav withSources() withJavadoc()
    val akkaStream = "com.typesafe.akka" %% "akka-stream" % V.akkaStreamv withSources() withJavadoc()
    val playJson = "com.typesafe.play" %% "play-json" % V.playv withSources() withJavadoc()
  }

  object Test {
    val bitcoinj = ("org.bitcoinj" % "bitcoinj-core" % "0.14.4" % "test").exclude("org.slf4j", "slf4j-api")
    val junitInterface = "com.novocode" % "junit-interface" % "0.10" % "test" withSources() withJavadoc()
    val logback = "ch.qos.logback" % "logback-classic" % V.logback % "test" withSources() withJavadoc()
    val scalacheck = "org.scalacheck" %% "scalacheck" % V.scalacheck % "test" withSources() withJavadoc()
    val scalaTest = "org.scalatest" %% "scalatest" % V.scalaTest % "test" withSources() withJavadoc()
    val spray = "io.spray" %% "spray-json" % V.spray  % "test" withSources() withJavadoc()
    val akkaHttp = "com.typesafe.akka" %% "akka-http-testkit" % V.akkav % "test" withSources() withJavadoc()
    val akkaStream = "com.typesafe.akka" %% "akka-stream-testkit" % V.akkaStreamv % "test" withSources() withJavadoc()
  }

  val core = List(
    Compile.bouncycastle,
    Compile.scodec,
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

  val rpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j,
    Test.akkaHttp,
    Test.akkaStream,
    Test.logback,
    Test.scalaTest,
    Test.scalacheck
  )

  val eclairRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j,
    Test.akkaHttp,
    Test.logback,
    Test.scalaTest,
    Test.scalacheck
  )

  val testkit = List(
    Compile.slf4j,
    "org.scalacheck" %% "scalacheck" % V.scalacheck withSources() withJavadoc()
  )
}
