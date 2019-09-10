import sbt._

object Deps {

  object V {
    val bouncyCastle = "1.55"
    val logback = "1.2.3"
    val scalacheck = "1.14.0"
    val scalaTest = "3.0.8"
    val slf4j = "1.7.28"
    val spray = "1.3.5"
    val zeromq = "0.5.1"
    val akkav = "10.1.9"
    val akkaStreamv = "2.5.25"
    val playv = "2.7.4"
    val scodecV = "1.1.12"
    val junitV = "0.11"
    val nativeLoaderV = "2.3.4"
    val typesafeConfigV = "1.3.4"

    // async dropped Scala 2.11 in 0.10.0
    val asyncOldScalaV = "0.9.7"
    val asyncNewScalaV = "0.10.0"

    val postgresV = "9.4.1210"
    val akkaActorV = akkaStreamv
    val slickV = "3.3.2"
    val sqliteV = "3.28.0"
    val scalameterV = "0.17"

    // Wallet/node/chain server deps
    val oldMicroPickleV = "0.7.4"
    val oldMicroJsonV = oldMicroPickleV

    val newMicroPickleV = "0.7.5"
    val newMicroJsonV = newMicroPickleV

    // akka-http-upickle is not yet published
    // to Maven central. There's a PR for adding
    // suport, https://github.com/hseeberger/akka-http-json/pull/314.
    // Until that's merged, you'll have to pull down
    // that PR, do `sbt publishLocal` and replace the
    // value here with whatever is in there. This
    // obviously has to be changed before this is
    // merged.

    val sourcecodeV = "0.1.7"

    // CLI deps
    val scoptV = "4.0.0-RC2"
    val sttpV = "1.6.6"
  }

  object Compile {

    val bouncycastle = "org.bouncycastle" % "bcprov-jdk15on" % V.bouncyCastle withSources () withJavadoc ()
    val scodec = "org.scodec" %% "scodec-bits" % V.scodecV withSources () withJavadoc ()
    val slf4j = "org.slf4j" % "slf4j-api" % V.slf4j % "provided" withSources () withJavadoc ()
    val zeromq = "org.zeromq" % "jeromq" % V.zeromq withSources () withJavadoc ()
    val akkaHttp = "com.typesafe.akka" %% "akka-http" % V.akkav withSources () withJavadoc ()
    val akkaStream = "com.typesafe.akka" %% "akka-stream" % V.akkaStreamv withSources () withJavadoc ()
    val akkaActor = "com.typesafe.akka" %% "akka-actor" % V.akkaStreamv withSources () withJavadoc ()

    val playJson = "com.typesafe.play" %% "play-json" % V.playv withSources () withJavadoc ()
    val typesafeConfig = "com.typesafe" % "config" % V.typesafeConfigV withSources () withJavadoc ()

    val logback = "ch.qos.logback" % "logback-classic" % V.logback withSources () withJavadoc ()

    //for loading secp256k1 natively
    val nativeLoader = "org.scijava" % "native-lib-loader" % V.nativeLoaderV withSources () withJavadoc ()

    //node deps
    val slick = "com.typesafe.slick" %% "slick" % V.slickV withSources () withJavadoc ()
    val slickHikari = "com.typesafe.slick" %% "slick-hikaricp" % V.slickV
    val sqlite = "org.xerial" % "sqlite-jdbc" % V.sqliteV
    val postgres = "org.postgresql" % "postgresql" % V.postgresV

    // zero dep JSON library. Have to use different versiont to juggle
    // Scala 2.11/12/13
    val oldMicroJson = "com.lihaoyi" %% "ujson" % V.oldMicroJsonV
    val newMicroJson = "com.lihaoyi" %% "ujson" % V.newMicroJsonV

    // serializing to and from JSON Have to use different versiont to juggle
    // Scala 2.11/12/13
    val oldMicroPickle = "com.lihaoyi" %% "upickle" % V.oldMicroPickleV
    val newMicroPickle = "com.lihaoyi" %% "upickle" % V.newMicroPickleV

    // get access to reflection data at compile-time
    val sourcecode = "com.lihaoyi" %% "sourcecode" % V.sourcecodeV

    // parsing of CLI opts and args
    val scopt = "com.github.scopt" %% "scopt" % V.scoptV

    // HTTP client lib
    val sttp = "com.softwaremill.sttp" %% "core" % V.sttpV

    val scalacheck = "org.scalacheck" %% "scalacheck" % V.scalacheck withSources () withJavadoc ()
    val scalaTest = "org.scalatest" %% "scalatest" % V.scalaTest withSources () withJavadoc ()
  }

  object Test {
    val oldAsync = "org.scala-lang.modules" %% "scala-async" % V.asyncOldScalaV % "test" withSources () withJavadoc ()
    val newAsync = "org.scala-lang.modules" %% "scala-async" % V.asyncNewScalaV % "test" withSources () withJavadoc ()
    val junitInterface = "com.novocode" % "junit-interface" % V.junitV % "test" withSources () withJavadoc ()
    val logback = Compile.logback % "test"
    val scalacheck = Compile.scalacheck % "test"
    val scalaTest = Compile.scalaTest % "test"
    val spray = "io.spray" %% "spray-json" % V.spray % "test" withSources () withJavadoc ()
    val akkaHttp = "com.typesafe.akka" %% "akka-http-testkit" % V.akkav % "test" withSources () withJavadoc ()
    val akkaStream = "com.typesafe.akka" %% "akka-stream-testkit" % V.akkaStreamv % "test" withSources () withJavadoc ()
    val playJson = Compile.playJson % "test"
    val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % V.akkaActorV withSources () withJavadoc ()
    val scalameter = "com.storm-enroute" %% "scalameter" % V.scalameterV % "test" withSources () withJavadoc ()
  }

  val chain = List(
    Compile.logback
  )

  val chainTest = List()

  val core = List(
    Compile.bouncycastle,
    Compile.scodec,
    Compile.slf4j
  )

  val secp256k1jni = List(
    Compile.nativeLoader,
    Test.junitInterface
  )

  val coreTest = List(
    Test.junitInterface,
    Test.logback,
    Test.scalaTest,
    Test.spray,
    Test.playJson
  )

  val bitcoindZmq = List(
    Compile.zeromq,
    Compile.slf4j,
    Test.logback,
    Test.scalacheck,
    Test.scalaTest
  )

  val bitcoindRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j,
    Compile.typesafeConfig
  )

  def bitcoindRpcTest(scalaVersion: String) = List(
    Test.akkaHttp,
    Test.akkaStream,
    Test.logback,
    Test.scalaTest,
    Test.scalacheck,
    if (scalaVersion.startsWith("2.11")) Test.oldAsync else Test.newAsync
  )

  val bench = List(
    "org.slf4j" % "slf4j-api" % V.slf4j withSources () withJavadoc (),
    Compile.logback
  )

  val dbCommons = List(
    Compile.slick,
    Compile.sourcecode,
    Compile.logback,
    Compile.sqlite,
    Compile.slickHikari
  )

  def cli(scalaVersion: String) = List(
    Compile.sttp,
    if (scalaVersion.startsWith("2.11")) Compile.oldMicroPickle
    else Compile.newMicroPickle,
    Compile.logback,
    Compile.scopt
  )

  def picklers(scalaVersion: String) = List(
    if (scalaVersion.startsWith("2.11")) Compile.oldMicroPickle
    else Compile.newMicroPickle
  )

  def server(scalaVersion: String) = List(
    if (scalaVersion.startsWith("2.11")) Compile.oldMicroPickle
    else Compile.newMicroPickle,
    Compile.logback,
    Compile.akkaHttp
  )

  val eclairRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j
  )

  val eclairRpcTest = List(
    Test.akkaHttp,
    Test.akkaStream,
    Test.logback,
    Test.scalaTest,
    Test.scalacheck
  )

  val node = List(
    Compile.akkaActor,
    Compile.logback,
    Compile.slick,
    Compile.slickHikari,
    Compile.sqlite
  )

  val nodeTest = List(
    Test.akkaTestkit,
    Test.scalaTest
  )

  val testkit = List(
    Compile.slf4j,
    Compile.scalacheck,
    Compile.scalaTest,
    Test.akkaTestkit
  )

  def wallet(scalaVersion: String) = List(
    if (scalaVersion.startsWith("2.11")) Compile.oldMicroJson
    else Compile.newMicroJson,
    Compile.logback
  )

  val walletTest = List(
    Test.akkaTestkit
  )

  val docs = List(
    Compile.logback,
    Test.scalaTest,
    Test.logback
  )
}
