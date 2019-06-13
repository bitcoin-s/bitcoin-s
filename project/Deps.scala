import sbt._

object Deps {

  object V {
    val bouncyCastle = "1.55"
    val logback = "1.2.3"
    val scalacheck = "1.14.0"
    val scalaTest = "3.0.5"
    val slf4j = "1.7.26"
    val spray = "1.3.4"
    val zeromq = "0.5.1"
    val akkav = "10.1.8"
    val akkaStreamv = "2.5.23"
    val playv = "2.7.2"
    val scodecV = "1.1.6"
    val junitV = "0.11"
    val nativeLoaderV = "2.3.2"
    val typesafeConfigV = "1.3.4"
    val ammoniteV = "1.6.7"

    val asyncV = "0.9.7"
    val jodaV = "2.10.2"
    val postgresV = "9.4.1210"
    val akkaActorV = akkaStreamv
    val slickV = "3.3.1"
    val sqliteV = "3.27.2.1"
    val uJsonV = "0.7.1"
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
    val ammonite = "com.lihaoyi" %% "ammonite" % V.ammoniteV cross CrossVersion.full

    //node deps
    val joda = "joda-time" % "joda-time" % V.jodaV

    val slick = "com.typesafe.slick" %% "slick" % V.slickV withSources () withJavadoc ()
    val slickHikari = "com.typesafe.slick" %% "slick-hikaricp" % V.slickV
    val sqlite = "org.xerial" % "sqlite-jdbc" % V.sqliteV
    val postgres = "org.postgresql" % "postgresql" % V.postgresV
    val uJson = "com.lihaoyi" %% "ujson" % V.uJsonV

    val scalacheck = "org.scalacheck" %% "scalacheck" % V.scalacheck withSources () withJavadoc ()
    val scalaTest = "org.scalatest" %% "scalatest" % V.scalaTest withSources () withJavadoc ()
  }

  object Test {
    val async = "org.scala-lang.modules" %% "scala-async" % V.asyncV % "test" withSources () withJavadoc ()

    val bitcoinj = ("org.bitcoinj" % "bitcoinj-core" % "0.14.4" % "test")
      .exclude("org.slf4j", "slf4j-api")
    val junitInterface = "com.novocode" % "junit-interface" % V.junitV % "test" withSources () withJavadoc ()
    val logback = Compile.logback % "test"
    val scalacheck = Compile.scalacheck % "test"
    val scalaTest = Compile.scalaTest % "test"
    val spray = "io.spray" %% "spray-json" % V.spray % "test" withSources () withJavadoc ()
    val akkaHttp = "com.typesafe.akka" %% "akka-http-testkit" % V.akkav % "test" withSources () withJavadoc ()
    val akkaStream = "com.typesafe.akka" %% "akka-stream-testkit" % V.akkaStreamv % "test" withSources () withJavadoc ()
    val ammonite = Compile.ammonite % "test"
    val playJson = Compile.playJson % "test"
    val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % V.akkaActorV withSources () withJavadoc ()
  }

  val root = List(
    Test.ammonite
  )

  val chain = List(
    Compile.slf4j,
    Test.ammonite
  )

  val chainTest = List(
    Test.ammonite,
    Test.logback
  )

  val core = List(
    Compile.bouncycastle,
    Compile.scodec,
    Compile.slf4j,
    Test.ammonite
  )

  val secp256k1jni = List(
    Compile.nativeLoader,
    Test.junitInterface,
    Test.ammonite
  )

  val coreTest = List(
    Test.bitcoinj,
    Test.junitInterface,
    Test.logback,
    Test.scalaTest,
    Test.spray,
    Test.ammonite,
    Test.playJson
  )

  val bitcoindZmq = List(
    Compile.zeromq,
    Compile.slf4j,
    Test.logback,
    Test.scalacheck,
    Test.scalaTest,
    Test.ammonite
  )

  val bitcoindRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j,
    Compile.typesafeConfig,
    Test.ammonite
  )

  val bitcoindRpcTest = List(
    Test.akkaHttp,
    Test.akkaStream,
    Test.logback,
    Test.scalaTest,
    Test.scalacheck,
    Test.async,
    Test.ammonite
  )

  val bench = List(
    "org.slf4j" % "slf4j-api" % V.slf4j withSources () withJavadoc (),
    Compile.logback,
    Test.ammonite
  )

  val dbCommons = List(
    Compile.slick,
    Compile.sqlite,
    Compile.slickHikari,
    Test.ammonite
  )

  val eclairRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j,
    Test.ammonite
  )

  val eclairRpcTest = List(
    Test.akkaHttp,
    Test.akkaStream,
    Test.logback,
    Test.scalaTest,
    Test.scalacheck,
    Test.ammonite
  )

  val node = List(
    Compile.akkaActor,
    Compile.logback,
    Compile.joda,
    Compile.slick,
    Compile.slickHikari,
    Compile.sqlite,
    Test.ammonite
  )

  val nodeTest = List(
    Test.akkaTestkit,
    Test.logback,
    Test.scalaTest,
    Test.ammonite
  )

  val testkit = List(
    Compile.slf4j,
    Compile.scalacheck,
    Compile.scalaTest,
    Test.akkaTestkit,
    Test.ammonite
  )

  val scripts = List(
    Compile.ammonite,
    Compile.logback
  )

  val wallet = List(
    Test.ammonite,
    Compile.uJson
  )

  val walletTest = List(
    Test.logback,
    Test.akkaTestkit,
    Test.ammonite
  )

  val docs = List(
    Compile.ammonite,
    Compile.logback,
    Test.scalaTest,
    Test.logback
  )
}
