import sbt._

object Deps {

  object V {
    val bouncyCastle = "1.65.01"
    val logback = "1.2.3"
    val scalacheck = "1.14.3"
    val scalaTest = "3.2.0"

    val scalaTestPlus =
      "3.2.0.0" //super annoying... https://oss.sonatype.org/content/groups/public/org/scalatestplus/
    val slf4j = "1.7.30"
    val spray = "1.3.5"
    val zeromq = "0.5.2"
    val akkav = "10.1.12"
    val playv = "2.9.0"
    val akkaStreamv = "2.6.6"
    val scodecV = "1.1.17"
    val junitV = "0.11"
    val nativeLoaderV = "2.3.4"
    val typesafeConfigV = "1.4.0"

    val scalaFxV = "14-R19"
    val javaFxV = "14.0.1"

    val asyncNewScalaV = "0.10.0"

    val flywayV = "6.4.2"
    val postgresV = "42.2.14"
    val akkaActorV = akkaStreamv
    val slickV = "3.3.2"
    val sqliteV = "3.32.3"
    val scalameterV = "0.17"
    val scalamockV = "4.4.0"
    val pgEmbeddedV = "0.13.3"

    val newMicroPickleV = "0.8.0"
    val newMicroJsonV = newMicroPickleV

    // akka-http-upickle is not yet published
    // to Maven central. There's a PR for adding
    // suport, https://github.com/hseeberger/akka-http-json/pull/314.
    // Until that's merged, you'll have to pull down
    // that PR, do `sbt publishLocal` and replace the
    // value here with whatever is in there. This
    // obviously has to be changed before this is
    // merged.

    val sourcecodeV = "0.2.1"

    // CLI deps
    val scoptV = "4.0.0-RC2"
    val sttpV = "1.7.2"
    val codehausV = "3.1.2"
  }

  object Compile {

    val bouncycastle =
      "org.bouncycastle" % "bcprov-jdk15on" % V.bouncyCastle withSources () withJavadoc ()

    val scodec =
      "org.scodec" %% "scodec-bits" % V.scodecV withSources () withJavadoc ()

    val slf4j =
      "org.slf4j" % "slf4j-api" % V.slf4j % "provided" withSources () withJavadoc ()

    val zeromq =
      "org.zeromq" % "jeromq" % V.zeromq withSources () withJavadoc ()

    val akkaHttp =
      "com.typesafe.akka" %% "akka-http" % V.akkav withSources () withJavadoc ()

    val akkaStream =
      "com.typesafe.akka" %% "akka-stream" % V.akkaStreamv withSources () withJavadoc ()

    val akkaActor =
      "com.typesafe.akka" %% "akka-actor" % V.akkaStreamv withSources () withJavadoc ()

    val scalaFx =
      "org.scalafx" %% "scalafx" % V.scalaFxV withSources () withJavadoc ()

    lazy val osName = System.getProperty("os.name") match {
      case n if n.startsWith("Linux")   => "linux"
      case n if n.startsWith("Mac")     => "mac"
      case n if n.startsWith("Windows") => "win"
      case _                            => throw new Exception("Unknown platform!")
    }

    // Not sure if all of these are needed, some might be possible to remove
    lazy val javaFxBase =
      "org.openjfx" % s"javafx-base" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxControls =
      "org.openjfx" % s"javafx-controls" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxFxml =
      "org.openjfx" % s"javafx-fxml" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxGraphics =
      "org.openjfx" % s"javafx-graphics" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxMedia =
      "org.openjfx" % s"javafx-media" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxSwing =
      "org.openjfx" % s"javafx-swing" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxWeb =
      "org.openjfx" % s"javafx-web" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxDeps = List(javaFxBase,
                               javaFxControls,
                               javaFxFxml,
                               javaFxGraphics,
                               javaFxMedia,
                               javaFxSwing,
                               javaFxWeb)

    val playJson =
      "com.typesafe.play" %% "play-json" % V.playv withSources () withJavadoc ()

    val typesafeConfig =
      "com.typesafe" % "config" % V.typesafeConfigV withSources () withJavadoc ()

    val logback =
      "ch.qos.logback" % "logback-classic" % V.logback withSources () withJavadoc ()
    val codehaus = "org.codehaus.janino" % "janino" % V.codehausV

    //for loading secp256k1 natively
    val nativeLoader =
      "org.scijava" % "native-lib-loader" % V.nativeLoaderV withSources () withJavadoc ()

    //node deps
    val slick =
      "com.typesafe.slick" %% "slick" % V.slickV withSources () withJavadoc ()
    val slickHikari = "com.typesafe.slick" %% "slick-hikaricp" % V.slickV
    val sqlite = "org.xerial" % "sqlite-jdbc" % V.sqliteV
    val postgres = "org.postgresql" % "postgresql" % V.postgresV
    val flyway = "org.flywaydb" % "flyway-core" % V.flywayV

    val newMicroJson = "com.lihaoyi" %% "ujson" % V.newMicroJsonV

    val newMicroPickle = "com.lihaoyi" %% "upickle" % V.newMicroPickleV

    // get access to reflection data at compile-time
    val sourcecode = "com.lihaoyi" %% "sourcecode" % V.sourcecodeV

    // parsing of CLI opts and args
    val scopt = "com.github.scopt" %% "scopt" % V.scoptV

    // HTTP client lib
    val sttp = "com.softwaremill.sttp" %% "core" % V.sttpV

    val scalacheck =
      "org.scalacheck" %% "scalacheck" % V.scalacheck withSources () withJavadoc ()

    val scalaTest =
      "org.scalatest" %% "scalatest" % V.scalaTest withSources () withJavadoc ()

    val scalaTestPlus =
      "org.scalatestplus" %% "scalacheck-1-14" % V.scalaTestPlus withSources () withJavadoc ()

    val pgEmbedded =
      "com.opentable.components" % "otj-pg-embedded" % V.pgEmbeddedV withSources () withJavadoc ()
  }

  object Test {

    val newAsync =
      "org.scala-lang.modules" %% "scala-async" % V.asyncNewScalaV % "test" withSources () withJavadoc ()

    val junitInterface =
      "com.novocode" % "junit-interface" % V.junitV % "test" withSources () withJavadoc ()
    val logback = Compile.logback % "test"
    val scalacheck = Compile.scalacheck % "test"
    val scalaTest = Compile.scalaTest % "test"
    val scalaMock = "org.scalamock" %% "scalamock" % V.scalamockV

    val spray =
      "io.spray" %% "spray-json" % V.spray % "test" withSources () withJavadoc ()

    val akkaHttp =
      "com.typesafe.akka" %% "akka-http-testkit" % V.akkav % "test" withSources () withJavadoc ()

    val akkaStream =
      "com.typesafe.akka" %% "akka-stream-testkit" % V.akkaStreamv % "test" withSources () withJavadoc ()
    val playJson = Compile.playJson % "test"

    val akkaTestkit =
      "com.typesafe.akka" %% "akka-testkit" % V.akkaActorV withSources () withJavadoc ()

    val scalameter =
      "com.storm-enroute" %% "scalameter" % V.scalameterV % "test" withSources () withJavadoc ()

    val pgEmbedded =
      "com.opentable.components" % "otj-pg-embedded" % V.pgEmbeddedV % "test" withSources () withJavadoc ()
  }

  val chain = List(
    Compile.logback
  )

  val chainTest = List(
    Test.pgEmbedded
  )

  def appCommons(scalaVersion: String) =
    List(
      Compile.newMicroPickle,
      Compile.playJson,
      Compile.slf4j
    )

  val core = List(
    Compile.bouncycastle,
    Compile.scodec,
    Compile.slf4j
  )

  val crypto = List(
    Compile.bouncycastle,
    Compile.scodec
  )

  // version number needed for MicroJson
  val dlc = List(
    Compile.playJson,
    Compile.newMicroJson
  )

  val dlcTest = List()

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

  val cryptoTest = List(
    Test.scalaTest
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
    Compile.typesafeConfig
  )

  def bitcoindRpcTest(scalaVersion: String) =
    List(
      Test.akkaHttp,
      Test.akkaStream,
      Test.logback,
      Test.scalaTest,
      Test.scalacheck,
      Test.newAsync
    )

  val bench = List(
    "org.slf4j" % "slf4j-api" % V.slf4j withSources () withJavadoc (),
    Compile.logback
  )

  val dbCommons = List(
    Compile.flyway,
    Compile.slick,
    Compile.sourcecode,
    Compile.logback,
    Compile.sqlite,
    Compile.postgres,
    Compile.slickHikari,
    Test.scalaTest,
    Test.pgEmbedded
  )

  def cli(scalaVersion: String) =
    List(
      Compile.sttp,
      Compile.newMicroPickle,
      Compile.logback,
      Compile.scopt,
      //we can remove this dependency when this is fixed
      //https://github.com/oracle/graal/issues/1943
      //see https://github.com/bitcoin-s/bitcoin-s/issues/1100
      Compile.codehaus
    )

  val gui = List(Compile.scalaFx) ++ Compile.javaFxDeps

  def server(scalaVersion: String) =
    List(
      Compile.newMicroPickle,
      Compile.logback,
      Compile.akkaActor,
      Compile.akkaHttp
    )

  val dlcSuredbitsClient = List(
    Compile.akkaStream,
    Compile.akkaHttp,
    Compile.playJson
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

  val feeProvider = List(
    Compile.akkaHttp,
    Compile.akkaActor,
    Compile.akkaStream
  )

  val feeProviderTest = List(
    Test.akkaTestkit,
    Test.scalaTest
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
    Test.scalaTest,
    Test.pgEmbedded
  )

  val testkit = List(
    Compile.slf4j,
    Compile.scalacheck,
    Compile.scalaTest,
    Compile.scalaTestPlus,
    Compile.pgEmbedded,
    Test.akkaTestkit
  )

  def keyManager(scalaVersion: String) =
    List(
      Compile.newMicroJson
    )

  val keyManagerTest = List(
    Compile.slf4j,
    Test.logback
  )

  def wallet(scalaVersion: String) =
    List(
      Compile.newMicroJson,
      Compile.logback
    )

  val walletTest = List(
    Test.akkaTestkit,
    Test.pgEmbedded
  )

  val docs = List(
    Compile.logback,
    Test.scalaTest,
    Test.logback
  )

  val walletServerTest = List(
    Test.scalaMock,
    Test.akkaHttp,
    Test.akkaStream,
    Test.akkaTestkit
  )

}
