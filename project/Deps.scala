import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._

object Deps {

  object V {
    val bouncyCastle = "1.68"
    val dropwizardMetricsV = "4.1.18" //https://github.com/dropwizard/metrics
    val logback = "1.2.3"
    val grizzledSlf4j = "1.3.4"
    val scalacheck = "1.15.3"
    val scalaTest = "3.2.7"

    val scalaTestPlus =
      "3.2.1.0" //super annoying... https://oss.sonatype.org/content/groups/public/org/scalatestplus/
    val slf4j = "1.7.30"
    val spray = "1.3.6"
    val zeromq = "0.5.2"
    val akkav = "10.2.4"
    val playv = "2.9.2"
    val akkaStreamv = "2.6.13"
    val scodecV = "1.1.25"
    val junitV = "0.11"
    val nativeLoaderV = "2.3.5"
    val typesafeConfigV = "1.4.1"

    val scalaFxV = "15.0.1-R21"
    val javaFxV = "17-ea+5"

    val asyncNewScalaV = "0.10.0"

    val flywayV = "6.4.2"
    val postgresV = "42.2.19"
    val akkaActorV = akkaStreamv
    val slickV = "3.3.3"
    val sqliteV = "3.34.0"

    val scalameterV = "0.17"
    val scalamockV = "5.1.0"
    val scalaCollectionCompatV = "2.4.3"
    val pgEmbeddedV = "0.13.3"

    val breezeV = "1.1"

    val newMicroPickleV = "1.3.8"
    val newMicroJsonV = newMicroPickleV

    // akka-http-upickle is not yet published
    // to Maven central. There's a PR for adding
    // suport, https://github.com/hseeberger/akka-http-json/pull/314.
    // Until that's merged, you'll have to pull down
    // that PR, do `sbt publishLocal` and replace the
    // value here with whatever is in there. This
    // obviously has to be changed before this is
    // merged.

    val sourcecodeV = "0.2.4"

    val scalaJsStubsV = "1.0.0"
    // CLI deps
    val scoptV = "4.0.1"
    val sttpV = "1.7.2"
    val codehausV = "3.1.3"
    val scalaJsTimeV = "2.2.0"
  }

  object Compile {

    val bouncycastle =
      "org.bouncycastle" % "bcprov-jdk15on" % V.bouncyCastle withSources () withJavadoc ()

    val scodec =
      Def.setting(
        "org.scodec" %%% "scodec-bits" % V.scodecV withSources () withJavadoc ())

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

    val akkaSlf4j =
      "com.typesafe.akka" %% "akka-slf4j" % V.akkaStreamv withSources () withJavadoc ()

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

    val breezeViz =
      ("org.scalanlp" %% "breeze-viz" % V.breezeV withSources () withJavadoc ())
        .exclude("bouncycastle", "bcprov-jdk14")

    val playJson =
      "com.typesafe.play" %% "play-json" % V.playv withSources () withJavadoc ()

    val typesafeConfig =
      "com.typesafe" % "config" % V.typesafeConfigV withSources () withJavadoc ()

    val logback =
      "ch.qos.logback" % "logback-classic" % V.logback withSources () withJavadoc ()

    val grizzledSlf4j =
      "org.clapper" %% "grizzled-slf4j" % V.grizzledSlf4j withSources () withJavadoc ()

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

    val newMicroPickle =
      Def.setting("com.lihaoyi" %%% "upickle" % V.newMicroPickleV)

    // get access to reflection data at compile-time
    val sourcecode = "com.lihaoyi" %% "sourcecode" % V.sourcecodeV

    // parsing of CLI opts and args
    val scopt = "com.github.scopt" %% "scopt" % V.scoptV

    // HTTP client lib
    val sttp = "com.softwaremill.sttp" %% "core" % V.sttpV

    val scalaCollectionCompat =
      "org.scala-lang.modules" %% "scala-collection-compat" % V.scalaCollectionCompatV

    val scalacheck =
      Def.setting(
        "org.scalacheck" %%% "scalacheck" % V.scalacheck withSources () withJavadoc ())

    val scalaJsStubs =
      "org.scala-js" %% "scalajs-stubs" % V.scalaJsStubsV % "provided"

    val scalaJsTime =
      Def.setting(
        "io.github.cquiroz" %%% "scala-java-time" % V.scalaJsTimeV withSources () withJavadoc ())

    val scalaTest =
      Def.setting(
        "org.scalatest" %%% "scalatest" % V.scalaTest withSources () withJavadoc ())

    val scalaTestPlus = Def.setting(
      "org.scalatestplus" %%% "scalacheck-1-14" % V.scalaTestPlus withSources () withJavadoc ())

    val pgEmbedded =
      "com.opentable.components" % "otj-pg-embedded" % V.pgEmbeddedV withSources () withJavadoc ()

    val dropwizardMetrics =
      "io.dropwizard.metrics" % "metrics-core" % V.dropwizardMetricsV withSources () withJavadoc ()
  }

  object Test {

    val newAsync =
      "org.scala-lang.modules" %% "scala-async" % V.asyncNewScalaV % "test" withSources () withJavadoc ()

    val junitInterface =
      "com.novocode" % "junit-interface" % V.junitV % "test" withSources () withJavadoc ()
    val logback = Compile.logback % "test"
    val grizzledSlf4j = Compile.grizzledSlf4j % "test"

    val scalacheck = Def.setting(
      "org.scalacheck" %%% "scalacheck" % V.scalacheck % "test" withSources () withJavadoc ())

    val scalaTest = Def.setting(
      "org.scalatest" %%% "scalatest" % V.scalaTest % "test" withSources () withJavadoc ())
    val scalaMock = "org.scalamock" %% "scalamock" % V.scalamockV

    val spray =
      "io.spray" %% "spray-json" % V.spray % "test" withSources () withJavadoc ()

    val akkaHttpTestkit =
      "com.typesafe.akka" %% "akka-http-testkit" % V.akkav % "test" withSources () withJavadoc ()

    val akkaStream =
      "com.typesafe.akka" %% "akka-stream-testkit" % V.akkaStreamv % "test" withSources () withJavadoc ()
    val playJson = Compile.playJson % "test"

    val akkaTestkit =
      "com.typesafe.akka" %% "akka-testkit" % V.akkaActorV withSources () withJavadoc ()

    val scalameter =
      "com.storm-enroute" %% "scalameter" % V.scalameterV % "test" withSources () withJavadoc ()

    val scalaCollectionCompat =
      "org.scala-lang.modules" %% "scala-collection-compat" % V.scalaCollectionCompatV

    val pgEmbedded =
      "com.opentable.components" % "otj-pg-embedded" % V.pgEmbeddedV % "test" withSources () withJavadoc ()
  }

  val chain = List(
    Compile.logback
  )

  val chainTest = List(
    Test.pgEmbedded
  )

  val appCommons = Def.setting {
    List(
      Compile.newMicroPickle.value,
      Compile.playJson,
      Compile.slf4j,
      Compile.grizzledSlf4j
    )
  }

  def core = Def.setting {
    List(
      Compile.bouncycastle,
      Compile.scodec.value
    )
  }

  val cryptoJVM = List(
    Compile.bouncycastle,
    Compile.scalaJsStubs
  )

  def crypto: Def.Initialize[Seq[ModuleID]] = {
    Def.setting {
      List(
        Compile.scodec.value
      )
    }
  }

  val secp256k1jni = List(
    Compile.nativeLoader,
    Test.junitInterface
  )

  def coreTest = Def.setting {
    List(
      Test.junitInterface,
      Test.scalaTest.value,
      Test.scalaCollectionCompat,
      Compile.newMicroPickle.value
    )
  }

  val coreTestJVM = Def.setting {
    List(
      Test.junitInterface,
      Test.scalaTest.value,
      Test.scalaCollectionCompat
    )
  }

  val coreJs = Def.setting {
    List(
      Compile.scalaJsTime.value
    )
  }

  def cryptoTest = Def.setting {
    List(
      Test.scalaTest.value,
      Test.scalacheck.value,
      Compile.scalaTestPlus.value
    )
  }

  def bitcoindZmq = Def.setting {
    List(
      Compile.zeromq,
      Compile.slf4j,
      Compile.grizzledSlf4j,
      Test.logback,
      Test.scalacheck.value,
      Test.scalaTest.value
    )
  }

  val bitcoindRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.typesafeConfig,
    Compile.slf4j,
    Compile.grizzledSlf4j
  )

  def bitcoindRpcTest = Def.setting {
    List(
      Test.akkaHttpTestkit,
      Test.akkaStream,
      Test.logback,
      Test.scalaTest.value,
      Test.scalacheck.value,
      Test.newAsync,
      Test.scalaCollectionCompat
    )
  }

  val bench = List(
    "org.slf4j" % "slf4j-api" % V.slf4j withSources () withJavadoc (),
    Compile.logback
  )

  def dbCommons = Def.setting {
    List(
      Compile.dropwizardMetrics,
      Compile.flyway,
      Compile.slick,
      Compile.sourcecode,
      Compile.logback,
      Compile.sqlite,
      Compile.postgres,
      Compile.slickHikari,
      Compile.slf4j,
      Compile.grizzledSlf4j,
      Test.scalaTest.value,
      Test.pgEmbedded
    )
  }

  val cli = Def.setting {
    List(
      Compile.sttp,
      Compile.newMicroPickle.value,
      Compile.logback,
      Compile.scopt,
      //we can remove this dependency when this is fixed
      //https://github.com/oracle/graal/issues/1943
      //see https://github.com/bitcoin-s/bitcoin-s/issues/1100
      Compile.codehaus
    )
  }

  val gui = List(Compile.breezeViz, Compile.scalaFx) ++ Compile.javaFxDeps

  val server = Def.setting {
    List(
      Compile.newMicroPickle.value,
      Compile.logback,
      Compile.akkaActor,
      Compile.akkaHttp,
      Compile.akkaSlf4j
    )
  }

  val oracleServer = Def.setting {
    List(
      Compile.newMicroPickle.value,
      Compile.logback,
      Compile.akkaActor,
      Compile.akkaHttp,
      Compile.akkaSlf4j
    )
  }

  val eclairRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j,
    Compile.grizzledSlf4j
  )

  def eclairRpcTest = Def.setting {
    List(
      Test.akkaHttpTestkit,
      Test.akkaStream,
      Test.logback,
      Test.scalaTest.value,
      Test.scalacheck.value
    )
  }

  def feeProvider = Def.setting {
    List(
      Compile.akkaHttp,
      Compile.akkaActor,
      Compile.akkaStream
    )
  }

  def feeProviderTest = Def.setting {
    List(
      Test.akkaTestkit,
      Test.scalaTest.value
    )
  }

  val node = List(
    Compile.akkaActor,
    Compile.logback,
    Compile.slick,
    Compile.slickHikari,
    Compile.sqlite,
    Compile.slf4j,
    Compile.grizzledSlf4j
  )

  val nodeTest = Def.setting {
    List(
      Test.akkaTestkit,
      Test.scalaTest.value,
      Test.pgEmbedded
    )
  }

  def testkitCore = Def.setting {
    List(
      Compile.newMicroPickle.value,
      Compile.scalaCollectionCompat,
      Compile.scalacheck.value,
      Compile.scalaTest.value,
      Compile.scalaTestPlus.value
    )
  }

  def testkit = Def.setting {
    List(
      Compile.slf4j,
      Compile.scalacheck.value,
      Compile.scalaTest.value,
      Compile.scalaTestPlus.value,
      Compile.pgEmbedded,
      Compile.slf4j,
      Compile.grizzledSlf4j,
      Test.akkaTestkit
    )
  }

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
      Compile.logback,
      Compile.slf4j,
      Compile.grizzledSlf4j
    )

  val walletTest = List(
    Test.akkaTestkit,
    Test.pgEmbedded
  )

  def docs = Def.setting {
    List(
      Compile.logback,
      Test.scalaTest.value,
      Test.logback
    )
  }

  val walletServerTest = List(
    Test.scalaMock,
    Test.akkaHttpTestkit,
    Test.akkaStream,
    Test.akkaTestkit
  )

  val dlcOracle =
    List(
      Compile.newMicroJson,
      Compile.logback,
      Compile.slf4j,
      Compile.grizzledSlf4j
    )

  val dlcOracleTest =
    List(
      Compile.newMicroJson,
      Compile.logback
    )

  val serverRoutes = List(
    Compile.akkaHttp,
    Compile.akkaActor,
    Compile.akkaSlf4j,
    Compile.akkaStream,
    Compile.slf4j,
    Compile.grizzledSlf4j
  )
}
