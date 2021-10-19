import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._

object Deps {

  object V {
    val bouncyCastle = "1.70"
    val dropwizardMetricsV = "4.2.9" //https://github.com/dropwizard/metrics

    val logback = "1.2.11"
    val log4jV = "1.2.17"
    val logkitV = "1.0.1"
    val avalonLoggingV = "4.1.5"

    val grizzledSlf4j = "1.3.4"
    val scalacheck = "1.15.4"
    val scalaTest = "3.2.12"

    val scalaTestPlus =
      "3.2.12.0-RC2" //super annoying... https://oss.sonatype.org/content/groups/public/org/scalatestplus/
    val slf4j = "1.7.36"
    val spray = "1.3.6"
    val zeromq = "0.5.2"
    val scalapb = "0.11.10"
    val akkav = "10.2.9"
    val playv = "2.9.2"
    val akkaStreamv = "2.6.19"
    val jUnixSocketV = "2.4.0"
    val scodecV = "1.1.30"
    val junitV = "0.13.3"
    val nativeLoaderV = "2.4.0"
    val typesafeConfigV = "1.4.2"

    val scalaFxV = "17.0.1-R26"
    val javaFxV = "18-ea+10"

    val asyncNewScalaV = "1.0.1"

    val flywayV = "8.5.9"
    val postgresV = "42.3.4"
    val akkaActorV = akkaStreamv
    val slickV = "3.3.3"
    val sqliteV = "3.36.0.3"

    val scalameterV = "0.17"
    val scalamockV = "5.2.0"
    val scalaCollectionCompatV = "2.6.0"
    val pgEmbeddedV = "0.13.4"

    val breezeV = "1.3"

    val newMicroPickleV = "1.6.0"
    val newMicroJsonV = newMicroPickleV

    val osgiFrameworkV = "1.10.0"
    val osgiJdbcV = "1.0.1"
    val osgiCoreV = "8.0.0"

    // akka-http-upickle is not yet published
    // to Maven central. There's a PR for adding
    // suport, https://github.com/hseeberger/akka-http-json/pull/314.
    // Until that's merged, you'll have to pull down
    // that PR, do `sbt publishLocal` and replace the
    // value here with whatever is in there. This
    // obviously has to be changed before this is
    // merged.

    val scalaJsStubsV = "1.1.0"
    // CLI deps
    val scoptV = "4.0.1"
    val sttpV = "3.6.1"
    val codehausV = "3.1.7"
    val scalaJsTimeV = "2.3.0"
    val zxingV = "3.4.1"

    val monixV = "3.4.0"

    val javaxServletV = "4.0.1"
    val javaxJmsV = "2.0.1"
    val javaxMailV = "1.4.7"

    val gsonV = "2.9.0"
    val jnaV = "5.11.0"
    val waffleJnaV = "1.9.1"
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

    val scalapb =
      "com.thesamet.scalapb" %% "scalapb-runtime" % V.scalapb % "protobuf"

    val akkaHttp =
      "com.typesafe.akka" %% "akka-http" % V.akkav withSources () withJavadoc ()

    val akkaHttp2 =
      "com.typesafe.akka" %% "akka-http2-support" % V.akkav withSources () withJavadoc ()

    val akkaStream =
      "com.typesafe.akka" %% "akka-stream" % V.akkaStreamv withSources () withJavadoc ()

    val akkaDiscovery =
      "com.typesafe.akka" %% "akka-discovery" % V.akkaStreamv withSources () withJavadoc ()

    val akkaActor =
      "com.typesafe.akka" %% "akka-actor" % V.akkaStreamv withSources () withJavadoc ()

    val akkaSlf4j =
      "com.typesafe.akka" %% "akka-slf4j" % V.akkaStreamv withSources () withJavadoc ()

    val akkaTestkit =
      "com.typesafe.akka" %% "akka-testkit" % V.akkaActorV withSources () withJavadoc ()

    val gson = "com.google.code.gson" % "gson" % V.gsonV //https://github.com/google/gson

    val jUnixSocket =
      "com.kohlschutter.junixsocket" % "junixsocket-core" % V.jUnixSocketV

    val scalaFx =
      "org.scalafx" %% "scalafx" % V.scalaFxV withSources () withJavadoc ()

    lazy val arch = System.getProperty("os.arch")

    lazy val osName = System.getProperty("os.name") match {
      case n if n.startsWith("Linux") => "linux"
      case n if n.startsWith("Mac") =>
        if (arch == "aarch64") {
          //needed to accommodate the different chip
          //arch for M1
          s"mac-${arch}"
        } else {
          "mac"
        }
      case n if n.startsWith("Windows") => "win"
      case x                            => throw new Exception(s"Unknown platform $x!")
    }

    // Not sure if all of these are needed, some might be possible to remove
    lazy val javaFxBase =
      "org.openjfx" % s"javafx-base" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxControls =
      "org.openjfx" % s"javafx-controls" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxGraphics =
      "org.openjfx" % s"javafx-graphics" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxMedia =
      "org.openjfx" % s"javafx-media" % V.javaFxV classifier osName withSources () withJavadoc ()

    lazy val javaFxDeps =
      List(javaFxBase, javaFxControls, javaFxGraphics, javaFxMedia)

    val javaxServlet = "javax.servlet" % "javax.servlet-api" % V.javaxServletV // https://mvnrepository.com/artifact/javax.servlet/javax.servlet-api
    val javaxJms = "javax.jms" % "javax.jms-api" % V.javaxJmsV // https://mvnrepository.com/artifact/javax.jms/javax.jms-api
    val javaxMail = "javax.mail" % "mail" % V.javaxMailV // https://mvnrepository.com/artifact/javax.mail/mail


    val jna = "net.java.dev.jna" % "jna" % V.jnaV
    val waffleJna = "com.github.waffle" % "waffle-jna" % V.waffleJnaV

    val breezeViz =
      ("org.scalanlp" %% "breeze-viz" % V.breezeV withSources () withJavadoc ())
        .exclude("bouncycastle", "bcprov-jdk14")

    val playJson =
      "com.typesafe.play" %% "play-json" % V.playv withSources () withJavadoc ()

    val typesafeConfig =
      "com.typesafe" % "config" % V.typesafeConfigV withSources () withJavadoc ()

    val logback =
      "ch.qos.logback" % "logback-classic" % V.logback withSources () withJavadoc ()

    val log4j = "log4j" % "log4j" % V.log4jV //https://github.com/apache/commons-logging/blob/0d4f2604ada038fd95e714d504d2278f1bd5814a/pom.xml#L486
    val logkit = "logkit" % "logkit" % V.logkitV //https://github.com/apache/commons-logging/blob/0d4f2604ada038fd95e714d504d2278f1bd5814a/pom.xml#L492
    val avalonLogging = "avalon-framework" % "avalon-framework" % V.avalonLoggingV //https://github.com/apache/commons-logging/blob/0d4f2604ada038fd95e714d504d2278f1bd5814a/pom.xml#L498

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

    val osgiFramework = "org.osgi" % "org.osgi.framework" % V.osgiFrameworkV // https://mvnrepository.com/artifact/org.osgi/org.osgi.framework,
    val osgiJdbc = "org.osgi" % "org.osgi.service.jdbc" % V.osgiJdbcV // https://mvnrepository.com/artifact/org.osgi/org.osgi.service.jdbc
    val osgiCore = "org.osgi" % "osgi.core" % V.osgiCoreV // https://mvnrepository.com/artifact/org.osgi/osgi.core,

    // parsing of CLI opts and args
    val scopt = "com.github.scopt" %% "scopt" % V.scoptV

    // HTTP client lib
    val sttp = "com.softwaremill.sttp.client3" %% "core" % V.sttpV

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
      "org.scalatestplus" %%% "scalacheck-1-15" % V.scalaTestPlus withSources () withJavadoc ())

    val pgEmbedded =
      "com.opentable.components" % "otj-pg-embedded" % V.pgEmbeddedV withSources () withJavadoc ()

    val dropwizardMetricsCore =
      "io.dropwizard.metrics" % "metrics-core" % V.dropwizardMetricsV withSources () withJavadoc ()

    val dropwizardMetricsHealthChecks = "io.dropwizard.metrics" % "metrics-healthchecks" % V.dropwizardMetricsV
    val dropwizardMetricsJvm = "io.dropwizard.metrics" % "metrics-jvm" % V.dropwizardMetricsV // https://mvnrepository.com/artifact/io.dropwizard.metrics/metrics-jvm

    val zxingCore =
      "com.google.zxing" % "core" % V.zxingV withSources () withJavadoc ()

    val zxingJ2SE =
      "com.google.zxing" % "javase" % V.zxingV withSources () withJavadoc ()

    val monixExecution =
      Def.setting(
        "io.monix" %%% "monix-execution" % V.monixV withSources () withJavadoc ())
  }

  object Test {

    val newAsync =
      "org.scala-lang.modules" %% "scala-async" % V.asyncNewScalaV % "test" withSources () withJavadoc ()

    val junitInterface =
      "com.github.sbt" % "junit-interface" % V.junitV % "test" withSources () withJavadoc ()
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

    val scalameter =
      "com.storm-enroute" %% "scalameter" % V.scalameterV % "test" withSources () withJavadoc ()

    val scalaCollectionCompat =
      "org.scala-lang.modules" %% "scala-collection-compat" % V.scalaCollectionCompatV

    val pgEmbedded =
      "com.opentable.components" % "otj-pg-embedded" % V.pgEmbeddedV % "test" withSources () withJavadoc ()

    val akkaTestkit =
      "com.typesafe.akka" %% "akka-testkit" % V.akkaActorV withSources () withJavadoc ()
  }

  def asyncUtils = Def.setting {
    Vector(Compile.monixExecution.value)
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
      Compile.grizzledSlf4j,
      Compile.typesafeConfig
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

  val dlcWallet =
    List(
      Compile.newMicroJson,
      Compile.grizzledSlf4j
    )

  val dlcNode =
    List(
      Compile.newMicroJson,
      Compile.grizzledSlf4j,
      Compile.akkaActor
    )

  val dlcNodeTest =
    List(
      Test.akkaTestkit
    )

  val dlcWalletTest =
    List(
      Test.akkaTestkit,
      Test.pgEmbedded
    )

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
      Test.scalaCollectionCompat,
      Compile.logback
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
      Compile.dropwizardMetricsCore,
      Compile.flyway,
      Compile.slick,
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
      Compile.scopt
    )
  }

  val gui = List(Compile.akkaActor,
                 Compile.breezeViz,
                 Compile.scalaFx,
                 Compile.zxingCore,
                 Compile.zxingJ2SE) ++ Compile.javaFxDeps

  val server = Def.setting {
    Vector(
      Compile.newMicroPickle.value,
      Compile.logback,
      Compile.akkaActor,
      Compile.akkaHttp,
      Compile.akkaStream,
      Compile.akkaSlf4j
    ) ++ appServerTransitiveDeps
  }

  /** Transitive dependencies needed for the oracleServer to build properly with jlink */
  private val serverTransitiveDeps = Vector(
    //transitive deps needed for jlink
    Compile.codehaus,
    Compile.gson,

    Compile.dropwizardMetricsHealthChecks,
    Compile.dropwizardMetricsJvm,

    //postgres transitive deps
    Compile.jna,
    Compile.waffleJna,
    Compile.osgiCore,
    Compile.osgiJdbc,
    Compile.osgiFramework,

    //logging transitive deps
    Compile.log4j,
    Compile.avalonLogging,
    Compile.logkit,

    //transitive javax deps
    Compile.javaxServlet,
    Compile.javaxMail,
    Compile.javaxJms
  )


  private val appServerTransitiveDeps = serverTransitiveDeps
  private val oracleServerTransitiveDeps = serverTransitiveDeps

  val oracleServer = Def.setting {
    Vector(
      Compile.newMicroPickle.value,
      Compile.logback,
      Compile.akkaActor,
      Compile.akkaHttp,
      Compile.akkaSlf4j,
    ) ++ oracleServerTransitiveDeps
  }

  val eclairRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j,
    Compile.grizzledSlf4j
  )

  val clightningRpc = List(
    Compile.jUnixSocket,
    Compile.playJson,
    Compile.grizzledSlf4j
  )

  val clightningRpcTest = Def.setting {
    List(
      Test.logback,
      Test.scalaTest.value,
      Test.scalacheck.value
    )
  }

  val tor: Def.Initialize[List[ModuleID]] = Def.setting {
    List(
      Compile.akkaStream,
      Compile.akkaHttp,
      Compile.scodec.value,
      Compile.grizzledSlf4j
    )
  }

  val lndRpc = List(
    Compile.scalapb,
    Compile.akkaHttp,
    Compile.akkaHttp2,
    Compile.akkaStream,
    Compile.akkaDiscovery,
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

  val esplora = Def.setting {
    List(
      Compile.akkaHttp,
      Compile.akkaActor,
      Compile.akkaStream
    )
  }

  val esploraTest = Def.setting {
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
      Compile.scalacheck.value,
      Compile.scalaTest.value,
      Compile.scalaTestPlus.value,
      Compile.pgEmbedded,
      Compile.slf4j,
      Compile.grizzledSlf4j,
      Compile.akkaTestkit
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
    Compile.typesafeConfig,
    Test.scalaMock,
    Test.akkaHttpTestkit,
    Test.akkaStream
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

  val oracleExplorerClient = Vector(
    Compile.akkaActor,
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson
  )

  val dlcTest = Vector(
    Compile.playJson
  )
}
