import com.typesafe.sbt.SbtGit.GitKeys._

import scala.util.Properties

cancelable in Global := true

//don't allow us to wipe all of our prod databases
flywayClean / aggregate := false
//allow us to wipe our test databases
Test / flywayClean / aggregate := true

lazy val commonCompilerOpts = {
  List(
    "-Xmax-classfile-name",
    "128",
    "-Xsource:2.12",
    "-target:jvm-1.8"
  )
}

//https://docs.scala-lang.org/overviews/compiler-options/index.html
lazy val compilerOpts = Seq(
  "-encoding",
  "UTF-8",
  "-unchecked",
  "-feature",
  "-deprecation",
  "-Xfuture",
  "-Ywarn-dead-code",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Ywarn-unused",
  "-unchecked",
  "-deprecation",
  "-feature"
) ++ commonCompilerOpts

lazy val testCompilerOpts = commonCompilerOpts

lazy val isCI = {
  sys.props
    .get("CI")
    .isDefined
}
lazy val commonSettings = List(
  organization := "org.bitcoin-s",
  homepage := Some(url("https://bitcoin-s.org")),
  developers := List(
    Developer(
      "christewart",
      "Chris Stewart",
      "stewart.chris1234@gmail.com",
      url("https://twitter.com/Chris_Stewart_5")
    )
  ),
  ////
  // scaladoc settings
  Compile / doc / scalacOptions ++= List(
    "-doc-title",
    "Bitcoin-S",
    "-doc-version",
    version.value
  ),
  // Set apiURL to define the base URL for the Scaladocs for our library.
  // This will enable clients of our library to automatically link against
  // the API documentation using autoAPIMappings.
  apiURL := homepage.value.map(_.toString + "/api").map(url(_)),
  // scaladoc settings end
  ////
  scalacOptions in Compile := compilerOpts,
  scalacOptions in Test := testCompilerOpts,
  Compile / compile / javacOptions ++= {
    if (isCI) {
      //jdk11 is used on CI, we need to use the --release flag to make sure
      //byte code is compatible with jdk 8
      //https://github.com/eclipse/jetty.project/issues/3244#issuecomment-495322586
      Seq("--release", "8")
    } else {
      Seq("-source", "1.8", "-target", "1.8")
    }
  },
  //show full stack trace of failed tests
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),
  //show duration of tests
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  // Travis has performance issues on macOS
  Test / parallelExecution := !(Properties.isMac && isCI)
)

lazy val commonTestSettings = Seq(
  publish / skip := true
) ++ commonSettings

lazy val commonTestWithDbSettings = Seq(
  // To make in-memory DBs work properly
  Test / fork := true,
  // To avoid deadlock issues with SQLite
  Test / parallelExecution := false
) ++ commonTestSettings

lazy val commonProdSettings = commonSettings

lazy val Benchmark = config("bench") extend Test
lazy val benchSettings: Seq[Def.SettingsDefinition] = {
  //for scalameter
  //https://scalameter.github.io/home/download/
  //you can add benchmarking to a project by adding these to lines
  //to the projects build definition
  //  .settings(benchSettings: _*)
  //  .configs(Benchmark)
  List(
    unmanagedSourceDirectories in Test += baseDirectory.value / "src" / "bench" / "scala",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Benchmark := false,
    outputStrategy in Benchmark := Some(StdoutOutput),
    fork in Benchmark := true,
    connectInput in Benchmark := true,
    inConfig(Benchmark)(Defaults.testSettings)
  )
}

// quoting the val name this way makes it appear as
// 'bitcoin-s' in sbt/bloop instead of 'bitcoins'
lazy val `bitcoin-s` = project
  .in(file("."))
  .aggregate(
    secp256k1jni,
    chain,
    chainTest,
    cli,
    cliTest,
    core,
    coreTest,
    dbCommons,
    bitcoindRpc,
    bitcoindRpcTest,
    bench,
    eclairRpc,
    eclairRpcTest,
    node,
    nodeTest,
    picklers,
    wallet,
    walletTest,
    walletServer,
    walletServerTest,
    testkit,
    scripts,
    zmq
  )
  .settings(commonSettings: _*)
  // crossScalaVersions must be set to Nil on the aggregating project
  .settings(crossScalaVersions := Nil)
  // unidoc aggregates Scaladocs for all subprojects into one big doc
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    // we modify the unidoc task to move the generated Scaladocs into the
    // website directory afterwards
    Compile / unidoc := {
      import java.nio.file._
      import scala.collection.JavaConverters._
      val logger = streams.value.log

      def cleanPath(path: Path, isRoot: Boolean = true): Unit = {
        if (Files.isDirectory(path)) {
          path.toFile.list().map { file =>
            val toClean = path.resolve(file)
            cleanPath(toClean, isRoot = false)
          }
          if (isRoot) ()
          else Files.deleteIfExists(path)
        } else if (isRoot) {
          ()
        } else if (path.toString.endsWith(".gitkeep")) {
          ()
        } else {
          Files.deleteIfExists(path)
        }
      }

      val websiteScaladocDir =
        Paths.get("website", "static", "api").toAbsolutePath

      logger.info(s"Cleaning website Scaladoc directory $websiteScaladocDir")
      cleanPath(websiteScaladocDir)

      // returned value is a list of files,
      // list has one element
      val generatedDir = (Compile / unidoc).value.head

      logger.info(s"Moving files in $generatedDir to $websiteScaladocDir")

      try {
        Files
          .walk(generatedDir.toPath)
          .iterator()
          .asScala
          .drop(1) // skip the root directory
          .foreach { child =>
            val pathDiff = generatedDir.toPath.relativize(child)
            Files.copy(child,
                       websiteScaladocDir.resolve(pathDiff),
                       StandardCopyOption.REPLACE_EXISTING)
          }
      } catch {
        case e: Throwable =>
          logger.err(
            "Error when copying Scaladocs to website folder: ${e.toString}")
          throw e
      }
      Seq(generatedDir)
    }
  )
  .settings(
    name := "bitcoin-s",
    gitRemoteRepo := "git@github.com:bitcoin-s/bitcoin-s-core.git",
    publish / skip := true
  )

lazy val secp256k1jni = project
  .in(file("secp256k1jni"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Deps.secp256k1jni,
    // we place lib files in this directory
    unmanagedResourceDirectories in Compile += baseDirectory.value / "natives",
    //since this is not a scala module, we have no code coverage
    //this also doesn't place nice with scoverage, see
    //https://github.com/scoverage/sbt-scoverage/issues/275
    coverageEnabled := false
  )

/**
  * If you want sbt projects to depend on each other in
  * slighly nonstandard ways, the way to do it is through
  * stringly typed syntax.
  *
  * Suppose you have a module A and a module B:
  * {{{
  * lazy val A = project.in(file("A"))
  * lazy val B = project.in(file("B"))
  *    .dependsOn(A)
  * // .dependsOn(A % "compile->compile")
  * // the line above is equivalent to the one
  * // above it
  * }}}
  *
  * With this setup, main sources in B can access
  * main sources in A
  *
  * To make test sources in A available to test
  * sources in B, you'd write this:
  * {{{
  * lazy val A = project.in(file("A"))
  * lazy val B = project.in(file("B"))
  *    .dependsOn(A % "test->test")
  * }}}
  *
  * And finally, to make main sources able to access
  * main sources as well as test sources able to
  * access test sources:
  * {{{
  * lazy val A = project.in(file("A"))
  * lazy val B = project.in(file("B"))
  *    .dependsOn(A % "compile->compile;test->test")
  * }}}
  *
  * We use this to make logging configuration in core
  * propagate to tests in the other modules, without
  * being published in the generated JARs.
  */
val testAndCompile = "compile->compile;test->test"

lazy val core = project
  .in(file("core"))
  .settings(commonProdSettings: _*)
  .dependsOn(
    secp256k1jni
  )

lazy val coreTest = project
  .in(file("core-test"))
  .settings(commonTestSettings: _*)
  .settings(
    name := "bitcoin-s-core-test"
  )
  .dependsOn(
    core % testAndCompile,
    testkit
  )

lazy val walletServer = project
  .in(file("app/server"))
  .settings(commonSettings: _*)
  .dependsOn(
    picklers,
    node,
    chain,
    wallet,
    bitcoindRpc
  )

lazy val walletServerTest = project
  .in(file("app/server-test"))
  .settings(commonTestSettings)
  .dependsOn(
    walletServer,
    testkit
  )

// internal picklers used by server
// and CLI
lazy val picklers = project
  .in(file("app/picklers"))
  .settings(commonSettings: _*)
  .dependsOn(core % testAndCompile)

lazy val cli = project
  .in(file("app/cli"))
  .settings(commonSettings: _*)
  .dependsOn(
    picklers
  )

lazy val cliTest = project
  .in(file("app/cli-test"))
  .settings(commonTestSettings: _*)
  .dependsOn(
    cli,
    testkit
  )

lazy val chainDbSettings = dbFlywaySettings("chaindb")
lazy val chain = project
  .in(file("chain"))
  .settings(commonProdSettings: _*)
  .settings(chainDbSettings: _*)
  .settings(
    name := "bitcoin-s-chain",
    libraryDependencies ++= Deps.chain
  )
  .dependsOn(core, dbCommons)
  .enablePlugins(FlywayPlugin)

lazy val chainTest = project
  .in(file("chain-test"))
  .settings(commonTestWithDbSettings: _*)
  .settings(chainDbSettings: _*)
  .settings(
    name := "bitcoin-s-chain-test",
    libraryDependencies ++= Deps.chainTest
  )
  .dependsOn(chain, core % testAndCompile, testkit, zmq)
  .enablePlugins(FlywayPlugin)

lazy val dbCommons = project
  .in(file("db-commons"))
  .settings(commonSettings: _*)
  .settings(
    name := "bitcoin-s-db-commons",
    libraryDependencies ++= Deps.dbCommons
  )
  .dependsOn(core)

lazy val zmq = project
  .in(file("zmq"))
  .settings(commonSettings: _*)
  .settings(name := "bitcoin-s-zmq", libraryDependencies ++= Deps.bitcoindZmq)
  .dependsOn(
    core % testAndCompile
  )

lazy val bitcoindRpc = project
  .in(file("bitcoind-rpc"))
  .settings(commonProdSettings: _*)
  .settings(name := "bitcoin-s-bitcoind-rpc",
            libraryDependencies ++= Deps.bitcoindRpc)
  .dependsOn(core)

lazy val bitcoindRpcTest = project
  .in(file("bitcoind-rpc-test"))
  .settings(commonTestSettings: _*)
  .settings(libraryDependencies ++= Deps.bitcoindRpcTest,
            name := "bitcoin-s-bitcoind-rpc-test")
  .dependsOn(core % testAndCompile, testkit)

lazy val bench = project
  .in(file("bench"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Deps.bench,
    name := "bitcoin-s-bench",
    skip in publish := true
  )
  .dependsOn(core % testAndCompile)

lazy val eclairRpc = project
  .in(file("eclair-rpc"))
  .settings(commonProdSettings: _*)
  .settings(name := "bitcoin-s-eclair-rpc",
            libraryDependencies ++= Deps.eclairRpc)
  .dependsOn(
    core,
    bitcoindRpc
  )

lazy val eclairRpcTest = project
  .in(file("eclair-rpc-test"))
  .settings(commonTestSettings: _*)
  .settings(libraryDependencies ++= Deps.eclairRpcTest,
            name := "bitcoin-s-eclair-rpc-test")
  .dependsOn(core % testAndCompile, testkit)

lazy val nodeDbSettings = dbFlywaySettings("nodedb")
lazy val node =
  project
    .in(file("node"))
    .settings(commonSettings: _*)
    .settings(nodeDbSettings: _*)
    .settings(
      name := "bitcoin-s-node",
      libraryDependencies ++= Deps.node
    )
    .dependsOn(
      core,
      chain,
      dbCommons,
      bitcoindRpc
    )
    .enablePlugins(FlywayPlugin)

lazy val nodeTest =
  project
    .in(file("node-test"))
    .settings(commonTestWithDbSettings: _*)
    .settings(nodeDbSettings: _*)
    .settings(
      name := "bitcoin-s-node-test",
      // There's a weird issue with forking
      // in node tests, for example this CI
      // error: https://travis-ci.org/bitcoin-s/bitcoin-s-core/jobs/525018199#L1252
      // It seems to be related to this
      // Scalatest issue:
      // https://github.com/scalatest/scalatest/issues/556
      Test / fork := false,
      libraryDependencies ++= Deps.nodeTest
    )
    .dependsOn(
      core % testAndCompile,
      node,
      testkit
    )
    .enablePlugins(FlywayPlugin)

lazy val testkit = project
  .in(file("testkit"))
  .settings(commonSettings: _*)
  .dependsOn(
    core % testAndCompile,
    walletServer,
    chain,
    bitcoindRpc,
    eclairRpc,
    node,
    wallet,
    zmq
  )

lazy val docs = project
  .in(file("bitcoin-s-docs")) // important: it must not be docs/
  .settings(commonTestSettings: _*)
  .dependsOn(
    bitcoindRpc,
    core,
    eclairRpc,
    secp256k1jni,
    testkit,
    zmq
  )

lazy val walletDbSettings = dbFlywaySettings("walletdb")
lazy val wallet = project
  .in(file("wallet"))
  .settings(commonProdSettings: _*)
  .settings(walletDbSettings: _*)
  .settings(
    name := "bitcoin-s-wallet",
    libraryDependencies ++= Deps.wallet
  )
  .dependsOn(core, dbCommons)
  .enablePlugins(FlywayPlugin)

lazy val walletTest = project
  .in(file("wallet-test"))
  .settings(commonTestWithDbSettings: _*)
  .settings(walletDbSettings: _*)
  .settings(
    name := "bitcoin-s-wallet-test",
    libraryDependencies ++= Deps.walletTest
  )
  .dependsOn(core % testAndCompile, testkit, wallet)
  .enablePlugins(FlywayPlugin)

lazy val scripts = project
  .in(file("scripts"))
  .settings(commonTestSettings: _*)
  .settings(
    name := "bitcoin-s-scripts",
    libraryDependencies ++= Deps.scripts
  )
  .dependsOn(
    bitcoindRpc,
    chain,
    core % testAndCompile,
    eclairRpc,
    node,
    secp256k1jni,
    testkit,
    wallet,
    zmq
  )

/** Given a database name, returns the appropriate
  * Flyway settings we apply to a project (chain, node, wallet) */
def dbFlywaySettings(dbName: String): List[Setting[_]] = {
  lazy val DB_HOST = "localhost"
  lazy val DB_NAME = s"${dbName}.sqlite"
  lazy val network = "unittest" //mainnet, testnet3, regtest, unittest

  lazy val mainnetDir = s"${System.getenv("HOME")}/.bitcoin-s/mainnet/"
  lazy val testnetDir = s"${System.getenv("HOME")}/.bitcoin-s/testnet3/"
  lazy val regtestDir = s"${System.getenv("HOME")}/.bitcoin-s/regtest/"
  lazy val unittestDir = s"${System.getenv("HOME")}/.bitcoin-s/unittest/"

  lazy val dirs = List(mainnetDir, testnetDir, regtestDir, unittestDir)

  //create directies if they DNE
  dirs.foreach { d =>
    val file = new File(d)
    file.mkdirs()
    val db = new File(d + DB_NAME)
    db.createNewFile()
  }

  def makeNetworkSettings(directoryPath: String): List[Setting[_]] = List(
    Test / flywayUrl := s"jdbc:sqlite:$directoryPath$DB_NAME",
    Test / flywayLocations := List("nodedb/migration"),
    Test / flywayUser := "nodedb",
    Test / flywayPassword := "",
    flywayUrl := s"jdbc:sqlite:$directoryPath$DB_NAME",
    flywayUser := "nodedb",
    flywayPassword := ""
  )

  lazy val mainnet = makeNetworkSettings(mainnetDir)

  lazy val testnet3 = makeNetworkSettings(testnetDir)

  lazy val regtest = makeNetworkSettings(regtestDir)

  lazy val unittest = makeNetworkSettings(unittestDir)

  network match {
    case "mainnet"  => mainnet
    case "testnet3" => testnet3
    case "regtest"  => regtest
    case "unittest" => unittest
    case unknown: String =>
      throw new IllegalArgumentException(s"Unknown network=${unknown}")
  }
}
