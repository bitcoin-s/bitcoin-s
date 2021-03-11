import Deps.V
import com.typesafe.sbt.SbtGit.GitKeys._

import scala.util.Properties

cancelable in Global := true

//don't allow us to wipe all of our prod databases
flywayClean / aggregate := false
//allow us to wipe our test databases
Test / flywayClean / aggregate := true

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

import Projects._

lazy val commonJsSettings = {
  Seq(
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
    },
    skip.in(publish) := true
  ) ++ CommonSettings.settings ++
    //get rid of -Xfatal-warnings for now with scalajs
    //this will just give us a bunch of warnings rather than errors
    Seq(
      scalacOptions in Compile ~= (_ filterNot (s => s == "-Xfatal-warnings")))
}

lazy val crypto = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(
    name := "bitcoin-s-crypto",
    libraryDependencies ++= Deps.crypto.value
  )
  .settings(CommonSettings.settings: _*)
  .jvmSettings(
    libraryDependencies ++= Deps.cryptoJVM
  )
  .jsSettings(
    npmDependencies in Compile ++= Seq(
      "bcrypto" -> "5.4.0"
    )
  )
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .in(file("crypto"))

lazy val cryptoJS = crypto.js
  .enablePlugins(ScalaJSBundlerPlugin)

lazy val cryptoJVM = crypto.jvm
  .dependsOn(secp256k1jni)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(name := "bitcoin-s-core")
  .settings(libraryDependencies ++= Deps.core.value)
  .settings(CommonSettings.settings: _*)
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .in(file("core"))
  .dependsOn(crypto)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js

lazy val asyncUtils = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("async-utils"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(name := "bitcoin-s-async-utils")
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(core)

lazy val asyncUtilsJVM = asyncUtils.jvm

lazy val asyncUtilsJS = asyncUtils.js

lazy val asyncUtilsTest = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("async-utils-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(name := "bitcoin-s-async-utils-test")
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(asyncUtils, testkitCore)

lazy val asyncUtilsTestJVM = asyncUtilsTest.jvm

lazy val asyncUtilsTestJS = asyncUtilsTest.js

lazy val testkitCore = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("testkit-core"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(name := "bitcoin-s-testkit-core",
            libraryDependencies ++= Deps.testkitCore.value)
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(asyncUtils, core, crypto)

lazy val testkitCoreJVM = testkitCore.jvm

lazy val testkitCoreJS = testkitCore.js

lazy val bitcoindRpc = project
  .in(file("bitcoind-rpc"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(
    asyncUtilsJVM,
    appCommons
  )

lazy val eclairRpc = project
  .in(file("eclair-rpc"))
  .dependsOn(asyncUtilsJVM, bitcoindRpc)

lazy val jsProjects: Vector[ProjectReference] =
  Vector(asyncUtilsJS,
         asyncUtilsTestJS,
         cryptoJS,
         coreJS,
         cryptoTestJS,
         coreTestJS,
         testkitCoreJS)

// quoting the val name this way makes it appear as
// 'bitcoin-s' in sbt/bloop instead of 'bitcoins'
lazy val `bitcoin-s` = project
  .in(file("."))
  .aggregate(
    asyncUtilsJVM,
    secp256k1jni,
    chain,
    chainTest,
    cli,
    cliTest,
    coreJVM,
    coreJS,
    coreTestJVM,
    coreTestJS,
    cryptoJVM,
    cryptoJS,
    cryptoTestJVM,
    cryptoTestJS,
    dbCommons,
    dbCommonsTest,
    feeProvider,
    feeProviderTest,
    dlcOracle,
    dlcOracleTest,
    bitcoindRpc,
    bitcoindRpcTest,
    bench,
    eclairRpc,
    eclairRpcTest,
    bundle,
    gui,
    keyManager,
    keyManagerTest,
    node,
    nodeTest,
    wallet,
    walletTest,
    appServer,
    appServerTest,
    appCommons,
    appCommonsTest,
    testkitCoreJVM,
    testkitCoreJS,
    testkit,
    zmq,
    oracleServer,
    oracleServerTest,
    serverRoutes
  )
  .dependsOn(
    secp256k1jni,
    chain,
    chainTest,
    cli,
    cliTest,
    coreJVM,
    coreJS,
    coreTestJVM,
    coreTestJS,
    cryptoJVM,
    cryptoJS,
    cryptoTestJVM,
    cryptoTestJS,
    dbCommons,
    dbCommonsTest,
    feeProvider,
    feeProviderTest,
    dlcOracle,
    dlcOracleTest,
    bitcoindRpc,
    bitcoindRpcTest,
    bench,
    eclairRpc,
    eclairRpcTest,
    bundle,
    gui,
    keyManager,
    keyManagerTest,
    node,
    nodeTest,
    wallet,
    walletTest,
    appServer,
    appServerTest,
    appCommons,
    appCommonsTest,
    testkit,
    zmq,
    oracleServer,
    oracleServerTest,
    serverRoutes
  )
  .settings(CommonSettings.settings: _*)
  // unidoc aggregates Scaladocs for all subprojects into one big doc
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    //removes scalajs projects from unidoc, see
    //https://github.com/bitcoin-s/bitcoin-s/issues/2740
    unidocProjectFilter in (ScalaUnidoc, unidoc) := {
      inAnyProject -- inProjects(jsProjects: _*)
    },
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
            Files.copy(
              child,
              websiteScaladocDir.resolve(pathDiff),
              StandardCopyOption.REPLACE_EXISTING
            )
          }
      } catch {
        case e: Throwable =>
          logger.err(
            "Error when copying Scaladocs to website folder: ${e.toString}"
          )
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
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    libraryDependencies ++= Deps.secp256k1jni,
    // we place lib files in this directory
    unmanagedResourceDirectories in Compile += baseDirectory.value / "natives",
    //since this is not a scala module, we have no code coverage
    //this also doesn't place nice with scoverage, see
    //https://github.com/scoverage/sbt-scoverage/issues/275
    coverageEnabled := false
  )

/** If you want sbt projects to depend on each other in
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

lazy val cryptoTest = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("crypto-test"))
  .settings(CommonSettings.testSettings: _*)
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .settings(
    name := "bitcoin-s-crypto-test",
    libraryDependencies ++= Deps.cryptoTest.value
  )
  .dependsOn(crypto)

lazy val cryptoTestJVM = cryptoTest.jvm

lazy val cryptoTestJS = cryptoTest.js
  .enablePlugins(ScalaJSBundlerPlugin)

lazy val coreTest = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-core-test",
    libraryDependencies ++= Deps.coreTest.value
  )
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(
    core,
    testkitCore
  )

lazy val coreTestJVM = coreTest.jvm

lazy val coreTestJS = coreTest.js

lazy val appCommons = project
  .in(file("app-commons"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(
    coreJVM % testAndCompile
  )

lazy val appCommonsTest = project
  .in(file("app-commons-test"))
  .settings(CommonSettings.testSettings: _*)
  .dependsOn(appCommons, testkit)

lazy val oracleServer = project
  .in(file("app/oracle-server"))
  .settings(CommonSettings.appSettings: _*)
  .settings(CommonSettings.dockerSettings: _*)
  .dependsOn(
    dlcOracle,
    serverRoutes
  )
  .enablePlugins(JavaAppPackaging, DockerPlugin)

lazy val oracleServerTest = project
  .in(file("app/oracle-server-test"))
  .settings(CommonSettings.testSettings)
  .settings(libraryDependencies ++= Deps.walletServerTest)
  .dependsOn(
    oracleServer,
    testkit
  )

lazy val serverRoutes = project
  .in(file("app/server-routes"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(name := "bitcoin-s-server-routes")
  .settings(libraryDependencies ++= Deps.serverRoutes)
  .dependsOn(appCommons, dbCommons)

lazy val appServer = project
  .in(file("app/server"))
  .settings(CommonSettings.appSettings: _*)
  .settings(CommonSettings.dockerSettings: _*)
  .dependsOn(
    serverRoutes,
    appCommons,
    node,
    chain,
    wallet,
    bitcoindRpc,
    feeProvider,
    zmq
  )
  .enablePlugins(JavaAppPackaging, DockerPlugin)

lazy val appServerTest = project
  .in(file("app/server-test"))
  .settings(CommonSettings.testSettings)
  .settings(libraryDependencies ++= Deps.walletServerTest)
  .dependsOn(
    appServer,
    testkit
  )

lazy val cli = project
  .in(file("app/cli"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-cli"
  )
  .dependsOn(
    appCommons
  )

lazy val cliTest = project
  .in(file("app/cli-test"))
  .settings(CommonSettings.testSettings: _*)
  .dependsOn(
    cli,
    testkit
  )

lazy val bundle = project
  .in(file("app/bundle"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(appServer, gui)

lazy val gui = project
  .in(file("app/gui"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(
    cli
  )

lazy val chainDbSettings = dbFlywaySettings("chaindb")

lazy val chain = project
  .in(file("chain"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(chainDbSettings: _*)
  .settings(
    name := "bitcoin-s-chain",
    libraryDependencies ++= Deps.chain
  )
  .dependsOn(coreJVM, dbCommons)
  .enablePlugins(FlywayPlugin)

lazy val chainTest = project
  .in(file("chain-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(chainDbSettings: _*)
  .settings(
    name := "bitcoin-s-chain-test",
    libraryDependencies ++= Deps.chainTest
  )
  .dependsOn(chain, coreJVM % testAndCompile, testkit, zmq)
  .enablePlugins(FlywayPlugin)

lazy val dbCommons = project
  .in(file("db-commons"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-db-commons",
    libraryDependencies ++= Deps.dbCommons.value
  )
  .dependsOn(coreJVM, appCommons)

lazy val dbCommonsTest = project
  .in(file("db-commons-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-db-commons-test"
  )
  .dependsOn(testkit)

lazy val feeProvider = project
  .in(file("fee-provider"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-fee-provider",
    libraryDependencies ++= Deps.feeProvider.value
  )
  .dependsOn(coreJVM, appCommons)

lazy val feeProviderTest = project
  .in(file("fee-provider-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-fee-provider-test",
    libraryDependencies ++= Deps.feeProviderTest.value
  )
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val zmq = project
  .in(file("zmq"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(name := "bitcoin-s-zmq",
            libraryDependencies ++= Deps.bitcoindZmq.value)
  .dependsOn(
    coreJVM % testAndCompile
  )

lazy val bitcoindRpcTest = project
  .in(file("bitcoind-rpc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(name := "bitcoin-s-bitcoind-rpc-test",
            libraryDependencies ++= Deps.bitcoindRpcTest.value)
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val bench = project
  .in(file("bench"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    libraryDependencies ++= Deps.bench,
    name := "bitcoin-s-bench",
    skip in publish := true
  )
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val eclairRpcTest = project
  .in(file("eclair-rpc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    libraryDependencies ++= Deps.eclairRpcTest.value,
    name := "bitcoin-s-eclair-rpc-test"
  )
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val nodeDbSettings = dbFlywaySettings("nodedb")

lazy val node =
  project
    .in(file("node"))
    .settings(CommonSettings.prodSettings: _*)
    .settings(nodeDbSettings: _*)
    .settings(
      name := "bitcoin-s-node",
      libraryDependencies ++= Deps.node
    )
    .dependsOn(
      asyncUtilsJVM,
      coreJVM,
      chain,
      dbCommons,
      bitcoindRpc
    )
    .enablePlugins(FlywayPlugin)

lazy val nodeTest =
  project
    .in(file("node-test"))
    .settings(CommonSettings.testSettings: _*)
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
      libraryDependencies ++= Deps.nodeTest.value
    )
    .dependsOn(
      coreJVM % testAndCompile,
      node,
      testkit
    )
    .enablePlugins(FlywayPlugin)

lazy val testkit = project
  .in(file("testkit"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-testkit",
    libraryDependencies ++= Deps.testkit.value
  )
  .dependsOn(
    asyncUtilsJVM,
    coreJVM % testAndCompile,
    appServer,
    chain,
    bitcoindRpc,
    eclairRpc,
    node,
    wallet,
    zmq,
    dlcOracle,
    testkitCoreJVM
  )

lazy val docs = project
  .in(file("bitcoin-s-docs")) // important: it must not be docs/
  .settings(CommonSettings.testSettings: _*)
  .settings(libraryDependencies ++= Deps.docs.value)
  .dependsOn(
    bitcoindRpc,
    chain,
    cli,
    coreJVM,
    eclairRpc,
    keyManager,
    secp256k1jni,
    testkit,
    wallet,
    zmq
  )

lazy val keyManager = project
  .in(file("key-manager"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(coreJVM, dbCommons)

lazy val keyManagerTest = project
  .in(file("key-manager-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(name := "bitcoin-s-keymanager-test",
            libraryDependencies ++= Deps.keyManagerTest)
  .dependsOn(keyManager, testkit)

lazy val walletDbSettings = dbFlywaySettings("walletdb")

lazy val wallet = project
  .in(file("wallet"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(walletDbSettings: _*)
  .settings(
    name := "bitcoin-s-wallet",
    libraryDependencies ++= Deps.wallet(scalaVersion.value)
  )
  .dependsOn(coreJVM, appCommons, dbCommons, keyManager)
  .enablePlugins(FlywayPlugin)

lazy val walletTest = project
  .in(file("wallet-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(walletDbSettings: _*)
  .settings(
    name := "bitcoin-s-wallet-test",
    libraryDependencies ++= Deps.walletTest
  )
  .dependsOn(coreJVM % testAndCompile, testkit, wallet)
  .enablePlugins(FlywayPlugin)

lazy val dlcOracle = project
  .in(file("dlc-oracle"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-oracle",
    libraryDependencies ++= Deps.dlcOracle
  )
  .dependsOn(coreJVM, keyManager, dbCommons)

lazy val dlcOracleTest = project
  .in(file("dlc-oracle-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-oracle-test",
    libraryDependencies ++= Deps.dlcOracleTest
  )
  .dependsOn(coreJVM % testAndCompile, dlcOracle, testkit)

/** Given a database name, returns the appropriate
  * Flyway settings we apply to a project (chain, node, wallet)
  */
def dbFlywaySettings(dbName: String): List[Setting[_]] = {
  lazy val DB_HOST = "localhost"
  lazy val DB_NAME = s"${dbName}.sqlite"
  lazy val network = "unittest" //mainnet, testnet3, regtest, unittest

  lazy val mainnetDir = s"${System.getenv("HOME")}/.bitcoin-s/mainnet/"
  lazy val testnetDir = s"${System.getenv("HOME")}/.bitcoin-s/testnet3/"
  lazy val regtestDir = s"${System.getenv("HOME")}/.bitcoin-s/regtest/"
  lazy val signetDir = s"${System.getenv("HOME")}/.bitcoin-s/signet/"
  lazy val unittestDir = s"${System.getenv("HOME")}/.bitcoin-s/unittest/"

  lazy val dirs =
    List(mainnetDir, testnetDir, regtestDir, signetDir, unittestDir)

  //create directies if they DNE
  dirs.foreach { d =>
    val file = new File(d)
    file.mkdirs()
    val db = new File(d + DB_NAME)
    db.createNewFile()
  }

  def makeNetworkSettings(directoryPath: String): List[Setting[_]] =
    List(
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

  lazy val signet = makeNetworkSettings(signetDir)

  lazy val unittest = makeNetworkSettings(unittestDir)

  network match {
    case "mainnet"  => mainnet
    case "testnet3" => testnet3
    case "regtest"  => regtest
    case "signet"   => signet
    case "unittest" => unittest
    case unknown: String =>
      throw new IllegalArgumentException(s"Unknown network=${unknown}")
  }
}
