import com.typesafe.sbt.SbtGit.GitKeys._

import scala.util.Properties

Global / cancelable := true

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
    Test / unmanagedSourceDirectories += baseDirectory.value / "src" / "bench" / "scala",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    Benchmark / parallelExecution := false,
    Benchmark / outputStrategy := Some(StdoutOutput),
    Benchmark / fork := true,
    Benchmark / connectInput := true,
    inConfig(Benchmark)(Defaults.testSettings)
  )
}

lazy val commonJsSettings = {
  Seq(
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
    }
  ) ++ CommonSettings.settings
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
    Compile / npmDependencies ++= Seq(
      "bcrypto" -> "5.4.0"
    )
  )
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .in(file("crypto"))

lazy val cryptoJS = crypto.js
  .settings(Compile / scalacOptions += {
    "-Wconf:cat=unused:site=org\\.bitcoins\\.crypto\\.facade\\..*:silent,cat=w-flag-dead-code:site=org\\.bitcoins\\.crypto\\.facade\\..*:silent"
  })
  .enablePlugins(ScalaJSBundlerPlugin)

lazy val cryptoJVM = crypto.jvm
  .dependsOn(secp256k1jni)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(name := "bitcoin-s-core")
  .settings(libraryDependencies ++= Deps.core.value)
  .settings(CommonSettings.prodSettings: _*)
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .in(file("core"))
  .dependsOn(crypto)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js
  .settings(libraryDependencies ++= Deps.coreJs.value)
  .enablePlugins(ScalaJSBundlerPlugin)

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
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(asyncUtilsJVM, bitcoindRpc)

lazy val lndRpc = project
  .in(file("lnd-rpc"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(asyncUtilsJVM, bitcoindRpc)

lazy val tor = project
  .in(file("tor"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(cryptoJVM)

lazy val torTest = project
  .in(file("tor-test"))
  .settings(CommonSettings.testSettings: _*)
  .dependsOn(tor, testkit)

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
    //coreTestJS,
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
    dlcTest,
    dlcWallet,
    dlcWalletTest,
    dlcOracle,
    dlcOracleTest,
    dlcSuredbitsClient,
    dlcSuredbitsClientTest,
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
    //testkitCoreJS,
    testkit,
    zmq,
    oracleExplorerClient,
    oracleServer,
    oracleServerTest,
    serverRoutes,
    lndRpc,
    lndRpcTest,
    tor,
    torTest,
    scripts
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
    //coreTestJS,
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
    dlcTest,
    dlcWallet,
    dlcWalletTest,
    dlcSuredbitsClient,
    dlcSuredbitsClientTest,
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
    oracleExplorerClient,
    oracleServer,
    oracleServerTest,
    serverRoutes,
    lndRpc,
    lndRpcTest,
    tor,
    torTest,
    scripts
  )
  .settings(CommonSettings.settings: _*)
  // unidoc aggregates Scaladocs for all subprojects into one big doc
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
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
    Compile / unmanagedResourceDirectories += baseDirectory.value / "natives",
    //since this is not a scala module, we have no code coverage
    //this also doesn't place nice with scoverage, see
    //https://github.com/scoverage/sbt-scoverage/issues/275
    coverageEnabled := false
  )

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
  .settings(libraryDependencies ++= Deps.coreTestJVM.value)

lazy val coreTestJS = coreTest.js
  .enablePlugins(ScalaJSBundlerPlugin)

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
  .settings(CommonSettings.testSettings: _*)
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
    zmq,
    dlcWallet
  )
  .enablePlugins(JavaAppPackaging, DockerPlugin)

lazy val appServerTest = project
  .in(file("app/server-test"))
  .settings(CommonSettings.testSettings: _*)
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

lazy val dlcSuredbitsClient = project
  .in(file("app/dlc-suredbits-client"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(eclairRpc, wallet)

lazy val dlcSuredbitsClientTest = project
  .in(file("app/dlc-suredbits-client-test"))
  .settings(CommonSettings.testSettings: _*)
  .dependsOn(
    dlcSuredbitsClient,
    testkit
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

def isCI = {
  Properties
    .envOrNone("CI")
    .isDefined
}

lazy val bitcoindRpcTest = project
  .in(file("bitcoind-rpc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(name := "bitcoin-s-bitcoind-rpc-test",
            libraryDependencies ++= Deps.bitcoindRpcTest.value,
            parallelExecution := !(isCI && Properties.isMac))
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val bench = project
  .in(file("bench"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    libraryDependencies ++= Deps.bench,
    name := "bitcoin-s-bench",
    publish / skip := true
  )
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val eclairRpcTest = project
  .in(file("eclair-rpc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    libraryDependencies ++= Deps.eclairRpcTest.value,
    name := "bitcoin-s-eclair-rpc-test",
    parallelExecution := !(isCI && Properties.isMac)
  )
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val lndRpcTest = project
  .in(file("lnd-rpc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    libraryDependencies ++= Deps.eclairRpcTest.value,
    name := "bitcoin-s-lnd-rpc-test",
    parallelExecution := !(isCI && Properties.isMac)
  )
  .dependsOn(coreJVM % testAndCompile, testkit, lndRpc)

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
    lndRpc,
    node,
    wallet,
    zmq,
    testkitCoreJVM,
    dlcOracle,
    dlcWallet
  )

lazy val docs = project
  .in(file("bitcoin-s-docs")) // important: it must not be docs/
  .settings(CommonSettings.testSettings: _*)
  .settings(libraryDependencies ++= Deps.docs.value)
  .settings(
    name := "bitcoin-s-docs",
    moduleName := name.value,
    //removes scalajs projects from unidoc, see
    //https://github.com/bitcoin-s/bitcoin-s/issues/2740
    ScalaUnidoc / unidoc / unidocProjectFilter := {
      inAnyProject -- inProjects(jsProjects: _*)
    },
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite
      .dependsOn(Compile / unidoc)
      .value,
    docusaurusPublishGhpages := docusaurusPublishGhpages
      .dependsOn(Compile / unidoc)
      .value
  )
  .enablePlugins(MdocPlugin,
                 DocusaurusPlugin,
                 ScalaUnidocPlugin,
                 BuildInfoPlugin)
  .dependsOn(
    appCommons,
    asyncUtilsJVM,
    appServer,
    bitcoindRpc,
    chain,
    cli,
    cryptoJVM,
    coreJVM,
    dbCommons,
    oracleExplorerClient,
    feeProvider,
    dlcOracle,
    eclairRpc,
    keyManager,
    node,
    secp256k1jni,
    testkitCoreJVM,
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
  .dependsOn(coreJVM, appCommons, dbCommons, keyManager, asyncUtilsJVM)
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

lazy val oracleExplorerClient = project
  .in(file("oracle-explorer-client"))
  .settings(CommonSettings.settings: _*)
  .settings(
    name := "bitcoin-s-oracle-explorer-client",
    libraryDependencies ++= Deps.oracleExplorerClient
  )
  .dependsOn(coreJVM, appCommons, testkit % "test->test")

lazy val scripts = project
  .in(file("app/scripts"))
  .settings(CommonSettings.settings: _*)
  .settings(
    name := "bitcoin-s-scripts",
    publishArtifact := false //do not want to publish our scripts
  )
  .dependsOn(appServer)
  .enablePlugins(JavaAppPackaging)

lazy val dlcTest = project
  .in(file("dlc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-test",
    libraryDependencies ++= Deps.dlcTest
  )
  .dependsOn(
    coreJVM % testAndCompile,
    testkitCoreJVM
  )

lazy val dlcWallet = project
  .in(file("dlc-wallet"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-wallet",
    libraryDependencies ++= Deps.dlcWallet
  )
  .dependsOn(wallet)

lazy val dlcWalletTest = project
  .in(file("dlc-wallet-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-wallet-test",
    libraryDependencies ++= Deps.dlcWalletTest
  )
  .dependsOn(coreJVM % testAndCompile, dlcWallet, testkit, dlcTest)

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
