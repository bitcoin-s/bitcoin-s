import sbt.Credentials
import sbt.Keys.publishTo
import com.typesafe.sbt.SbtGit.GitKeys._

import scala.util.Properties

cancelable in Global := true

fork in Test := true

//don't allow us to wipe all of our prod databases
flywayClean / aggregate := false
//allow us to wipe our test databases
Test / flywayClean / aggregate := true

lazy val timestamp = new java.util.Date().getTime

lazy val commonCompilerOpts = {
  List(
    "-Xmax-classfile-name",
    "128"
  )
}
//https://docs.scala-lang.org/overviews/compiler-options/index.html
lazy val compilerOpts = Seq(
  "-target:jvm-1.8",
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

lazy val commonSettings = List(
  scalacOptions in Compile := compilerOpts,

  scalacOptions in Test := testCompilerOpts,

  //show full stack trace of failed tests
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),

  //show duration of tests
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),

  assemblyOption in assembly := (assemblyOption in assembly).value
    .copy(includeScala = false),

  bintrayOrganization := Some("bitcoin-s"),

  bintrayRepository := "bitcoin-s-core",

  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),

  resolvers += Resolver.bintrayRepo("bitcoin-s", "bitcoin-s-core"),

  resolvers += Resolver.jcenterRepo,

  resolvers += "oss-jfrog-artifactory-snapshot" at "https://oss.jfrog.org/artifactory/oss-snapshot-local",

  credentials ++= List(
    //for snapshot publishing
    //http://szimano.org/automatic-deployments-to-jfrog-oss-and-bintrayjcentermaven-central-via-travis-ci-from-sbt/
    Credentials(Path.userHome / ".bintray" / ".artifactory"),

    //sbt bintray plugin
    //release publshing
    Credentials(Path.userHome / ".bintray" / ".credentials")
  ),


  publishTo := {
    //preserve the old bintray publishing stuff here
    //we need to preserve it for the publishTo settings below
    val bintrayPublish = publishTo.value
    if (isSnapshot.value) {
      Some("Artifactory Realm" at
        "https://oss.jfrog.org/artifactory/oss-snapshot-local;build.timestamp=" + timestamp)
    } else {
      bintrayPublish
    }
  },
  bintrayReleaseOnPublish := !isSnapshot.value,
  //fix for https://github.com/sbt/sbt/issues/3519
  updateOptions := updateOptions.value.withGigahorse(false),
  git.formattedShaVersion := git.gitHeadCommit.value.map { sha =>
    s"${sha.take(6)}-$timestamp-SNAPSHOT"
  },

  /**
    * Adding Ammonite REPL to test scope, can access both test and compile
    * sources. Docs: http://ammonite.io/#Ammonite-REPL
    * Creates an ad-hoc main file that can be run by doing 
    * test:run (or test:runMain amm if there's multiple main files
    * in scope)
    */
  Test / sourceGenerators += Def.task {
    val file = (Test / sourceManaged).value / "amm.scala"
    IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
    Seq(file)
  }.taskValue,
  // Travis has performance issues on macOS
  Test / parallelExecution := !(Properties.isMac && sys.props
    .get("CI")
    .isDefined)
)

lazy val commonTestSettings = Seq(
  publish / skip := true,
) ++ commonSettings

lazy val commonProdSettings = Seq(
  Test / bloopGenerate := None
) ++ commonSettings

lazy val bitcoins = project
  .in(file("."))
  .aggregate(
    secp256k1jni,
    chain,
    chainTest,
    core,
    coreTest,
    bitcoindRpc,
    bitcoindRpcTest,
    bench,
    eclairRpc,
    eclairRpcTest,
    node,
    nodeTest,
    wallet,
    walletTest,
    testkit,
    zmq,
    doc
  )
  .settings(commonSettings: _*)
  .settings(crossScalaVersions := Nil)
  .settings(libraryDependencies ++= Deps.root)
  .enablePlugins(ScalaUnidocPlugin, GhpagesPlugin, GitVersioning)
  .settings(
    name := "bitcoin-s",
    ScalaUnidoc / siteSubdirName := "latest/api",
    addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, ScalaUnidoc / siteSubdirName),
    gitRemoteRepo := "git@github.com:bitcoin-s/bitcoin-s-core.git"
  )


lazy val secp256k1jni = project
  .in(file("secp256k1jni"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Deps.secp256k1jni,
    unmanagedResourceDirectories in Compile += baseDirectory.value / "natives",
    //since this is not a scala module, we have no code coverage
    //this also doesn't place nice with scoverage, see
    //https://github.com/scoverage/sbt-scoverage/issues/275
    coverageEnabled := false
  )
  .enablePlugins()

lazy val core = project
  .in(file("core"))
  .settings(commonProdSettings: _*)
  .dependsOn(
    secp256k1jni
  ).enablePlugins()

lazy val coreTest = project
  .in(file("core-test"))
  .settings(commonTestSettings: _*)
  .settings(
    name := "bitcoin-s-core-test"
  ).dependsOn(
    core,
    testkit,
  ).enablePlugins()

lazy val chainDbSettings = dbFlywaySettings("chaindb")
lazy val chain = project
  .in(file("chain"))
  .settings(commonSettings: _*)
  .settings(chainDbSettings: _*)
  .settings(
    name := "bitcoin-s-chain",
    libraryDependencies ++= Deps.chain
  ).dependsOn(core, dbCommons)
  .enablePlugins(FlywayPlugin)

lazy val chainTest = project
  .in(file("chain-test"))
  .settings(commonSettings: _*)
  .settings(chainDbSettings: _*)
  .settings(
    skip in publish := true,
    name := "bitcoin-s-chain-test",
    libraryDependencies ++= Deps.chainTest,
    parallelExecution in Test := false
  ).dependsOn(chain, core, testkit, zmq)
  .enablePlugins(FlywayPlugin)


lazy val dbCommons = project
  .in(file("db-commons"))
  .settings(commonSettings: _*)
  .settings(
    name := "bitcoin-s-db-commons",
    libraryDependencies ++= Deps.dbCommons
  ).dependsOn(core)
  .enablePlugins()


lazy val zmq = project
  .in(file("zmq"))
  .settings(commonSettings: _*)
  .settings(
    name := "bitcoin-s-zmq",
    libraryDependencies ++= Deps.bitcoindZmq)
  .dependsOn(
    core
  ).enablePlugins()

lazy val bitcoindRpc = project
  .in(file("bitcoind-rpc"))
  .settings(commonProdSettings: _*)
  .settings(
    name := "bitcoin-s-bitcoind-rpc",
    libraryDependencies ++= Deps.bitcoindRpc)
  .dependsOn(core)
  .enablePlugins()

lazy val bitcoindRpcTest = project
  .in(file("bitcoind-rpc-test"))
  .settings(commonTestSettings: _*)
  .settings(
    libraryDependencies ++= Deps.bitcoindRpcTest,
    name := "bitcoin-s-bitcoind-rpc-test")
  .dependsOn(testkit)
  .enablePlugins()

lazy val bench = project
  .in(file("bench"))

  .settings(commonSettings: _*)
  .settings(assemblyOption in assembly := (assemblyOption in assembly).value
    .copy(includeScala = true))
  .settings(
    libraryDependencies ++= Deps.bench,
    name := "bitcoin-s-bench",
    skip in publish := true
  )
  .dependsOn(core)
  .enablePlugins()

lazy val eclairRpc = project
  .in(file("eclair-rpc"))
  .settings(commonProdSettings: _*)
  .settings(
    name := "bitcoin-s-eclair-rpc",
    libraryDependencies ++= Deps.eclairRpc)
  .dependsOn(
    core,
    bitcoindRpc
  ).enablePlugins()

lazy val eclairRpcTest = project
  .in(file("eclair-rpc-test"))
  .settings(commonTestSettings: _*)
  .settings(libraryDependencies ++= Deps.eclairRpcTest,
    name := "bitcoin-s-eclair-rpc-test",
  )
  .dependsOn(testkit)
  .enablePlugins()

lazy val nodeDbSettings = dbFlywaySettings("nodedb")
lazy val node = {
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
      dbCommons
    ).enablePlugins(FlywayPlugin)
}

lazy val nodeTest = {
  project
    .in(file("node-test"))
    .settings(commonSettings: _*)
    .settings(nodeDbSettings: _*)
    .settings(
      name := "bitcoin-s-node-test",
      libraryDependencies ++= Deps.nodeTest
    ).dependsOn(
    node,
    testkit
  ).enablePlugins(FlywayPlugin)
}

lazy val testkit = project
  .in(file("testkit"))
  .settings(commonProdSettings: _*)
  .dependsOn(
    core,
    bitcoindRpc,
    eclairRpc,
    node
  ).enablePlugins()

lazy val walletDbSettings = dbFlywaySettings("walletdb")
lazy val wallet = project
  .in(file("wallet"))
  .settings(commonSettings: _*)
  .settings(walletDbSettings: _*)
  .settings(
    name := "bitcoin-s-wallet",
    libraryDependencies ++= Deps.wallet
  )
  .dependsOn(core, dbCommons)
  .enablePlugins(FlywayPlugin)

lazy val walletTest = project
  .in(file("wallet-test"))
  .settings(commonSettings: _*)
  .settings(walletDbSettings: _*)
  .settings(
    name := "bitcoin-s-wallet-test",
    libraryDependencies ++= Deps.walletTest,
    skip in publish := true
  )
  .dependsOn(core, testkit, wallet)
  .enablePlugins(FlywayPlugin)

lazy val doc = project
  .in(file("doc"))
  .settings(commonTestSettings: _*)
  .settings(
    name := "bitcoin-s-doc",
    libraryDependencies ++= Deps.doc,
  )
  .dependsOn(
    secp256k1jni,
    core
  )

// Ammonite is invoked through running
// a main class it places in test sources
// for us. This makes it a bit less awkward
// to start the Ammonite shell. Sadly, 
// prepending the project and then doing
// `amm` (e.g. sbt coreTest/amm`) does not 
// work. For that you either have to do 
// `sbt coreTest/test:run` or: 
// sbt
// project coreTest
// amm
addCommandAlias("amm", "test:run")

publishArtifact in bitcoins := false

def dbFlywaySettings(dbName: String): List[Setting[_]] = {
  lazy val DB_HOST = "localhost"
  lazy val DB_NAME = s"${dbName}.sqlite"
  lazy val network = "unittest" //mainnet, testnet3, regtest, unittest

  lazy val mainnetDir = s"${System.getenv("HOME")}/.bitcoin-s/mainnet/"
  lazy val testnetDir = s"${System.getenv("HOME")}/.bitcoin-s/testnet3/"
  lazy val regtestDir = s"${System.getenv("HOME")}/.bitcoin-s/regtest/"
  lazy val unittestDir = s"${System.getenv("HOME")}/.bitcoin-s/unittest/"

  lazy val dirs = List(mainnetDir,testnetDir,regtestDir,unittestDir)

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
    case "mainnet" => mainnet
    case "testnet3" => testnet3
    case "regtest" => regtest
    case "unittest" => unittest
    case unknown: String => throw new IllegalArgumentException(s"Unknown network=${unknown}")
  }
}

publishArtifact in bitcoins := false

previewSite / aggregate := false
previewAuto / aggregate := false
