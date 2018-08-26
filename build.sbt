

lazy val compilerOpts =
  List("-unchecked", "-deprecation", "-feature", "-Xmax-classfile-name", "128")

lazy val commonSettings = List(
  scalacOptions := compilerOpts,
  assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)
)
lazy val root = project
    .in(file("."))
    .aggregate(
      secp256k1jni,
      core,
      coreGen,
      coreTest,
      zmq,
      rpc
    )
    .settings(commonSettings: _*)

lazy val secp256k1jni = project
  .in(file("secp256k1jni"))
  .settings(commonSettings: _*)
  .enablePlugins()

lazy val core = project
  .in(file("core"))
  .enablePlugins()
  .settings(commonSettings: _*)
  .dependsOn(
    secp256k1jni
  )

lazy val coreGen = project
  .in(file("core-gen"))
  .enablePlugins()
  .settings(commonSettings: _*)
  .dependsOn(
    core
  )

lazy val coreTest = project
  .in(file("core-test"))
  .enablePlugins()
  .settings(commonSettings: _*)
  .dependsOn(
    core,
    coreGen % "test->test"
  )

lazy val zmq = project
  .in(file("zmq"))
  .enablePlugins()
  .settings(commonSettings: _*)
  .dependsOn(
    core
  )

lazy val rpc = project
  .in(file("rpc"))
  .enablePlugins()
  .settings(commonSettings: _*)
  .dependsOn(
    core,
    coreGen % "test->test"
  ).settings(
    testOptions in Test += Tests.Argument("-oF")
  )

publishArtifact in root := false
