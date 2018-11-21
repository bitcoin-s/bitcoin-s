cancelable in Global := true

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
      rpc,
      bench
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

lazy val bench = project
  .in(file("bench"))
  .enablePlugins()
  .settings(assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = true))
  .settings(libraryDependencies ++= Deps.bench)
  .dependsOn(core)

publishArtifact in root := false
