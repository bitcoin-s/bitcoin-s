name := "bitcoin-s-cli"

libraryDependencies ++= Deps.cli(scalaVersion.value)

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin, NativeImagePlugin)
