import Deps._

name := "bitcoin-s-cli"

libraryDependencies ++= Deps.cli

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)
