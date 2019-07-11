name := "bitcoin-s-cli"

libraryDependencies ++= Deps.cli

publish / skip := false

graalVMNativeImageOptions += "-H:EnableURLProtocols=http"

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)
