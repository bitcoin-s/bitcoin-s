name := "bitcoin-s-cli"

libraryDependencies ++= Deps.cli

publish / skip := true

graalVMNativeImageOptions += "-H:EnableURLProtocols=http"

enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)
