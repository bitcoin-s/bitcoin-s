name := "bitcoin-s-core-gen"

libraryDependencies ++= Deps.coreGen

publishArtifact in Compile := false

publishArtifact in Test := true
