lazy val publishWebsite = taskKey[Unit]("Publish website")

publishWebsite := Def
  .sequential(
    Compile / unidoc,
    Compile / docusaurusPublishGhpages
  )
  .value

publish / skip := true

////////
/// Mdoc

// Without this we get errors on bad links,
// but those links are to other parts of
// the website, AKA false errors.
// See this issue: https://github.com/scalameta/mdoc/issues/94
mdocExtraArguments := List("--no-link-hygiene")

// these variables gets passed to mdoc, and can be read
// from there
mdocVariables ++= Map(
  "STABLE_VERSION" -> CommonSettings.previousStableVersion,
  "UNSTABLE_VERSION" -> version.value
)

// this expoes the values below as typed values in Scala sources
enablePlugins(BuildInfoPlugin)
buildInfoKeys := Seq[BuildInfoKey](mdocVariables, mdocExtraArguments)
buildInfoPackage := "org.bitcoins.docs"

// Mdoc end
///////

Test / bloopGenerate := None
Compile / bloopGenerate := None

//https://stackoverflow.com/questions/26940253/in-sbt-how-do-you-override-scalacoptions-for-console-in-all-configurations
Compile / scalacOptions ~= (_.filterNot(s => s == "-Xfatal-warnings"))
