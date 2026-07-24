lazy val publishWebsite = taskKey[Unit]("Publish website")

// website/ is still classic Docusaurus v1 (see website/package.json),
// but sbt-mdoc 2.9.1 changed DocusaurusPlugin's default to V3, which
// runs `yarn deploy` instead of `yarn publish-gh-pages` and breaks
// docusaurusPublishGhpages. Pin back to V1 until the site is migrated.
docusaurusVersion := DocusaurusVersion.V1

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

Test / bloopGenerate := sbt.Value(None)
Compile / bloopGenerate := sbt.Value(None)

//https://stackoverflow.com/questions/26940253/in-sbt-how-do-you-override-scalacoptions-for-console-in-all-configurations
Compile / scalacOptions ~= (_.filterNot(s => s == "-Xfatal-warnings"))
