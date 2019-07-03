lazy val bitcoins = RootProject(file("."))

lazy val publishWebsite = taskKey[Unit]("Publish website")

publishWebsite := Def
  .sequential(
    bitcoins / Compile / unidoc,
    Compile / docusaurusPublishGhpages
  )
  .value

name := "bitcoin-s-docs"

publish / skip := true

////////
/// Mdoc

// Without this we get errors on bad links,
// but those links are to other parts of
// the website, AKA false errors.
// See this issue: https://github.com/scalameta/mdoc/issues/94
mdocExtraArguments := List("--no-link-hygiene")

mdocVariables := Map(
  "STABLE_VERSION" -> previousStableVersion.value.get,
  "UNSTABLE_VERSION" -> version.value
)

enablePlugins(MdocPlugin, DocusaurusPlugin, BuildInfoPlugin)
buildInfoKeys := Seq[BuildInfoKey](mdocVariables, mdocExtraArguments)
buildInfoPackage := "org.bitcoins.docs"

// Mdoc end
///////

libraryDependencies ++= Deps.docs
