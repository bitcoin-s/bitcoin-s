// these two imports are needed for sbt syntax to work
import com.typesafe.sbt.SbtNativePackager.Docker
import com.typesafe.sbt.SbtNativePackager.autoImport.packageName

import java.nio.file.Paths
import com.typesafe.sbt.packager.Keys.{
  daemonUser,
  daemonUserUid,
  dockerAlias,
  dockerAliases,
  dockerCommands,
  dockerExposedVolumes,
  dockerRepository,
  dockerUpdateLatest,
  maintainer
}
import com.typesafe.sbt.packager.archetypes.jlink.JlinkPlugin.autoImport.JlinkIgnore
import com.typesafe.sbt.packager.docker.{Cmd, DockerChmodType}
import com.typesafe.sbt.packager.docker.DockerPlugin.autoImport.{
  dockerAdditionalPermissions,
  dockerBaseImage
}
import sbt._
import sbt.Keys._
import sbtprotoc.ProtocPlugin.autoImport.PB
import sbtassembly.AssemblyKeys._
import sbtdynver.DynVer

import scala.sys.process.Process
import scala.util.Properties

object CommonSettings {

  val previousStableVersion: String = "1.9.7"

  private def isCI = {
    Properties
      .envOrNone("CI")
      .isDefined
  }

  lazy val settings: Seq[Setting[_]] = Vector(
    organization := "org.bitcoin-s",
    homepage := Some(url("https://bitcoin-s.org")),
    maintainer := "Chris Stewart <stewart.chris1234@gmail.com>",
    developers := List(
      Developer(
        "christewart",
        "Chris Stewart",
        "stewart.chris1234@gmail.com",
        url("https://twitter.com/Chris_Stewart_5")
      )
    ),
    Compile / scalacOptions ++= compilerOpts(scalaVersion = scalaVersion.value),
    Test / scalacOptions ++= testCompilerOpts(scalaVersion =
      scalaVersion.value),
    //remove annoying import unused things in the scala console
    //https://stackoverflow.com/questions/26940253/in-sbt-how-do-you-override-scalacoptions-for-console-in-all-configurations
    Compile / console / scalacOptions ~= (_ filterNot (s =>
      s == "-Ywarn-unused-import"
        || s == "-Ywarn-unused"
        || s == "-Xfatal-warnings"
        //for 2.13 -- they use different compiler opts
        || s == "-Xlint:unused")),
    //we don't want -Xfatal-warnings for publishing with publish/publishLocal either
    Compile / doc / scalacOptions ~= (_ filterNot (s =>
      s == "-Xfatal-warnings")),
    //silence all scaladoc warnings generated from invalid syntax
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/3232
    Compile / doc / scalacOptions ++= Vector(s"-Wconf:any:ws"),
    Test / console / scalacOptions ++= (Compile / console / scalacOptions).value,
    Test / scalacOptions ++= testCompilerOpts(scalaVersion.value),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    assembly / test := {}
  )

  lazy val jvmSettings: Seq[Setting[_]] = List(
    ////
    // scaladoc settings
    Compile / doc / scalacOptions ++= List(
      "-doc-title",
      "Bitcoin-S",
      "-doc-version",
      version.value
    ),
    // Set apiURL to define the base URL for the Scaladocs for our library.
    // This will enable clients of our library to automatically link against
    // the API documentation using autoAPIMappings.
    apiURL := homepage.value.map(_.toString + "/api").map(url(_)),
    // scaladoc settings end
    ////
    Compile / compile / javacOptions ++= {
      //https://github.com/eclipse/jetty.project/issues/3244#issuecomment-495322586
      Seq("--release", "8")
    }
  )

  lazy val jlinkModules = Seq(
    "jdk.crypto.ec"
  )

  //these are java modules we do not need
  //our artifacts do not use java.desktop
  //there may be others we don't need
  //but this is the most obvious and reduces
  //artifact size by 15MB
  //do 'show jlinkModules' in the module
  //to see what ones are used
  lazy val rmJlinkModules = Seq(
    "java.desktop"
  )

  lazy val rmCliJlinkModules = {
    rmJlinkModules ++ Vector(
      "java.logging",
      "java.naming",
      "java.sql",
      "java.xml",
      "jdk.unsupported"
    )
  }

  lazy val jlinkOptions = Seq(
    "--no-header-files",
    "--no-man-pages",
    "--compress=2"
  )

  private val commonCompilerOpts = {
    List(
      //https://stackoverflow.com/a/43103038/967713
      "-release",
      "8"
    )
  }

  /** Linting options for scalac */
  private val scala2_13CompilerLinting = {
    Seq(
      "-Xlint:unused",
      "-Xlint:adapted-args",
      "-Xlint:nullary-unit",
      "-Xlint:inaccessible",
      "-Xlint:infer-any",
      "-Xlint:missing-interpolator",
      "-Xlint:eta-sam"
    )
  }

  /** Compiler options for source code */
  private val scala2_13SourceCompilerOpts = {
    Seq("-Xfatal-warnings") ++ scala2_13CompilerLinting
  }

  private val nonScala2_13CompilerOpts = Seq(
    "-Xmax-classfile-name",
    "128",
    "-Ywarn-unused",
    "-Ywarn-unused-import"
  )

  //https://docs.scala-lang.org/overviews/compiler-options/index.html
  def compilerOpts(scalaVersion: String): Seq[String] = {
    Seq(
      "-unchecked",
      "-feature",
      "-deprecation",
      "-Ywarn-dead-code",
      "-Ywarn-value-discard",
      "-Ywarn-unused",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-Ypatmat-exhaust-depth",
      "off"
    ) ++ commonCompilerOpts ++ {
      if (scalaVersion.startsWith("2.13")) {
        scala2_13SourceCompilerOpts
      } else nonScala2_13CompilerOpts
    }
  }

  def testCompilerOpts(scalaVersion: String): Seq[String] = {
    (commonCompilerOpts ++
      //initialization checks: https://docs.scala-lang.org/tutorials/FAQ/initialization-order.html
      Vector("-Xcheckinit") ++
      compilerOpts(scalaVersion))
      .filterNot(_ == "-Xfatal-warnings")
  }

  lazy val testSettings: Seq[Setting[_]] = Seq(
    //show full stack trace (-oF) of failed tests and duration of tests (-oD)
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    Test / logBuffered := false,
    skip / publish := true
  ) ++ settings

  lazy val prodSettings: Seq[Setting[_]] = settings

  lazy val appSettings: Seq[Setting[_]] = prodSettings ++ Vector(
    //gives us the 'universal' directory in build artifacts
    Compile / unmanagedResourceDirectories += baseDirectory.value / "src" / "universal"
  )

  lazy val dockerSettings: Seq[Setting[_]] = {
    Vector(
      //https://sbt-native-packager.readthedocs.io/en/latest/formats/docker.html
      dockerBaseImage := "eclipse-temurin:17",
      dockerRepository := Some("bitcoinscala"),
      Docker / daemonUser := "bitcoin-s",
      //needed for umbrel environment, container uids and host uids must matchup so we can
      //properly write to volumes on the host machine
      //see: https://medium.com/@mccode/understanding-how-uid-and-gid-work-in-docker-containers-c37a01d01cf
      //Docker / daemonUserUid := Some("1000"),
      Docker / packageName := packageName.value,
      Docker / version := version.value,
      //add a default exposed volume of /bitcoin-s so we can always write data here
      dockerExposedVolumes += "/bitcoin-s",
      dockerUpdateLatest := DynVer.isSnapshot
    )
  }

  // See https://softwaremill.com/how-to-build-multi-platform-docker-image-with-sbt-and-docker-buildx/
  lazy val ensureDockerBuildx =
    taskKey[Unit]("Ensure that docker buildx configuration exists")

  lazy val dockerBuildWithBuildx =
    taskKey[Unit]("Build docker images using buildx")

  /** These settings are needed to produce docker images across different chip architectures
    * such as amd64 and arm64
    * @see https://softwaremill.com/how-to-build-multi-platform-docker-image-with-sbt-and-docker-buildx/
    */
  lazy val dockerBuildxSettings = {
    Seq(
      ensureDockerBuildx := {
        if (Process("docker buildx inspect multi-arch-builder").! == 1) {
          Process("docker buildx create --use --name multi-arch-builder",
                  baseDirectory.value).!
        }
      },
      dockerBuildWithBuildx := {
        streams.value.log("Building and pushing image with Buildx")
        dockerAliases.value.foreach { alias =>
          //issue the command below in to the terminal in the same directory that
          //our sbt plugin generates the docker file.
          //if you want to reproduce the docker file, run docker:stage
          //in your sbt terminal and you should find it in target/docker/stage/
          val cmd =
            "docker buildx build --platform=linux/amd64,linux/arm64 --push -t " +
              alias + " ."
          val dockerFileDir =
            baseDirectory.value / "target" / "docker" / "stage"
          Process(cmd, dockerFileDir).!
        }
      },
      Docker / publish := Def
        .sequential(
          Docker / publishLocal,
          ensureDockerBuildx,
          dockerBuildWithBuildx
        )
        .value
    )
  }

  lazy val binariesPath =
    Paths.get(Properties.userHome, ".bitcoin-s", "binaries")

  lazy val cryptoJlinkIgnore = {
    Vector(
      "org.bouncycastle" -> "junit.framework"
    ) //bouncy castle 1.74 requires junit as a transitive dep
  }

  lazy val dbCommonsJlinkIgnore = {
    //we don't use android
    Vector(
      "org.flywaydb.core.api.android" -> "android.content",
      "org.flywaydb.core.internal.logging.android" -> "android.util",
      "org.flywaydb.core.internal.resource.android" -> "android.content.res",
      "org.flywaydb.core.internal.scanner.android" -> "android.content",
      "org.flywaydb.core.internal.scanner.android" -> "android.content.pm",
      "org.flywaydb.core.internal.scanner.android" -> "android.content.res",
      "org.flywaydb.core.internal.scanner.android" -> "dalvik.system",
      //we don't use hibernate
      "com.zaxxer.hikari.hibernate" -> "org.hibernate",
      //we don't ship with support for any aws products
      "org.flywaydb.core.internal.resource.s3" -> "software.amazon.awssdk.awscore.exception",
      "org.flywaydb.core.internal.resource.s3" -> "software.amazon.awssdk.core",
      "org.flywaydb.core.internal.resource.s3" -> "software.amazon.awssdk.services.s3",
      "org.flywaydb.core.internal.resource.s3" -> "software.amazon.awssdk.services.s3.model",
      "org.flywaydb.core.internal.scanner.cloud.s3" -> "software.amazon.awssdk.core.exception",
      "org.flywaydb.core.internal.scanner.cloud.s3" -> "software.amazon.awssdk.services.s3",
      "org.flywaydb.core.internal.scanner.cloud.s3" -> "software.amazon.awssdk.services.s3.model",
      "org.flywaydb.core.api.configuration" -> "software.amazon.awssdk.services.s3",
      //we don't use oracle database products
      "org.flywaydb.core.internal.database.oracle" -> "oracle.jdbc",
      //we don't use jboss
      "org.flywaydb.core.internal.scanner.classpath.jboss" -> "org.jboss.vfs",
      "org.flywaydb.core.internal.logging.log4j2" -> "org.apache.logging.log4j",
      "com.zaxxer.hikari.metrics.micrometer" -> "io.micrometer.core.instrument",
      "com.zaxxer.hikari.pool" -> "io.micrometer.core.instrument",
      "slick.jdbc" -> "javax.xml.bind",
      "com.zaxxer.hikari.metrics.prometheus" -> "io.prometheus.client",
      "com.zaxxer.hikari.util" -> "javassist",
      "com.zaxxer.hikari.util" -> "javassist.bytecode",
      //postgres requires this weird waffle dep
      "waffle.jaas" -> "java.security.acl",
      //no native image support for now
      //https://github.com/xerial/sqlite-jdbc/commit/6f426839c56f3924be6cad8920d9192400a37d5f#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5R117
      "org.sqlite.nativeimage" -> "org.graalvm.nativeimage.hosted"
    )
  }

  lazy val loggingJlinkIgnore = {
    Vector(
      "ch.qos.logback.core.net" -> "javax.mail",
      "ch.qos.logback.core.net" -> "javax.mail.internet",
      "org.apache.log4j.jmx" -> "com.sun.jdmk.comm",
      "ch.qos.logback.classic" -> "jakarta.servlet.http",
      "ch.qos.logback.classic.helpers" -> "jakarta.servlet",
      "ch.qos.logback.classic.helpers" -> "jakarta.servlet.http",
      "ch.qos.logback.classic.selector.servlet" -> "jakarta.servlet",
      "ch.qos.logback.classic.servlet" -> "jakarta.servlet",
      "ch.qos.logback.core.net" -> "jakarta.mail",
      "ch.qos.logback.core.net" -> "jakarta.mail.internet",
      "ch.qos.logback.core.status" -> "jakarta.servlet",
      "ch.qos.logback.core.status" -> "jakarta.servlet.http"
    )
  }

  lazy val oracleServerJlinkIgnore = {
    val oracleServerIgnore = Vector(
      "java.xml" -> "java.activation",
      "com.github.benmanes.caffeine" -> "javax.annotation",
      "com.github.benmanes.caffeine.cache" -> "javax.annotation",
      "com.github.benmanes.caffeine.cache.stats" -> "javax.annotation",
      //optional
      "org.codehaus.janino" -> "org.apache.tools.ant"
    )
      .++(loggingJlinkIgnore)
      .++(dbCommonsJlinkIgnore)
      .++(cryptoJlinkIgnore)
    JlinkIgnore.byPackagePrefix(oracleServerIgnore: _*)
  }

  lazy val appServerJlinkIgnore = {

    val appServerIgnore = loggingJlinkIgnore
      .++(dbCommonsJlinkIgnore)
      .++(cryptoJlinkIgnore)
      .++(
        Vector(
          //https://github.com/janino-compiler/janino/blob/f6bb39d3137ad2e99b41ecc48aaaf8ab2644bd1c/janino/pom.xml#L37
          "org.codehaus.janino" -> "org.apache.tools.ant",
          "com.github.benmanes.caffeine" -> "javax.annotation",
          "com.github.benmanes.caffeine.cache" -> "javax.annotation",
          "com.github.benmanes.caffeine.cache.stats" -> "javax.annotation",
          "monix.execution.misc" -> "scala.tools.nsc"
        ))
    JlinkIgnore.byPackagePrefix(appServerIgnore: _*)
  }

  lazy val cliJlinkIgnore = {
    val cliIgnore = Vector(
      "scala.meta.internal.svm_subs" -> "com.oracle.svm.core.annotate",
      "org.slf4j" -> "org.slf4j.impl"
    ).++(cryptoJlinkIgnore)

    JlinkIgnore.byPackagePrefix(cliIgnore: _*)
  }

  def buildPackageName(packageName: String): String = {
    val osName = getSimpleOSName
    val split = packageName.split("-")
    val versionIdx = split.zipWithIndex.find(_._1.count(_ == '.') > 1).get._2
    val insertedOSName = split.take(versionIdx) ++ Vector(osName)
    if (isRelease) {
      //bitcoin-s-server-linux-1.9.3-1-60bfd603-SNAPSHOT.zip -> bitcoin-s-server-linux-1.9.3.zip
      insertedOSName.mkString("-") ++ "-" ++ split(versionIdx)
    } else {
      //bitcoin-s-server-1.9.2-1-59aaf330-20220616-1614-SNAPSHOT -> bitcoin-s-server-linux-1.9.2-1-59aaf330-20220616-1614-SNAPSHOT
      //bitcoin-s-cli-1.9.2-1-59aaf330-20220616-1614-SNAPSHOT.zip -> bitcoin-s-cli-linux-1.9.2-1-59aaf330-20220616-1614-SNAPSHOT.zip
      (insertedOSName ++ split.drop(versionIdx)).mkString("-")
    }
  }

  /** @see https://github.com/sbt/sbt-dynver#detail */
  def isRelease: Boolean = {
    DynVer.isVersionStable && !DynVer.isSnapshot
  }

  private def getSimpleOSName: String = {
    if (Properties.isWin) {
      "windows"
    } else if (Properties.isMac) {
      "mac"
    } else if (Properties.isLinux) {
      "linux"
    } else {
      "unknown-os"
    }
  }
}
