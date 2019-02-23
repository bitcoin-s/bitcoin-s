import sbt._
import Keys._ 
object BitcoinSSpvNodeBuild extends Build {

  val appName = "bitcoins-spv-node"
  val appV = "0.0.1" 
  val scalaV = "2.11.7"
  val organization = "org.bitcoins.node"
  val slf4jV = "1.7.5"
  val logbackV = "1.0.13"
  val akkaV = "2.4.7"
  val slickV = "3.1.1" 
/*  val appDependencies = Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.0",
    "com.typesafe.akka" %% "akka-actor" % akkaV withSources() withJavadoc(), 
    "com.typesafe.akka" %% "akka-testkit" % akkaV withSources() withJavadoc(), 
    "ch.qos.logback" % "logback-classic" % logbackV, 
    "joda-time" % "joda-time" % "2.9.4",
    ("com.typesafe.akka" %% "akka-slf4j" % akkaV withSources() withJavadoc()).exclude("org.slf4j", "slf4j-api"), 
    "com.typesafe.slick" %% "slick" % slickV withSources() withJavadoc(),
    "com.typesafe.slick" %% "slick-hikaricp" % "3.1.1",
    "org.postgresql" % "postgresql" % "9.4.1210" 
  )*/
  
  lazy val root = Project(appName, file(".")).settings(
    version := appV,
    scalaVersion := scalaV,
    resolvers += Resolver.sonatypeRepo("releases"),  
    libraryDependencies ++= appDependencies,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    mainClass := Some("org.bitcoins.node.Main"),
    parallelExecution in Test := false,
    //hints for testOptions config here: http://stackoverflow.com/questions/7237824/how-can-i-get-complete-stacktraces-for-exceptions-thrown-in-tests-when-using-sbt 
    testOptions in Test += Tests.Argument("-oF")
  )
} 
