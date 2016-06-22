name := """LearnShapeless"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.1",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
  "commons-io" % "commons-io" % "2.3"
)

scalacOptions += "-feature"

initialCommands in console := "import shapeless._"
initialCommands in console in Test := "import shapeless._"
