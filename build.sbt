name := "muddy"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "joda-time" % "joda-time" % "2.3"
)

play.Project.playScalaSettings

scalacOptions += "-feature"
