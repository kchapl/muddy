name := "money"

version := "1.0-SNAPSHOT"

resolvers += "org.catch22" at "http://marklister.github.io/product-collections/"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "joda-time" % "joda-time" % "2.3",
  "org.catch22" %% "product-collections" % "0.0.4.2-SNAPSHOT"
)

play.Project.playScalaSettings
