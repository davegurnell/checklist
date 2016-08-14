organization   := "com.davegurnell"
name           := "checklist"
version        := "0.0.1"
scalaVersion   := "2.11.8"
scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  compilerPlugin("org.spire-math"  %% "kind-projector" % "0.6.3"),
  compilerPlugin("org.scalamacros" %% "paradise"       % "2.1.0" cross CrossVersion.full)
)

libraryDependencies ++= Seq(
  "org.scala-lang"              % "scala-reflect"            % scalaVersion.value,
  "org.typelevel"              %% "cats"                     % "0.7.0-SNAPSHOT",
  // "org.scala-lang.modules"     %% "scala-parser-combinators" % "1.0.1",
  "com.github.julien-truffaut" %% "monocle-core"             % "1.2.2",
  "com.github.julien-truffaut" %% "monocle-macro"            % "1.2.2"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % Test
)
