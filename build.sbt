organization  in ThisBuild := "com.davegurnell"
name          in ThisBuild := "checklist"
version       in ThisBuild := "0.1.0"

scalaVersion       := "2.12.1"
crossScalaVersions := Seq("2.11.8", "2.12.1")

scalacOptions ++= Seq("-deprecation", "-feature")

enablePlugins(ScalaJSPlugin)

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  compilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.3"),
  compilerPlugin("org.scalamacros" %% "paradise"       % "2.1.0" cross CrossVersion.full)
)

libraryDependencies ++= Seq(
  "org.scala-lang"               % "scala-reflect" % scalaVersion.value,
  "org.typelevel"              %%% "cats-core"     % "0.9.0",
  "com.github.julien-truffaut" %%% "monocle-core"  % "1.4.0",
  "com.github.julien-truffaut" %%% "monocle-macro" % "1.4.0",
  "org.scalatest"              %%% "scalatest"     % "3.0.0" % Test
)

pomExtra in Global := {
  <url>https://github.com/davegurnell/checklist</url>
  <scm>
    <connection>scm:git:github.com/davegurnell/checklist</connection>
    <developerConnection>scm:git:git@github.com:davegurnell/checklist</developerConnection>
    <url>github.com/davegurnell/checklist</url>
  </scm>
  <developers>
    <developer>
      <id>davegurnell</id>
      <name>Dave Gurnell</name>
      <url>http://twitter.com/davegurnell</url>
    </developer>
  </developers>
}
