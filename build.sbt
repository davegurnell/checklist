organization  in ThisBuild := "com.davegurnell"
name          in ThisBuild := "checklist"
version       in ThisBuild := "0.2.1"

scalaVersion       in ThisBuild := "2.12.3"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.3")
scalacOptions      in ThisBuild ++= scalacVersionOptions((scalaVersion in Compile).value.split('.').dropRight(1).mkString("."))

licenses in Global += ("Apache-2.0", url("http://apache.org/licenses/LICENSE-2.0"))
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

enablePlugins(ScalaJSPlugin)

lazy val checklist = crossProject.
  crossType(CrossType.Pure).
  settings(
    scalacOptions     ++= Seq(
      "-deprecation",
      "-feature",
      "-Xfatal-warnings"
    ),
    libraryDependencies ++= Seq(
      compilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.3"),
      compilerPlugin("org.scalamacros" %% "paradise"       % "2.1.0" cross CrossVersion.full)
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang"               % "scala-reflect"  % scalaVersion.value % Provided,
      "com.chuusai"                %%% "shapeless"      % "2.3.2",
      "org.typelevel"              %%% "cats-core"      % "0.9.0",
      "com.github.julien-truffaut" %%% "monocle-core"   % "1.4.0",
      "com.github.julien-truffaut" %%% "monocle-macro"  % "1.4.0",
      "org.scalatest"              %%% "scalatest"      % "3.0.0" % Test
    )
  )

lazy val checklistJVM = checklist.jvm
lazy val checklistJS  = checklist.js

lazy val root = project.in(file(".")).
  aggregate(checklistJS, checklistJVM).
  settings(
    publishArtifact := false,
    publish         := {},
    publishLocal    := {},
    skip in publish := true
  )

 lazy val scalacVersionOptions =
    Map(
      "2.12" -> Seq(
        "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
        "-encoding", "utf-8",                // Specify character encoding used by source files.
        "-explaintypes",                     // Explain type errors in more detail.
        "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
        "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
        "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
        "-language:higherKinds",             // Allow higher-kinded types
        "-language:implicitConversions",     // Allow definition of implicit functions called views
        "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
        "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
        "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
        "-Xfuture",                          // Turn on future language features.
        "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
        "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
        "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
        "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
        "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
        "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
        "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
        "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
        "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
        "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
        "-Xlint:option-implicit",            // Option.apply used implicit view.
        "-Xlint:package-object-classes",     // Class or object defined in package object.
        "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
        "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
        "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
        "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
        "-Xlint:unsound-match",              // Pattern match may not be typesafe.
        "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
        "-Ypartial-unification",             // Enable partial unification in type constructor inference
        "-Ywarn-dead-code",                  // Warn when dead code is identified.
        "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
        "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
        "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
        "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
        "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
        "-Ywarn-numeric-widen",              // Warn when numerics are widened.
        "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
        "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
      ),
      "2.11" -> Seq(
        "-encoding", "UTF-8",
        "-feature",
        "-language:existentials",
        "-language:higherKinds",
        "-language:implicitConversions",
        "-language:experimental.macros",
        "-unchecked",
        "-Xlint",
        "-Yno-adapted-args",
        "-Ywarn-dead-code",
        "-Ywarn-value-discard",
        "-Xmax-classfile-name", "128",
        "-Xfatal-warnings"
      )
    )
