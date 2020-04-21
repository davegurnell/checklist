organization in ThisBuild := "com.davegurnell"
version in ThisBuild := "0.5.1"

scalaVersion in ThisBuild := "2.13.1"
crossScalaVersions in ThisBuild := Seq("2.12.11", "2.13.1")
scalacOptions in ThisBuild ++= scalacVersionOptions((scalaVersion in Compile).value.split('.').dropRight(1).mkString("."))

enablePlugins(ScalaJSPlugin)

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(sonatypeSettings("checklist-core"))
  .settings(
    libraryDependencies += compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v <= 12 =>
          Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
        case _ =>
          Nil
      }
    },
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      "org.typelevel" %%% "cats-core" % "2.1.1",
      "com.github.julien-truffaut" %%% "monocle-core" % "2.0.4",
      "com.github.julien-truffaut" %%% "monocle-macro" % "2.0.4",
      "org.typelevel" %%% "cats-laws" % "2.1.1" % Test,
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test,
      "org.scalatestplus" %%% "scalacheck-1-14" % "3.1.1.1" % Test,
      "org.scalatest" %%% "scalatest" % "3.1.1" % Test,
      "org.typelevel" %%% "discipline-core" % "1.0.2" % Test,
      "org.typelevel" %%% "discipline-scalatest" % "1.0.1" % Test
    )
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val refinement = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("refinement"))
  .dependsOn(core)
  .settings(sonatypeSettings("checklist-refinement"))
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai" %%% "shapeless" % "2.3.3",
      "org.scalatest" %%% "scalatest" % "3.1.1" % Test
    )
  )

lazy val refinementJVM = refinement.jvm
lazy val refinementJS = refinement.js

lazy val root = project
  .in(file("."))
  .aggregate(coreJS, coreJVM, refinementJS, refinementJVM)
  .settings(disableSonatypeSettings)

def scalacVersionOptions(scalaVersion: String) =
  scalaVersion match {
    case "2.11" =>
      Seq(
        "-deprecation",
        "-encoding",
        "UTF-8",
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
        "-Ypartial-unification",
        "-Xmax-classfile-name",
        "128",
        "-Xfatal-warnings"
      )

    case "2.12" =>
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-explaintypes", // Explain type errors in more detail.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
        "-language:experimental.macros", // Allow macro definition (besides implementation and application)
        "-language:higherKinds", // Allow higher-kinded types
        "-language:implicitConversions", // Allow definition of implicit functions called views
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
        "-Xfatal-warnings", // Fail the compilation if there are any warnings.
        "-Xfuture", // Turn on future language features.
        "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
        "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
        "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
        "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
        "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
        "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
        "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
        "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
        "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
        "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
        "-Xlint:option-implicit", // Option.apply used implicit view.
        "-Xlint:package-object-classes", // Class or object defined in package object.
        "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
        "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
        "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
        "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
        "-Xlint:unsound-match", // Pattern match may not be typesafe.
        "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
        "-Ypartial-unification", // Enable partial unification in type constructor inference
        "-Ywarn-dead-code", // Warn when dead code is identified.
        "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
        "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
        "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
        "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
        "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
        "-Ywarn-numeric-widen", // Warn when numerics are widened.
        "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
        "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
      )

    case "2.13" =>
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-explaintypes", // Explain type errors in more detail.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
        "-language:experimental.macros", // Allow macro definition (besides implementation and application)
        "-language:higherKinds", // Allow higher-kinded types
        "-language:implicitConversions", // Allow definition of implicit functions called views
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
        "-Xfatal-warnings", // Fail the compilation if there are any warnings.
        "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
        "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
        "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
        "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
        "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
        "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
        "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
        "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
        "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
        "-Xlint:option-implicit", // Option.apply used implicit view.
        "-Xlint:package-object-classes", // Class or object defined in package object.
        "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
        "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
        "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
        "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
        "-Ymacro-annotations", // EmableEnable macro annotations
        "-Ywarn-dead-code", // Warn when dead code is identified.
        "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
        "-Ywarn-numeric-widen", // Warn when numerics are widened.
        "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
        "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
      )
  }

def sonatypeSettings(libraryName: String) =
  Seq(
    name := libraryName,
    publishTo := sonatypePublishTo.value,
    publishMavenStyle := true,
    licenses += ("Apache-2.0", url("http://apache.org/licenses/LICENSE-2.0")),
    pomExtra := {
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
  )

def disableSonatypeSettings =
  Seq(
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    skip in publish := true
  )
