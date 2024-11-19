ThisBuild / organization := "com.davegurnell"
ThisBuild / version := "0.7.0"

val scala3Version = "3.5.1"
val scala2Version = "2.13.14"
ThisBuild / scalaVersion := scala2Version
ThisBuild / crossScalaVersions := Seq(scala2Version, scala3Version)
ThisBuild / scalacOptions ++= scalacVersionOptions((Compile / scalaVersion).value.split('.').dropRight(1).mkString("."))

enablePlugins(ScalaJSPlugin)

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
//import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(sonatypeSettings("checklist-core"))
  .settings(
    libraryDependencies ++= {
      if (tlIsScala3.value) Seq.empty
      else
        Seq(
          compilerPlugin(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full))
        )
    },
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"            % "2.12.0",
      "dev.optics" %%% "monocle-core"            % "3.3.0",
      "dev.optics" %%% "monocle-macro"           % "3.3.0",
      "org.typelevel" %%% "cats-laws"            % "2.12.0"   % Test,
      "org.scalatestplus" %%% "scalacheck-1-18"  % "3.2.19.0" % Test,
      "org.scalatest" %%% "scalatest"            % "3.2.19"   % Test,
      "org.typelevel" %%% "discipline-core"      % "1.7.0"    % Test,
      "org.typelevel" %%% "discipline-scalatest" % "2.3.0"    % Test
    )
  )

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val refinement = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("refinement"))
  .dependsOn(core)
  .settings(sonatypeSettings("checklist-refinement"))
  .settings(
    libraryDependencies ++= {
      if (tlIsScala3.value) Seq()
      else
        Seq(
          "com.chuusai" %%% "shapeless"   % "2.3.3",
          "org.scalatest" %%% "scalatest" % "3.2.19" % Test
        )
    },
    publishArtifact := !tlIsScala3.value,
    publish / skip := tlIsScala3.value
  )

lazy val refinementJVM = refinement.jvm
lazy val refinementJS  = refinement.js

lazy val root = project
  .in(file("."))
  .aggregate(coreJS, coreJVM, refinementJS, refinementJVM)
  .settings(disableSonatypeSettings)

def scalacVersionOptions(scalaVersion: String) =
  scalaVersion match {
    case "2.13" =>
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-Xsource:3",
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

    case "3.5" =>
      Seq(
        "-Xkind-projector",
        "-rewrite",
        "-source",
        "3.4-migration",
        "-language:implicitConversions,higherKinds,postfixOps",
        "-Wunused:all",
        "-explain"
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
    publish / skip := true
  )
