import com.ambiata.promulgate.project.ProjectPlugin.promulgate
import sbt._
import Keys._

object build extends Build {
  type Sett = Def.Setting[_]

  val pirate = Project(
    id = "pirate"
  , base = file(".")
  , settings = Defaults.coreDefaultSettings ++ Seq[Sett](
      name := "pirate"
    , organization := "io.mth"
    , version in ThisBuild := "1.0"
    , scalaVersion := "2.11.2"
    , crossScalaVersions := Seq("2.10.4", scalaVersion.value)
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      , "-feature"
      , "-language:_"
      , "-Ywarn-value-discard"
      , "-Xlint"
      , "-Xfatal-warnings"
      ) ++ (if (scalaBinaryVersion.value != "2.10") Seq("-Ywarn-unused-import") else Seq())
     , scalacOptions in (Compile,console) := Seq("-language:_", "-feature")
     , scalacOptions in (Test,console) := Seq("-language:_", "-feature")
    , libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.1.0"
      , "org.scalaz" %% "scalaz-effect" % "7.1.0"
      , "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.0"
      , "org.specs2" %% "specs2-core" % "2.4" % "test"
      , "org.specs2" %% "specs2-scalacheck" % "2.4" % "test"
      ) ++ (
        if (scalaVersion.value.contains("2.10")) Seq("com.chuusai"  % s"shapeless_${scalaVersion.value}" % "2.0.0")
        else                                     Seq("com.chuusai" %% s"shapeless"                       % "2.0.0")
      )
    ) ++ promulgate.library(s"io.mth.pirate", "ambiata-oss")
  )
}
