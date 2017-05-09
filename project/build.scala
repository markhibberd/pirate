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
    , scalaVersion := "2.11.7"
    , crossScalaVersions := Seq("2.10.5", scalaVersion.value)
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
    , scalacOptions in Test := Seq("-Yrangepos")
    , libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.2.6"
      , "org.scalaz" %% "scalaz-effect" % "7.2.6"
      , "org.scalaz" %% "scalaz-scalacheck-binding" % "7.2.6" % "test"
      , "org.specs2" %% "specs2-core" % "3.8.9" % "test"
      , "org.specs2" %% "specs2-scalacheck" % "3.8.9" % "test"
      ) ++ (
        if (scalaVersion.value.contains("2.10")) Seq("com.chuusai"  % s"shapeless_${scalaVersion.value}" % "2.0.0")
        else                                     Seq("com.chuusai" %% s"shapeless"                       % "2.0.0")
      )
    ) ++ promulgate.library(s"io.mth.pirate", "ambiata-oss")
  )
}
