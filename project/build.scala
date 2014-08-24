import sbt._
import Keys._

object build extends Build {
  type Sett = Def.Setting[_]

  val pirate = Project(
    id = "pirate"
  , base = file(".")
  , settings = Defaults.coreDefaultSettings ++ publishSettings ++ Seq[Sett](
      name := "pirate"
    , organization := "io.mth"
    , version := "1.0-M1"
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
      )
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
    )
  )

  lazy val publishSettings = Seq(
    publishMavenStyle := true
    , publishArtifact in Test := false
    , pomIncludeRepository := { _ => false }
    , licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/BSD-3-Clause"))
    , homepage := Some(url("https://github.com/markhibberd/pirate"))
    , publishTo <<= version.apply(v => {
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("oss snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("oss releases"  at nexus + "service/local/staging/deploy/maven2")
    })

    , pomExtra := (
      <scm>
        <url>git@github.com:markhibberd/pirate.git</url>
        <connection>scm:git:git@github.com:markhibberd/pirate.git</connection>
      </scm>
      <developers>
        <developer>
          <id>mth</id>
          <name>Mark Hibberd</name>
          <url>http://mth.io</url>
        </developer>
      </developers>
    )
  )
}
