import sbt._
import Keys._

object build extends Build {
  type Sett = Def.Setting[_]

  val pirate = Project(
    id = "pirate"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ publishSettings ++ Seq[Sett](
      name := "pirate"
    , organization := "io.mth"
    , version := "1.0-M1"
    , scalaVersion := "2.10.4"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      , "-feature"
      , "-language:_"
      , "-Ywarn-all"
      , "-Xlint"
      )
    , sourceDirectory in Compile <<= baseDirectory { _ / "src" }
    , sourceDirectory in Test <<= baseDirectory { _ / "test" }
    , historyPath <<= baseDirectory { b => Some(b / "gen/sbt/.history") }
    , target <<= baseDirectory { _ / "gen/sbt/target" }
    , testOptions in Test += Tests.Setup(() => System.setProperty("specs2.outDir", "gen/sbt/target/specs2-reports"))
    , libraryDependencies ++= Seq(
        ("org.scalaz" %% "scalaz-core" % "7.0.6")
      , ("org.specs2" %% "specs2" % "2.3.4" % "test")
      , ("org.specs2" %% "specs2-scalacheck" % "2.3.4" % "test")
      , ("org.scalacheck" %% "scalacheck" % "1.11.1" % "test")
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
