import sbt._
import Keys._

object build extends Build {
  type Sett = Project.Setting[_]

  override lazy val settings = super.settings ++
        Seq(resolvers := Seq(
          "mth.io snapshots" at "http://repo.mth.io/snapshots"
        , "mth.io releases" at "http://repo.mth.io/releases"
        , "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
        , "releases" at "http://oss.sonatype.org/content/repositories/releases"
        ))

  lazy val pom =
    pomExtra := (
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


  lazy val publishSetting =
    publishTo <<= version.apply(v => {
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("oss snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("oss releases"  at nexus + "service/local/staging/deploy/maven2")
    })

  val pirate = Project(
    id = "pirate"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "pirate"
    , organization := "io.mth"
    , version := "0.9"
    , scalaVersion := "2.11.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , pom
    , publishMavenStyle := true
    , publishArtifact in Test := false
    , pomIncludeRepository := { _ => false }
    , sourceDirectory in Compile <<= baseDirectory { _ / "src" }
    , sourceDirectory in Test <<= baseDirectory { _ / "test" }
    , historyPath <<= baseDirectory { b => Some(b / "gen/sbt/.history") }
    , target <<= baseDirectory { _ / "gen/sbt/target" }
    , testOptions in Test += Tests.Setup(() => System.setProperty("specs2.outDir", "gen/sbt/target/specs2-reports"))
    , publishSetting
    , licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/BSD-3-Clause"))
    , homepage := Some(url("http://pirate.mth.io"))
    , libraryDependencies ++= Seq(
        ("org.scalaz" %% "scalaz-core" % "7.1.0")
      , ("org.specs2" %% "specs2" % "2.4.6" % "test")
      , ("org.specs2" %% "specs2-scalacheck" % "2.4.6" % "test")
      , ("org.scalacheck" %% "scalacheck" % "1.11.1" % "test")
      )
    )
  )
}
