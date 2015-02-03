scalacOptions += "-deprecation"

resolvers += "Era7 maven releases" at "http://releases.era7.com.s3.amazonaws.com"

resolvers += Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1")

addSbtPlugin("com.ambiata" % "promulgate" % "0.11.0-20141014013725-80c129f")
