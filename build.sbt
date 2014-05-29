
name := "abc-bank-scala"

version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
	"org.scalatest" % "scalatest_2.11" % "2.1.6" % "test",
	"joda-time" % "joda-time" % "2.3",
	"org.joda" % "joda-convert" % "1.6"
)

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)