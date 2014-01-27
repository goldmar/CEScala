name := "CEScala"

version := "1.0"

scalaVersion := "2.10.2"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scala-lang" % "scala-library" % "2.10.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2"

libraryDependencies += "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1"

libraryDependencies += "com.espertech" % "esper" % "4.10.0"

logBuffered := false
