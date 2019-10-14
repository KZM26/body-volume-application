organization  := "za.uct.bme.mi2d2"

name := """body-volume-application"""
version       := "0.1"

scalaVersion  := "2.12.6"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers += Resolver.bintrayRepo("unibas-gravis", "maven")

resolvers += Opts.resolver.sonatypeSnapshots

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")

libraryDependencies  ++= Seq(
            "ch.unibas.cs.gravis" %% "scalismo" % "0.17.+",
            "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.+",
            "ch.unibas.cs.gravis" %% "scalismo-ui" % "0.13.0",
            "ch.unibas.cs.gravis" %% "scalismo-faces" % "0.5.0"
)

libraryDependencies in ThisBuild ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.pegdown" % "pegdown" % "1.4.2" % "test"      // (For Html Scalatest reports)
)

libraryDependencies += "com.opencsv" % "opencsv" % "4.5"

assemblyJarName in assembly := "executable.jar"

mainClass in assembly := Some("com.example.ExampleApp")

assemblyMergeStrategy in assembly <<= (assemblyMergeStrategy in assembly) { old =>
  {
    case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
    case PathList("META-INF", s) if s.endsWith(".SF") || s.endsWith(".DSA") || s.endsWith(".RSA") => MergeStrategy.discard
    case "reference.conf" => MergeStrategy.concat
    case _ => MergeStrategy.first
  }
}
