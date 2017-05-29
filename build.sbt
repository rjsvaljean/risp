name := "risp"

scalaVersion := "2.12.2"

val circeVersion = "0.7.0"
val scalaTestVersion = "3.0.1"

val circeDeps = {

  Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)
}

libraryDependencies ++= Seq(
  "com.slamdata" %% "matryoshka-core" % "0.18.3",
  "com.lihaoyi" %% "fastparse" % "0.4.2",
  "org.scalactic" %% "scalactic" % scalaTestVersion % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "co.fs2" %% "fs2-core" % "0.9.6",
  "co.fs2" %% "fs2-io" % "0.9.6"
)

scalacOptions += "-Ypartial-unification"
