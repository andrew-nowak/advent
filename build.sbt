ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation")
ThisBuild / fork := true
ThisBuild / javaOptions ++= Seq("-ea")

lazy val lib = (project in file("lib")).settings(
  scalaVersion := "2.13.12"
)
lazy val y2019 = (project in file("2019")).settings(
  scalaVersion := "2.13.12"
).dependsOn(lib)
lazy val y2021 = (project in file("2021")).settings(
  scalaVersion := "2.13.12",
  libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.4"
).dependsOn(lib)
lazy val y2022 = (project in file("2022")).settings(
  scalaVersion := "2.13.12"
).dependsOn(lib)

val `5` = "hi"
