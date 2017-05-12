import sbt.Keys.scalacOptions

val scalazVersion = "7.1.13"

scalacOptions += "-feature"

scalacOptions += "-Xprint:typer"

initialCommands in console := "import scalaz._, Scalaz._, scala.language.higherKinds"

initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._"

lazy val root = (project in file(".")).
  settings(
    name := "category",
    version := "1.0",
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
      "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test",
      "com.typesafe" % "config" % "1.3.1" % "test",
      "org.scalaz" %% "scalaz-core" % scalazVersion,
      "org.scalaz" %% "scalaz-effect" % scalazVersion,
      "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
      "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
    )
  )
