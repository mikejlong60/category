lazy val root = (project in file(".")).
  settings(
    name := "category",
    version := "1.0",
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test", "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1")

  )
