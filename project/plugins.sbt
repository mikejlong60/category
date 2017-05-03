resolvers += Resolver.url(
  "bintray-dnvriend-ivy-sbt-plugins",
  url("http://dl.bintray.com/dnvriend/sbt-plugins"))(
  Resolver.ivyStylePatterns)

//addSbtPlugin("com.github.dnvriend" % "sbt-haskell" % "0.0.8")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")
