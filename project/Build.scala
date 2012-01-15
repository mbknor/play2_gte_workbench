import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "p2test3"
    val appVersion      = "1.0"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "kjetland" % "gt-engine_2.9.1" % "0.1.4" changing()

    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = JAVA).settings(
      // Add your own project settings here
      resolvers ++= Seq(Resolver.url("My local ivy repo", url("file:///Users/mortenkjetland/.ivy2/local/"))(Resolver.ivyStylePatterns))
    )

}
