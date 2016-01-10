import sbt._, Keys._

object VindiniumBot extends Build {

  lazy val bot = Project(
    id = "vindinium-bot",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      organization := "org.jousse",
      version := "0.1",
      scalaVersion := "2.11.7",
      resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/",
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json" % "2.3.7",
        "org.scalaj" %% "scalaj-http" % "0.3.16"
      ),
      scalacOptions ++= Seq("-language:_", "-deprecation", "-unchecked"))
  ).settings(com.github.retronym.SbtOneJar.oneJarSettings: _*)
}
