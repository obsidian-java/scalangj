name := "scalangj"
version := "0.1.8"

Global / sbtVersion := "1.8.2"

ThisBuild / resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
ThisBuild / resolvers += "Maven Repository" at "https://mvnrepository.com/artifact/"
ThisBuild / resolvers += "clojars" at "https://clojars.org/repo"

// for publishing to github
ThisBuild / organization := "obsidian.lang.java"
ThisBuild / publishMavenStyle := true

publishTo in ThisBuild := Some(Resolver.file("mavenLocal",  new File(Path.userHome.absolutePath+"/obsidian-java/binrepo/")))

//publishArtifact in Test := false
Test / publishArtifact := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/obsidian-java/scalangj</url>
  <licenses>
    <license>
      <name>Apache License 2.0</name>
      <url>https://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:obsidian-java/scalangj.git</url>
    <connection>scm:git:git@github.com:obsidian-java/scalangj.git</connection>
  </scm>
  <developers>
    <developer>
      <id>luzhuomi</id>
      <name>Kenny Zhuo Ming Lu</name>
      <url>http://sites.google.com/site/luzhuomi</url>
    </developer>
    <developer>
      <id>Chingles2404</id>
      <name>CCH</name>
      <url></url>
    </developer>
  </developers>)

ThisBuild / scalaVersion := "3.3.1"

lazy val root = project.in(file(".")).
  aggregate(scalangj.js, scalangj.jvm).
  settings(
    publish := {
    },
    publishLocal := {},
  )

lazy val scalangj = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "scalangj",
    version := "0.1.8",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
      "org.scalactic" %%% "scalactic" % "3.2.9",
      "org.scalatest" %%% "scalatest" % "3.2.9" % "test",
      "org.typelevel" %%% "paiges-core" % "0.4.2"
    )
  ).
  jsSettings(
    scalaJSUseMainModuleInitializer := true
  )