name := "scalangj"

version := "0.1.5"

scalaVersion := "3.3.1"

sbtVersion in Global := "1.8.2"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "https://mvnrepository.com/artifact/"

resolvers += "clojars" at "https://clojars.org/repo"


libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.4.2"

// for publishing to github

organization := "obsidian.lang.java"

publishMavenStyle := true


publishTo := Some(Resolver.file("mavenLocal",  new File(Path.userHome.absolutePath+"/obsidian-java/binrepo/")))


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

// the following code is to enable Scala.js
import org.scalajs.linker.interface.ModuleSplitStyle

lazy val livechart = project.in(file("."))
  .enablePlugins(ScalaJSPlugin) // Enable the Scala.js plugin in this project
  .settings(
    scalaVersion := "3.2.2",

    // Tell Scala.js that this is an application with a main method
    scalaJSUseMainModuleInitializer := true,

    /* Configure Scala.js to emit modules in the optimal way to
     * connect to Vite's incremental reload.
     * - emit ECMAScript modules
     * - emit as many small modules as possible for classes in the "livechart" package
     * - emit as few (large) modules as possible for all other classes
     *   (in particular, for the standard library)
     */
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("livechart")))
    },

    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
  )