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

//libraryDependencies += "org.apache.commons" % "commons-text" % "1.9"

libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.4.2"

// scalacOptions ++= Seq("-feature", "-deprecation", "-Yresolve-term-conflict:package")

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

/*
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
  ) */

import scala.sys.process._

lazy val installDependencies = Def.task[Unit] {
  val base = (ThisProject / baseDirectory).value
  val log = (ThisProject / streams).value.log
  if (!(base / "node_module").exists) {
    val pb =
      new java.lang.ProcessBuilder("npm", "install")
        .directory(base)
        .redirectErrorStream(true)

    pb ! log
  }
}

lazy val open = taskKey[Unit]("open vscode")
def openVSCodeTask: Def.Initialize[Task[Unit]] =
  Def
    .task[Unit] {
      val base = (ThisProject / baseDirectory).value
      val log = (ThisProject / streams).value.log

      val path = base.getCanonicalPath
      s"code --extensionDevelopmentPath=$path" ! log
      ()
    }
    .dependsOn(installDependencies)

lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.3.1",
    moduleName := "vscode-scalajs-hello",
    Compile / fastOptJS / artifactPath := baseDirectory.value / "out" / "extension.js",
    Compile / fullOptJS / artifactPath := baseDirectory.value / "out" / "extension.js",
    open := openVSCodeTask.dependsOn(Compile / fastOptJS).value,
    libraryDependencies ++= Seq(
      // ScalablyTyped.V.vscode,
      "com.lihaoyi" %%% "utest" % "0.8.2" % "test"
    ),
    Compile / npmDependencies ++= Seq("@types/vscode" -> "1.84.1"),
    testFrameworks += new TestFramework("utest.runner.Framework")
    // publishMarketplace := publishMarketplaceTask.dependsOn(fullOptJS in Compile).value
  )
  .enablePlugins(
    ScalaJSPlugin,
    ScalaJSBundlerPlugin,
    ScalablyTypedConverterPlugin
  )