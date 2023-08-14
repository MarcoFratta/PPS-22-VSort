import org.scalajs.linker.interface.ModuleSplitStyle


lazy val copyJsTask = TaskKey[Unit]("copyJsTask", "Copy javascript files to target directory")

addCommandAlias("jsCompile", ";fastOptJS;copyJsTask")

lazy val vsort = project.in(file("."))
  .enablePlugins(ScalaJSPlugin) // Enable the Scala.js plugin in this project
  .settings(
    scalaVersion := "3.2.2",
    copyJsTask := {
      val outDir = baseDirectory.value
      val inDir = baseDirectory.value / ("target/scala-" + scalaVersion.value) / "vsort-fastopt"
      val files = Seq("main.js", "main.js.map") map { p => (inDir / p, outDir / p) }
      IO.copy(sources = files, overwrite = true, preserveLastModified = false, preserveExecutable = false)
    },
    //webpackBundlingMode := BundlingMode.LibraryAndApplication() and then a fastOptJS::webpack,
    // Tell Scala.js that this is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    //scalaJSLinkerOutputDirectory := baseDirectory.value / "js"

    /* Configure Scala.js to emit modules in the optimal way to
     * connect to Vite's incremental reload.
     * - emit ECMAScript modules
     * - emit as many small modules as possible for classes in the  package
     * - emit as few (large) modules as possible for all other classes
     *   (in particular, for the standard library)
     */
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("vsort")))
    },

    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */

    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",

    // Depend on Laminar
    libraryDependencies += "com.raquo" %%% "laminar" % "15.0.1",

    // Testing framework
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    name := "VSort",

    libraryDependencies += "ai.dragonfly" %%% "vector" % "0.101",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test

  )


