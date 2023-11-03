enablePlugins(ScalaJSPlugin)

name := "Conway's Game of Life - Scala.js"
scalaVersion := "3.3.1" // or any other Scala version >= 2.11.12

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0"