scalaVersion := "2.11.12"

// Set to false or remove if you want to show stubs as linking errors
nativeLinkStubs := true

enablePlugins(ScalaNativePlugin)

// https://mvnrepository.com/artifact/org.scala-native/test-interface
libraryDependencies += "org.scala-native" % "test-interface_native0.3_2.11" % "0.3.8" % Test

