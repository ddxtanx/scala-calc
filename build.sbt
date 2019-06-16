name := "MonadTest"
scalaVersion := "2.13.0-RC3"
version := "0.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0-M3"
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")
//scalacOptions += "-Ypartial-unification"

