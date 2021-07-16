name := "applicative-functor-part-2-scala"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-Xsource:2.14"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"
