
name := "saga"

version := "0.1"

scalaVersion := "2.12.6"

val catsVersion = "1.1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-macros" % catsVersion,
  "org.typelevel" %% "cats-kernel" % catsVersion,
  "org.typelevel" %% "cats-free" % catsVersion,
//  "org.typelevel" %% "cats-mtl-core" % "0.2.1",
  "org.scalatest" %% "scalatest" % "3.0.1",
  "org.mockito" % "mockito-all" % "1.10.19"
)

scalacOptions ++= Seq("-Xfatal-warnings","-feature","-language:higherKinds", "-language:implicitConversions" , "-language:postfixOps", "-deprecation")
