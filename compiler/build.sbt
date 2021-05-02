name := "framework"

version := "0.1"

scalaVersion := "2.12.8"
val sparkVersion = "2.4.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
// https://mvnrepository.com/artifact/org.apache.spark/spark-mllib
//libraryDependencies += "org.apache.spark" %% "spark-mllib" % sparkVersion
libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.3"
libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.2"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.2"

testOptions += Tests.Argument("-oF")
