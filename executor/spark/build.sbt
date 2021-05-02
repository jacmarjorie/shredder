name := "sparkutils"

version := "0.1"

scalaVersion := "2.12.8"
val sparkVersion = "3.0.1"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.apache.spark" %% "spark-core" % sparkVersion
libraryDependencies += "org.apache.spark" %% "spark-sql" % sparkVersion
libraryDependencies += "org.apache.spark" %% "spark-mllib" % sparkVersion
libraryDependencies += "com.holdenkarau" %% "spark-testing-base" % "2.4.2_0.12.0" % Test
//libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.1"
libraryDependencies += "com.github.samtools" % "htsjdk" % "2.9.1"
libraryDependencies += "org.seqdoop" % "hadoop-bam" % "7.8.0"
