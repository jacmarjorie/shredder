package framework.generator.spark

import framework.examples._
import framework.examples.genomic._

object UdfTestApp extends App {

  override def main(args: Array[String]){

    // runs the standard pipeline
    AppWriter.runDataset(ExampleQuery, "ExampleTest,standard", optLevel = 1)

    // runs the shredded pipeline
    AppWriter.runDatasetShred(ExampleQuery, "ExampleTest,standard", optLevel = 1)
  }
}