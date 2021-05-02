### Example Queries

Each query directory contains a set of input relations in various formats with their respective types, 
as well as flat and nested queries that use these relations. 

* `tpch`: TPC-H queries from the benchmark. Since TPC-H loads data from a CSV file, there are also TPC-H loader specifics in this directory.
* `simple`: simple example queries used for testing
* `normalize`: queries that trigger normalization rules
* `optimize`: queries specific to exploring the domain-based optimization
* `genomic`: Biomedical benchmark queries and some basic GWAS example queries

### Writing a query

NRC queries are described natively in Scala using the NRC language defined in `src/main/scala/framework/nrc/NRC.scala`. 
A newly defined query should extend the Query trait (see Query.scala) to leverage various support functions for executing the 
stages of the pipeline. 

A Query (defined in `Query.scala`) is a trait that extends the components necessary 
to execute the pipeline for code generation. When writing a query, we will create 
an object that extends this. 

If your plan is to write a set of queries based on a ceratin set of inputs, then 
it will be best to create a base trait for your query. This is what `trait TPCHBase` 
is for queries in `examples/tpch/`. This base trait should contain the schema for 
all the inputs that will be used by TPCH queries. 

If we wanted to make one of these for genomic data, we need to reference the 
loading functions we have defined for our input, describe the type of the input, 
and then make a set of variables that we will use as reference in our queries. 
See `src/main/scala/common/Types.scala` for more NRC types. 

Trance comes with a prepacked set of types for genomic data sources, most of which 
are defined in `genomic/DriverGenes.scala` and `genomic/Annotations.scala`. See how to define your 
own type and use in a query in the `Example: Defining a query from scratch` tutorial below.

#### Example: Defining a query using parser (recommended)

The recommended way to define a query is to use the NRC parser functionality. 
`genomic/Sharing.scala` has several examples of this. Let us focus in on one: 
[ExampleQuery](https://github.com/jacmarjorie/trance/blob/sharing/compiler/src/main/scala/framework/examples/genomic/UdfTest.scala)

```
import framework.common._
import framework.examples.Query
import framework.nrc.Parser

object ExampleQuery extends DriverGene {
  
  // see file for details
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String = ...
  
  // name to identify your query
  val name = "ExampleQuery"
  
  // a map of input types for the parser
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

  // a query string that is passed to the parser
  // note that a list of assignments should be separated with ";"
  val query = 
    s"""
      cnvCases1 <= 
        for s in samples union 
          for c in copynumber union 
            if (s.bcr_aliquot_uuid = c.cn_aliquot_uuid)
            then {(sid := s.bcr_patient_uuid, gene := c.cn_gene_id, cnum := c.cn_copy_number)};

      hybridScore1 <= 
          for o in occurrences union
            {( oid := o.oid, sid1 := o.donorId, cands1 := 
              ( for t in o.transcript_consequences union
                 if (t.sift_score > 0.0)
                 then for c in cnvCases1 union
                    if (t.gene_id = c.gene && o.donorId = c.sid) then
                      {( gene1 := t.gene_id, score1 := (c.cnum + 0.01) * if (t.impact = "HIGH") then 0.80 
                          else if (t.impact = "MODERATE") then 0.50
                          else if (t.impact = "LOW") then 0.30
                          else 0.01 )}).sumBy({gene1}, {score1}) )}
    """

    // finally define the parser, note that it takes the input types 
    // map as input and pass the query string to the parser to 
    // generate the program.
    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]

}
```

Now you can follow the 'Code Generation` section below to compile and run a Spark application.


#### Example: Defining a query from scratch


First, make a file in src/main/scala/examples/Genomic.scala. In this file, 
define a trait that will be the base for our queries:

```
package framework.examples

import framework.common._
import framework.examples.Query

trait GenomicSchema extends Query {

  // this overrides a function that was previously used for tpch benchmarking
  // just define how to load your inputs here
  // the string below handles the case for non-skewed, non-shredded inputs
  def loadTables(shred: Boolean = false, skew: Boolean = false): String = {
      s"""|val basepath = "src/main/scala/Data/"
          |val path = basepath + "Variants/sub_chr22.vcf"
          |val variants = loadVCF(path, spark)
          |""".stripMargin
  }

  // define the types, which would reflect the case classes from your variant loader 
  val genoType = TupleType("sample" -> StringType, "call" -> IntType)
  val variantType = TupleType(
    "contig" -> StringType, 
    "start" -> IntType, 
    "reference" -> StringType, 
    "alternate" -> StringType, 
    genotypes -> BagType(genoType))

  // define references to these types
  val variants = BagVarRef("Variants", BagType(variantsType))
  val vr = TupleVarRef("v", variantType)
  val gr = TupleVarRef("g", genoType)

}
```

Now that we have a base type, we can write a query over variant information. 
In the same file, we will define an object to write out the query:

```
object GenomicQuery1 extends GenomicSchema {
  
  val name = "GenomicQuery1"

  // this query just iterates over the variant set
  val query = 
    ForeachUnion(vr, variants,                          // For v in Variants
      Singleton(Tuple("contig" -> vr("contig"),          //   {( contig := v.contig,  
        "start" -> vr("start"),                         //      start := v.start, 
        "reference" -> vr("reference"),                 //       reference := v.reference,
        "alternate" -> vr("alternate"),                 //       alternate := v.alternate,
        "genotypes" ->                                  //       genotypes := 
          ForeachUnion(gr, BagProject(vr, "genotypes"), //        For g in v.genotypes union
            Singleton(Tuple("sample" -> gr("sample"),   //          {(sample := g.sample,
                            "call" -> gr("call")))      //            call := g.call )}
        ))))                                            //    )}

  // finally define the query as the input to your NRC program
  val program = Program(Assignment(name, query))

}
```

Now you have written a query. You can extend the schema to use with other inputs, and 
you can define new query objects to write out more queries. 

The next step is to write and run an application that will generate the code for the query. 

#### Code Generation

For now code generation is setup to use helper functions specific to benchmarking. See `src/main/scala/generator/spark/App.scala` for examples of how the benchmark experiments were created. Here we will define our own application to call the queries defined in the above section. 

In `src/main/scala/generator/spark/` make a file called UdfTestApp.scala. This will be the application you use to generate code from the above. Write the following in the application file:

```
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
```

Now, run `sbt run` from the `compiler` folder. This should give you four application numbers. Select the application number specific to `framework.generator.spark.UdfTestApp`. You should see the NRC written out to the console and the corresponding plan underneath.

### Execution

After you have completed the above, the code will be written to `shredder/executor/spark/src/main/scala/sparkutils/generated/`. cd to `shredder/exectuor/spark`, compile the jar like in the example (`sbt package`). And run the jar specifying your query: 

```
spark-submit --class sparkutils.generated.ExampleQuery \
  --master "local[*]" target/scala-2.12/sparkutils_2.12-0.1.jar
```
