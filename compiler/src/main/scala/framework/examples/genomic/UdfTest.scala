package framework.examples.genomic

import java.util.Base64
import framework.common._
import framework.examples.Query
import framework.nrc.Parser
// test

// this is just the example from the 
object ExampleQuery extends DriverGene {
  
  // this will be reviewed next
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String = 
    if (shred){
      s""// TODO""
    }else{
      s"""|val sloader = new BiospecLoader(spark)
          |val samples = sloader.load("/mnt/app_hdd/data/biospecimen/aliquot/nationwidechildrens.org_biospecimen_aliquot_dlbc.txt")
          |val cloader = new CopyNumberLoader(spark)
          |val copynumber = cloader.load("/mnt/app_hdd/data/cnv", true)
          |
          |val geLoader = new GeneExpressionLoader(spark)
          |val expression = geLoader.load("/mnt/app_hdd/data/expression", true, aliquot_file = "/mnt/app_hdd/data/fpkm_uq_case_aliquot.txt")
          |
          |val occurrences = spark.read.json("/mnt/app_hdd/data/somatic/datasetPRAD")
          |val pathways =
          |""".stripMargin
    }
  
  // name to identify your query
  val name = "ExampleQuery"
  val genetype = TupleType("name" -> StringType)
  val pathtype = TupleType("p_name" -> StringType, "url" -> StringType, "gene_set" -> BagType(genetype))

  // a map of input types for the parser
    val tbls = Map("occurrences" -> occurmids.tp,
                    "copynumber" -> copynum.tp,
                    "samples" -> samples.tp,
                    "pathways" -> BagType(pathtype))


  // a query string that is passed to the parser
  // note that a list of assignments should be separated with ";"
  val query = 
    s"""
        GMB <=
          for p in pathways union
            {(pathway := p.p_name, burdens :=
              (for o in occurrences union
                for t in o.transcript_consequences union
                  for g in p.gene_set union
                    if (g.name = t.gene_id) then
                      {(sid := o.donorId, burden := if (t.impact = "HIGH") then 0.80
                                                else if (t.impact = "MODERATE") then 0.50
                                                else if (t.impact = "LOW") then 0.30
                                                else 0.01)}).sumBy({sid}, {burden}))}

    """

    // finally define the parser, note that it takes the input types
    // map as input and pass the query string to the parser to
    // generate the program.
    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]

}
